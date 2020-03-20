createBuildingDistribution <- function(nBuildings, lambda_p, z_mean, z_sd, DART_XorY_m, DARTbuildSizeXY, XYoffset_factor, 
                                       maxBuildRotation, seedVal = floor(runif(1, min = 1, max = 100)), maxIters = 500) {
  
  #predefine model domain
  nBuildingsXorY <- ceiling(sqrt(nBuildings))
  buildingDistance0 <- seq(0, DART_XorY_m, length.out = nBuildingsXorY)
  buildingDistance <- buildingDistance0[-1] - (diff(buildingDistance0) / 2)
  singleBuildingDistance <- diff(buildingDistance)[1]
  if (is.na(singleBuildingDistance)) stop("Add more buildings")
  totalArea <- DART_XorY_m^2
  singleBuildingArea <- (lambda_p * totalArea) / (length(buildingDistance)^2)
  singleBuildingLength <- sqrt(singleBuildingArea)
  x <- y <- buildingDistance
  #predefine overlapping plot flag
  overlappingPolys <- TRUE
  nIters <- 0
  while (overlappingPolys) {
    
    seedVal <- seedVal + 1
    set.seed(seed = seedVal)
    #initiate data frame with building centroid x,y locations
    outDF <- expand.grid(x = x, y = y)
    #randomly add x,y coordinate shifts
    xJitter <- runif(n = nrow(outDF), min = -(singleBuildingDistance * XYoffset_factor) / 2, 
                     max = (singleBuildingDistance * XYoffset_factor) / 2)
    yJitter <- runif(n = nrow(outDF), min = -(singleBuildingDistance * XYoffset_factor) / 2, 
                     max = (singleBuildingDistance * XYoffset_factor) / 2)
    outDF$x <- outDF$x + xJitter
    outDF$y <- outDF$y + yJitter
    
    #add dart bits to columns
    outDF$objInd <- 0
    outDF$z <- 0
    
    #set DART building X, Y size
    dartBuildXYsize <- (DARTbuildSizeXY * singleBuildingLength) / 2
    outDF$Xscale <- outDF$Yscale <- dartBuildXYsize
    #set DART building heights (norm distribution)
    outDF$Zscale <- rnorm(n = nrow(outDF), mean = z_mean, sd = z_sd)
    #set DART x and y rotation
    outDF$Xrot <- outDF$Yrot <- 0
    #set random rotation north +- 45 deg
    rotVals_raw <- runif(n = nrow(outDF), min = -maxBuildRotation, max = maxBuildRotation)
    rotVals_raw[rotVals_raw < 0] <- 360 + rotVals_raw[rotVals_raw < 0]
    outDF$Zrot <- rotVals_raw
    #finalise DART data frame
    outDF <- outDF %>%
      dplyr::select(objInd, x, y, z, Xscale, Yscale, Zscale, Xrot, Yrot, Zrot)
    
    #create polygons
    #*0.5 for gBuffer
    buffWidth <- (singleBuildingLength / 2)
    outDF_DARTcompatible <- outDF %>%
      dplyr::mutate(xNew = y, 
                    yNew = max(x) - x,
                    x = xNew, 
                    y = yNew,
                    Zrot = 360 - Zrot) %>%
      dplyr::select(-c(xNew, yNew))
    SPoints <- sp::SpatialPoints(outDF_DARTcompatible[c("x", "y")])
    spList <- list()
    for (i in seq_along(SPoints)) {
      spList[[i]] <- gBuffer(SPoints[i], width = buffWidth, quadsegs = 1, capStyle = "SQUARE")
      spList[[i]] <- maptools::elide(spList[[i]], rotate = outDF_DARTcompatible$Zrot[i],
                                     center = coordinates(SPoints[i]))
    }
    
    SP <- do.call(bind, spList)
    SPbbox <- bbox(SP)
    
    LHS <- raster::shift(SP, dx = -DART_XorY_m, dy = 0)
    topHS <- raster::shift(SP, dx = 0, dy = DART_XorY_m)
    
    SP_cycled <- do.call(bind, list(SP, LHS, topHS))
    area_preAgg <- as.numeric(gArea(SP_cycled))
    area_postAgg <- as.numeric(gArea(aggregate(SP_cycled)))
    if ((area_preAgg - area_postAgg) < 1e-5) overlappingPolys <- FALSE
    nIters <- nIters + 1
    if (nIters > maxIters) {
      warning(paste0("Maximum number of iterations reached (", maxIters, ")"))
      return(NULL)
    }
  }
  
  SPbbox <- bbox(SP)
  SP_shifted <- raster::shift(SP, dx = -SPbbox["x", "min"], dy = -SPbbox["y", "min"])
  SP_shifted <- SpatialPolygonsDataFrame(Sr = SP_shifted, data = data.frame("z" = outDF$Zscale))
  outDF_shifted <- outDF %>%
    dplyr::mutate(x = x + (SPbbox["y", "min"] * 0.5),
                  y = y - SPbbox["x", "min"])
  out <- list()
  out$polygons <- SP_shifted
  out$df <- outDF_shifted
  out$nIters <- nIters
  out$seed <- seedVal
  
  return(out)
}