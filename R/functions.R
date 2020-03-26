createBuildingDistribution <- function(nBuildings, 
                                       lambda_p,
                                       z_mean,
                                       z_sd, 
                                       DART_XorY_m, 
                                       DARTbuildSizeXYZ, 
                                       XYoffset_factor, 
                                       maxBuildRotation,
                                       seedVal = floor(runif(1, min = 1, max = 100)), 
                                       maxIters = 250) {
  
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
    #initiate data frame with building centroid x,y locations
    outDF <- expand.grid(x = x, y = y)
    #randomly add x,y coordinate shifts
    set.seed(seed = seedVal)
    xJitter <- runif(n = nrow(outDF), min = -(singleBuildingDistance * XYoffset_factor) / 2, 
                     max = (singleBuildingDistance * XYoffset_factor) / 2)
    set.seed(seed = seedVal)
    yJitter <- runif(n = nrow(outDF), min = -(singleBuildingDistance * XYoffset_factor) / 2, 
                     max = (singleBuildingDistance * XYoffset_factor) / 2)
    outDF$x <- outDF$x + xJitter
    outDF$y <- outDF$y + yJitter
    
    #add dart bits to columns
    outDF$objInd <- 0
    outDF$z <- 0
    #set DART building X, Y size
    outDF$Xscale <- outDF$Yscale <- singleBuildingLength / DARTbuildSizeXYZ
    #set DART building heights (norm distribution)
    set.seed(seed = seedVal)
    outDF$Zscale <- rnorm(n = nrow(outDF), mean = z_mean, sd = z_sd) / DARTbuildSizeXYZ
    #set DART x and y rotation
    outDF$Xrot <- outDF$Yrot <- 0
    #set random rotation north +- 45 deg
    set.seed(seed = seedVal)
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
      dplyr::mutate(xNew = max(y) - y, 
                    yNew = x,
                    x = xNew, 
                    y = yNew,
                    Zrot = 360 - Zrot) %>%
      dplyr::select(-c(xNew, yNew))
    SPoints <- sp::SpatialPoints(outDF[c("x", "y")])
    spList <- list()
    for (i in seq_along(SPoints)) {
      spList[[i]] <- gBuffer(SPoints[i], width = buffWidth, quadsegs = 1, capStyle = "SQUARE")
      spList[[i]] <- maptools::elide(spList[[i]], rotate = outDF$Zrot[i],
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
  
  #remove empty space atedge of domain
  SPbbox <- bbox(SP)
  SP_shifted <- raster::shift(SP, dx = -SPbbox["x", "min"], dy = -SPbbox["y", "min"])
  SP_shifted <- SpatialPolygonsDataFrame(Sr = SP_shifted, data = data.frame("z" = outDF$Zscale * DARTbuildSizeXYZ))
  
  #put all in output list
  out <- list()
  out$polygons <- SP_shifted
  out$df <- outDF_DARTcompatible
  out$nIters <- nIters
  out$seed <- seedVal
  out$domainExtent <- bbox(SP_shifted)
  out$newPAI <- sum(area(SP_shifted)) / (out$domainExtent["x", "max"] * out$domainExtent["y", "max"])
  paramsList <- createParamsList(nBuildings = nBuildings, 
                                 lambda_p = lambda_p,
                                 z_mean = z_mean,
                                 z_sd = z_sd, 
                                 DART_XorY_m = DART_XorY_m, 
                                 DARTbuildSizeXYZ = DARTbuildSizeXYZ, 
                                 XYoffset_factor = XYoffset_factor, 
                                 maxBuildRotation = maxBuildRotation,
                                 seedVal = seedVal, 
                                 maxIters = maxIters)
  out$params <- paramsList
  return(out)
}

createParamsList <- function(nBuildings, 
                             lambda_p,
                             z_mean,
                             z_sd, 
                             DART_XorY_m, 
                             DARTbuildSizeXYZ, 
                             XYoffset_factor, 
                             maxBuildRotation,
                             seedVal = floor(runif(1, min = 1, max = 100)), 
                             maxIters = 250) {
  
  out <- list()
  out$nBuildings <- nBuildings
  out$lambda_p <- lambda_p
  out$z_mean <- z_mean
  out$z_sd <- z_sd
  out$DART_XorY_m <- DART_XorY_m
  out$DARTbuildSizeXYZ <- DARTbuildSizeXYZ
  out$XYoffset_factor <- XYoffset_factor
  out$maxBuildRotation <- maxBuildRotation
  out$seedVal <- seedVal
  out$maxIters <- maxIters
  
  return(out)
}

buildingsFileID_Namestr <- function(fIDstr) {
  paste0("rBldgsParams_", fIDstr, sep = "")
}

buildingsFileID <- function(fID = NULL) {
  
  if (!is.null(fID)) return(list("ID" = fID, "fName" = buildingsFileID_Namestr(fID)))
  #making file ID
  library(stringi)
  set.seed(runif(1))
  randomID <- stri_rand_strings(1, 5, pattern = "[A-Z0-9]")
  out <- list()
  out$ID <- randomID
  out$fName <- buildingsFileID_Namestr(randomID)
  
  return(out)
  
}

writeParamsYaml <- function(buildDistribution, oDir, fID) {
  
  library(yaml)
  oFile <- file.path(oDir, paste0(fID$fName, ".yml"))
  if (file.exists(oFile)) stop(paste(oFile, "exists."))
  oList <- list(buildDistribution$params)
  names(oList) <- fID$ID
  yaml::write_yaml(oList, oFile)
}


writeShp <- function(buildDistribution, oDir, fID) {
  
  oDir_shp <- file.path(oDir, fID$ID)
  
  writeOGR(buildDistribution$polygons, dsn = oDir_shp, 
           driver = "ESRI Shapefile", layer = "z", 
           overwrite_layer = TRUE)
  
  
}

writeDARTdf <- function(buildDistribution, oDir, fID) {
  
  oDir_df <- file.path(oDir, fID$ID)
  oFile_df <- file.path(oDir_df, paste0("DART_fields_", fID$ID, ".txt", sep = ""))
  writeLines(text = "complete transformation", 
             con = oFile_df)
  write.table(x = buildDistribution$df, 
              file = oFile_df, 
              sep = " ", 
              col.names = FALSE, 
              row.names = FALSE, 
              append = TRUE)
}

writebuildDistribution <- function(buildDistribution, oDir, fID = NULL) {
  fID <- buildingsFileID(fID = fID) 
  oDir_full <- file.path(oDir, fID$ID)
  dir.create(oDir_full)
  writeParamsYaml(buildDistribution, oDir_full, fID)
  writeShp(buildDistribution, oDir, fID)
  writeDARTdf(buildDistribution, oDir, fID)
  return(oDir_full)
  
}
