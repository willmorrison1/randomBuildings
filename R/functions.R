createBuildingDistribution <- function(nBuildings, 
                                       lambda_p,
                                       z_mid,
                                       z_variability, 
                                       DART_XorY_m, 
                                       DARTbuildSizeXYZ, 
                                       XYoffset_factor, 
                                       maxBuildRotation,
                                       seedVal = floor(runif(1, min = 1, max = 100)), 
                                       maxIters = 250, 
                                       forcePolygonSeparation = TRUE,
                                      height_distribution_normal = TRUE) {
  require(dplyr)
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
    if (height_distribution_normal) {
    outDF$Zscale <- rnorm(n = nrow(outDF), mean = z_mid, sd = z_variability) / DARTbuildSizeXYZ
      } else {
      outDF$Zscale <- runif(n = nrow(outDF), min = z_mid - (z_variability / 2),  max = z_mid + (z_variability / 2)) / DARTbuildSizeXYZ
      }
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
    if ((area_preAgg - area_postAgg) < 1e-5 | !forcePolygonSeparation) overlappingPolys <- FALSE
    if (nIters > maxIters) {
      warning(paste0("Maximum number of iterations reached (", maxIters, ")"))
      return(NULL)
    }
    seedVal <- seedVal + 1
    nIters <- nIters + 1
  }
  polyDFcentroids <- data.frame(rgeos::gCentroid(SP, byid = TRUE))
  SP <- gIntersection(SP, as(extent(SP) - (singleBuildingDistance * 0.75), "SpatialPolygons"), byid = TRUE)
  if (length(SP) != nrow(polyDFcentroids)) 
    stop("You have lost buildings out of the edge of the domain please make XYoffset_factor smaller.")
  #remove empty space atedge of domain
  SPbbox <- bbox(SP)
  SP_shifted <- raster::shift(SP, dx = -SPbbox["x", "min"], dy = -SPbbox["y", "min"])
  polyDFcentroids$x <- polyDFcentroids$x - SPbbox["x", "min"]
  polyDFcentroids$y <- polyDFcentroids$y - SPbbox["y", "min"]
  SP_shifted <- SpatialPolygonsDataFrame(Sr = SP_shifted, 
                                         data = data.frame("z" = outDF$Zscale * DARTbuildSizeXYZ))
  #put all in output list
  out <- list()
  out$polygons <- SP_shifted
  domainExtentVals <- bbox(SP_shifted)
  outDF_DARTcompatible <- outDF_DARTcompatible %>%
    dplyr::mutate(y = y - SPbbox[,"min"]["x"],
                  x = x + min(domainExtentVals[,"max"]["y"] - polyDFcentroids$y))
  out$df <- outDF_DARTcompatible
  paramsList <- createParamsList(polygonsData = SP_shifted, 
                                 nBuildings = nBuildings, 
                                 lambda_p = lambda_p,
                                 z_mid = z_mid,
                                 z_variability = z_variability,
                                 height_distribution_normal = height_distribution_normal,
                                 DART_XorY_m = DART_XorY_m, 
                                 DARTbuildSizeXYZ = DARTbuildSizeXYZ, 
                                 XYoffset_factor = XYoffset_factor, 
                                 maxBuildRotation = maxBuildRotation,
                                 seedVal = seedVal - 1, 
                                 iters = nIters,
                                 maxIters = maxIters, 
                                 domainExtent = domainExtentVals)
  out$shiftAmount <- SPbbox
  out$params <- paramsList
  out$polygonCentroids <- polyDFcentroids
  return(out)
}

createParamsList <- function(polygonsData,
                             nBuildings, 
                             lambda_p,
                             z_mid,
                             z_variability, 
                             DART_XorY_m, 
                             DARTbuildSizeXYZ, 
                             XYoffset_factor, 
                             maxBuildRotation,
                             seedVal, 
                             iters,
                             maxIters,
                             domainExtent,
                            height_distribution_normal) {
  
  out <- list()
  out$nBuildings <- nBuildings
  out$lambda_p <- lambda_p
  out$z_mid <- z_mid
  out$z_variability <- z_variability
  out$DART_XorY_m <- DART_XorY_m
  out$DARTbuildSizeXYZ <- DARTbuildSizeXYZ
  out$XYoffset_factor <- XYoffset_factor
  out$maxBuildRotation <- maxBuildRotation
  out$seedVal <- seedVal
  out$iters <- iters
  out$maxIters <- maxIters
  out$domainExtent <- domainExtent
  out$height_distribution_normal <- height_distribution_normal
  out$newPAI <- sum(area(polygonsData)) / (domainExtent["x", "max"] * domainExtent["y", "max"])
  return(out)
}

buildingsFileID_Namestr <- function(fIDstr) {
  paste0("rBldgsParams_", fIDstr, sep = "")
}

buildingsFileID <- function(fID = NULL) {
  if (!is.null(fID)) return(list("ID" = fID, "fName" = buildingsFileID_Namestr(fID)))
  #making file ID
  library(stringi)
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
  if (!dir.exists(oDir_df)) dir.create(oDir_df)
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

expBuildToDARTfield <- function(inFileFull, outDir = dirname(inFileFull)) {
  baseDir <- dirname(inFileFull)
  fID_raw <- sapply(file_path_sans_ext(basename(inFileFull)), function(x) strsplit(x, "_")[[1]][1])
  fID <- buildingsFileID(fID = fID_raw)
  inDat <- read.table(inFileFull, header = TRUE)
  inMetaFileFull <- file.path(baseDir, paste0(fID$ID, "_parameters.txt"))
  inMeta <- read.table(inMetaFileFull, header = TRUE)
  outDat <- inDat
  DARTbuildSizeXYZ <- 2
  XYsize <- inMeta$nx
  
  #1) switch coordinates
  outDat$X_centre <- inDat$Y_centre
  outDat$Y_centre <- inDat$X_centre
  
  #2) reverse X axis
  outDat$X_centre <- XYsize - outDat$X_centre
  
  #3) add extra columns
  outDat <- outDat %>%
    dplyr::mutate(objInd = 0, 
                  Xrot = 0, 
                  Yrot = 0,
                  Rotation = Rotation,
                  Z_centre = 0,
                  Xscale = (Radius / DARTbuildSizeXYZ) * 2,
                  Yscale = (Radius / DARTbuildSizeXYZ) * 2, 
                  H = H / DARTbuildSizeXYZ) %>%
    #4) sort columns
    dplyr::select(objInd, X_centre, Y_centre, Z_centre, Xscale, Yscale, H, Xrot, Yrot, Rotation)
  #5) write data
  outDatAll <- list()
  outDatAll$df <- outDat 
  writeDARTdf(outDatAll, oDir = outDir, fID = fID)
}
