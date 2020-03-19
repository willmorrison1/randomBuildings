library(tidyr)
library(rgeos)
library(raster)
library(sp)
library(maptools)
library(rgdal)

#seed value for random operations
seedVal <- 2361
set.seed(seed = seedVal)

#output Directory for shape file
oDir <- "C:/PhD/Code/Analysis_Shapefile/Analyze Virtual London/shapefiles"

#total buildings in domain (forces building separation and width)
nBuildings <- 40
#plan area build fraction (lambda_p) (forces building separation and width)
lambda_p <- 0.3

#building height (normal) distribution
zVals <- c("mean" = 30, "sd" = 5)

#domain lengths (m)
DARTdomainDims <- c("X" = 430, "Y" = 430)

#DART building size (m) - the size of a DART cube in XYZ.
#all other building sizes in this code are scaled based on this size
DARTbuildSizeXY <- 1

#in XY coordinates, maximum spread of a building centroid from its regular grid location
#(multiplicative factor of distance between building centroids)
#e.g. building centroid distance bD = 10 (m) and XYoffset_factor = 0.5.
#Buildings will randomly be located at x'= x +- 2.5 m and y' = y +- 2.5 m
XYoffset_factor <- 0.4#0.15
#maximum building rotation +- north (deg). use 45 deg for most random (assuming nBuildings is large)
maxBuildRotation <- 45#45



#go
#predefine model domain
nBuildingsXorY <- ceiling(sqrt(nBuildings))
buildingDistance0 <- seq(0, DARTdomainDims["X"], length.out = nBuildingsXorY)
buildingDistance <- buildingDistance0[-1] - (diff(buildingDistance0) / 2)
singleBuildingDistance <- diff(buildingDistance)[1]
totalArea <- prod(DARTdomainDims)
#is lambda p the building area relative to all ground (including under buildings)?
buildingArea <- (lambda_p * totalArea) / nBuildings
buildingLength <- sqrt(buildingArea)
x <- y <- buildingDistance

#predefine overlapping plot flag
overlappingPolys <- TRUE
while (overlappingPolys) {

  print(seedVal)
  seedVal <- seedVal + 1
  set.seed(seed = seedVal)
  #initiate data frame with building centroid x,y locations
  outDF <- expand.grid(x = x, y = y)
  maxX <- max(x)
  maxY <- max(y)
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
  dartBuildXYsize <- (DARTbuildSizeXY * buildingLength) / 2
  outDF$Xscale <- outDF$Yscale <- dartBuildXYsize
  #set DART building heights (norm distribution)
  outDF$Zscale <- rnorm(n = nrow(outDF), mean = zVals["mean"], sd = zVals["sd"])
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
  buffWidth <- (buildingLength / 2)
  SPoints <- sp::SpatialPoints(outDF[c("x", "y")])
  spList <- list()

  for (i in seq_along(SPoints)) {
    spList[[i]] <- gBuffer(SPoints[i], width = buffWidth, quadsegs = 1, capStyle = "SQUARE")
    spList[[i]] <- maptools::elide(spList[[i]], rotate = outDF$Zrot[i], 
                                   center = coordinates(SPoints[i]))
  }
  
  SP <- do.call(bind, spList)
  LHS <- raster::shift(SP, dx = -DARTdomainDims["X"], dy = 0)
  topHS <- raster::shift(SP, dx = 0, dy = DARTdomainDims["Y"])

  SP_cycled <- do.call(bind, list(SP, LHS, topHS))
  area_preAgg <- as.numeric(gArea(SP_cycled))
  area_postAgg <- as.numeric(gArea(aggregate(SP_cycled)))

  if ((area_preAgg - area_postAgg) < 1e-5) overlappingPolys <- FALSE
}

SPbbox <- bbox(SP)
SP_shifted <- raster::shift(SP, dx = -SPbbox["x","min"], dy = -SPbbox["y","min"])

domainExtent <- raster::extent(0, DARTdomainDims["X"], 
                               0, DARTdomainDims["Y"])
newDomainExtent <- bbox(SP_shifted)
print(paste("Used seed", seedVal))
plot(SP_shifted, main = paste("Seed:", seedVal - 1))
axis(1, at = seq(-DARTdomainDims["X"], DARTdomainDims["X"], by = 20), cex.axis = 0.7)
axis(2, at = seq(-DARTdomainDims["Y"], DARTdomainDims["Y"], by = 20), cex.axis = 0.7)
rect(xleft = 0, ybottom = 0, xright = newDomainExtent["x", "max"], 
     ytop = newDomainExtent["y", "max"], lwd = 2)

actualPAI <- sum(area(SP_shifted)) / (newDomainExtent["x", "max"] * newDomainExtent["y", "max"])
print(actualPAI)
stop()
SPdf <- SpatialPolygonsDataFrame(Sr = SP, data = data.frame("z" = outDF$Zscale))
unlink(oDir)
writeOGR(SPdf, dsn = oDir, driver = "ESRI Shapefile", layer = "z", overwrite_layer = TRUE)

# unlink("V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt")
# writeLines(text = "complete transformation", con = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt")
# write.table(x = outDF, file = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt", sep = " ", col.names = FALSE, row.names = FALSE, append = TRUE)
