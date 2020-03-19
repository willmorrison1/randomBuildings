library(tidyr)
library(rgeos)
library(raster)
library(sp)
library(maptools)
library(rgdal)

source("R/functions.R")
#seed value for random operations
seedVal <- 2361
set.seed(seed = seedVal)

#output Directory for shape file
oDir <- "C:/PhD/Code/Analysis_Shapefile/Analyze Virtual London/shapefiles"

#total buildings in domain (forces building separation and width)
nBuildings <- 40
#plan area build fraction (lambda_p) (forces building separation and width)
lambda_p <- 0.3

#domain horizontal length in X and Y (m)
DART_XorY_m <- 430

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


for(i in seq(25, 150, by = 5)){
  print(i)
  buildDistribution <- createBuildingDistribution(nBuildings = i, lambda_p = 0.15, z_mean = 30, z_sd = 5, 
                                                  DART_XorY_m = 430, DARTbuildSizeXY = 1,
                                                  XYoffset_factor = 0.2, maxBuildRotation = 45, 
                                                  seedVal = 2361, maxIters = 200)
  
  
  newDomainExtent <- bbox(buildDistribution$polygons)
  print(paste("Used seed", seedVal))
  plot(buildDistribution$polygons, main = paste("Seed:", seedVal - 1))
  axis(1, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
  axis(2, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
  rect(xleft = 0, ybottom = 0, xright = newDomainExtent["x", "max"], 
       ytop = newDomainExtent["y", "max"], lwd = 2)
  rect(xleft = 0, ybottom = 0, xright = 430, 
       ytop = 430, lwd = 2, lty = 2)
  actualPAI <- sum(area(buildDistribution$polygons)) / (newDomainExtent["x", "max"] * newDomainExtent["y", "max"])
  print(actualPAI)
  Sys.sleep(1)
}


# unlink(oDir)
# writeOGR(buildDistribution$polygons, dsn = oDir, driver = "ESRI Shapefile", layer = "z", overwrite_layer = TRUE)

# unlink("V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt")
# writeLines(text = "complete transformation", con = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt")
# write.table(x = outDF, file = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt", sep = " ", col.names = FALSE, row.names = FALSE, append = TRUE)
