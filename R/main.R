library(tidyr)
library(rgeos)
library(raster)
library(sp)
library(maptools)
library(rgdal)

source("R/functions.R")
#seed value for random operations
seedVal <- 2365
set.seed(seed = seedVal)

#output Directory for shape file
oDir <- getwd()

nBuildings <- 175
lambda_p <- 0.37
DART_XorY_m <- 500
DARTbuildSizeXYZ <- 2
XYoffset_factor <- 0.45
maxBuildRotation <- 45
z_mean <- 30
z_sd <- 5

buildDistribution <- createBuildingDistribution(nBuildings = nBuildings, 
                                                lambda_p = lambda_p, 
                                                z_mean = z_mean, 
                                                z_sd = z_sd, 
                                                DART_XorY_m = DART_XorY_m, 
                                                DARTbuildSizeXYZ = DARTbuildSizeXYZ,
                                                XYoffset_factor = XYoffset_factor,
                                                maxBuildRotation = maxBuildRotation, 
                                                seedVal = seedVal, 
                                                maxIters = 200)


newDomainExtent <- buildDistribution$domainExtent
print(paste("Used seed", seedVal))
plot(buildDistribution$polygons, main = paste("Seed:", seedVal))
axis(1, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
axis(2, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
rect(xleft = 0, ybottom = 0, xright = newDomainExtent["x", "max"], 
     ytop = newDomainExtent["y", "max"], lwd = 2)
rect(xleft = 0, ybottom = 0, xright = DART_XorY_m, 
     ytop = DART_XorY_m, lwd = 2, lty = 2)
actualPAI <- buildDistribution$newPAI
oDir_ID <- writebuildDistribution(buildDistribution, oDir)
list.files(oDir_ID)
unlink(oDir_ID, recursive = TRUE)

