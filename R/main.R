library(tidyr)
library(rgeos)
library(raster)
library(sp)
library(maptools)
library(rgdal)

source("R/functions.R")
#seed value for random operations
seedVal <- 24991111
set.seed(seed = seedVal)

#output Directory for shape file
oDir <- "C:/PhD/Code/Analysis_Shapefile/Analyze Virtual London/shapefiles"

#total buildings in domain (forces building separation and width)
nBuildings <- 14
#plan area build fraction (lambda_p) (forces building separation and width)
lambda_p <- 0.15
#domain horizontal length in X and Y (m)
DART_XorY_m <- 430
#DART building size (m) - the size of a DART cube in XYZ.
#all other building sizes in this code are scaled based on this size
DARTbuildSizeXY <- 1
#in XY coordinates, maximum spread of a building centroid from its regular grid location
#(multiplicative factor of distance between building centroids)
#e.g. building centroid distance bD = 10 (m) and XYoffset_factor = 0.5.
#Buildings will randomly be located at x'= x +- 2.5 m and y' = y +- 2.5 m
XYoffset_factor <- 0.2
#maximum building rotation +- north (deg). use 45 deg for most random (assuming nBuildings is large)
maxBuildRotation <- 45
#mean building height (m)
z_mean = 30
#standard deviation of building height (m)
z_sd = 5

buildDistribution <- createBuildingDistribution(nBuildings = nBuildings, 
                                                lambda_p = lambda_p, 
                                                z_mean = z_mean, 
                                                z_sd = z_sd, 
                                                DART_XorY_m = DART_XorY_m, 
                                                DARTbuildSizeXY = DARTbuildSizeXY,
                                                XYoffset_factor = 1, 
                                                maxBuildRotation = maxBuildRotation, 
                                                seedVal = seedVal, 
                                                maxIters = 200)


newDomainExtent <- bbox(buildDistribution$polygons)
X11()

plot(buildDistribution$polygons, main = paste("Seed:", seedVal - 1))
axis(1, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
axis(2, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
rect(xleft = 0, ybottom = 0, xright = newDomainExtent["x", "max"], 
     ytop = newDomainExtent["y", "max"], lwd = 2)
rect(xleft = 0, ybottom = 0, xright = 430, 
     ytop = 430, lwd = 2, lty = 2)
actualPAI <- sum(area(buildDistribution$polygons)) / (newDomainExtent["x", "max"] * newDomainExtent["y", "max"])
print(actualPAI)
print(paste("DART x is:", newDomainExtent["y", "max"]))
print(paste("DART y is:", newDomainExtent["x", "max"]))
# unlink(oDir)
# writeOGR(buildDistribution$polygons, dsn = oDir, driver = "ESRI Shapefile",
#layer = "z", overwrite_layer = TRUE)

unlink("V:/Tier_processing/hv867657/DARTfiles/fieldRwip_2.txt")
writeLines(text = "complete transformation", con = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_2.txt")
write.table(x = buildDistribution$df, file = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_2.txt", sep = " ", col.names = FALSE, row.names = FALSE, append = TRUE)
