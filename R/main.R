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

nBuildings <- 50
lambda_p <- 0.3
DART_XorY_m <- 430
DARTbuildSizeXY <- 1
XYoffset_factor <- 0.5
maxBuildRotation <- 45
z_mean <- 30
z_sd <- 5

buildDistribution <- createBuildingDistribution(nBuildings = nBuildings, 
                                                lambda_p = lambda_p, 
                                                z_mean = z_mean, 
                                                z_sd = z_sd, 
                                                DART_XorY_m = DART_XorY_m, 
                                                DARTbuildSizeXY = DARTbuildSizeXY,
                                                XYoffset_factor = XYoffset_factor,
                                                maxBuildRotation = maxBuildRotation, 
                                                seedVal = seedVal, 
                                                maxIters = 200)


newDomainExtent <- bbox(buildDistribution$polygons)
print(paste("Used seed", seedVal))
plot(buildDistribution$polygons, main = paste("Seed:", seedVal))
axis(1, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
axis(2, at = seq(-DART_XorY_m, DART_XorY_m, by = 20), cex.axis = 0.7)
rect(xleft = 0, ybottom = 0, xright = newDomainExtent["x", "max"], 
     ytop = newDomainExtent["y", "max"], lwd = 2)
rect(xleft = 0, ybottom = 0, xright = 430, 
     ytop = 430, lwd = 2, lty = 2)
actualPAI <- sum(area(buildDistribution$polygons)) / (newDomainExtent["x", "max"] * newDomainExtent["y", "max"])


# unlink(oDir)
# writeOGR(buildDistribution$polygons, dsn = oDir, driver = "ESRI Shapefile", layer = "z", overwrite_layer = TRUE)

# unlink("V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt")
# writeLines(text = "complete transformation", con = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt")
# write.table(x = outDF, file = "V:/Tier_processing/hv867657/DARTfiles/fieldRwip_1.txt", sep = " ", col.names = FALSE, row.names = FALSE, append = TRUE)
