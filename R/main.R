library(tidyr)
library(rgeos)
library(raster)
library(sp)
library(maptools)
library(rgdal)
library(devtools)

source("R/functions.R")
#source_url("https://raw.githubusercontent.com/willmorrison1/randomBuildings/master/R/functions.R")

#seed value for random operations
seedVal <- 1291
set.seed(seed = seedVal)

#output Directory for shape file
#oDir_base <- file.path(Sys.getenv("MM_PROCESSING"), "/hv867657/SPARTACUS/randomCubes/")
oDir_base <- "C:/Users/micromet/Desktop/temp"
if (!dir.exists(oDir_base)) dir.create(oDir_base)

nBuildings <- 800
lambda_p <- 0.5
DART_XorY_m <- 2000
DARTbuildSizeXYZ <- 2
XYoffset_factor <- 3
maxBuildRotation <- 45
z_mean <- 30
z_sd <- 0

buildDistribution <- createBuildingDistribution(nBuildings = nBuildings,
                                                lambda_p = lambda_p,
                                                z_mean = z_mean,
                                                z_sd = z_sd,
                                                DART_XorY_m = DART_XorY_m,
                                                DARTbuildSizeXYZ = DARTbuildSizeXYZ,
                                                XYoffset_factor = XYoffset_factor,
                                                maxBuildRotation = maxBuildRotation,
                                                seedVal = seedVal,
                                                maxIters = 200,
                                                forcePolygonSeparation = FALSE)

newDomainExtent <- buildDistribution$params$domainExtent
print(paste("Used seed", seedVal))
plot(buildDistribution$polygons, main = paste("Seed:", buildDistribution$params$seedVal))
axis(1, at = seq(-DART_XorY_m, DART_XorY_m, by = DART_XorY_m / 10), cex.axis = 0.7)
axis(2, at = seq(-DART_XorY_m, DART_XorY_m, by = DART_XorY_m / 10), cex.axis = 0.7)
rect(xleft = 0, ybottom = 0, xright = newDomainExtent["x", "max"],
     ytop = newDomainExtent["y", "max"], lwd = 2)
rect(xleft = 0, ybottom = 0, xright = DART_XorY_m,
     ytop = DART_XorY_m, lwd = 2, lty = 2)
points(buildDistribution$df$y,
       (buildDistribution$params$domainExtent[,2][2] - buildDistribution$df$x), pch = 20)
points(buildDistribution$polygonCentroids, pch = 20, cex = 0.5, col = "red")
writebuildDistribution(buildDistribution = buildDistribution, oDir = oDir_base)
