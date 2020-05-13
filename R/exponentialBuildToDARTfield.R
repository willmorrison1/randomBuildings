library(tidyr)
library(dplyr)
library(tools)
source("R/functions.R")
inFileFull <- "sampleData/expBldsInput/9NLE6_10400305.txt"
fID <- buildingsFileID(file_path_sans_ext(basename(inFileFull)))
inDat <- read.table(inFileFull, header = TRUE)
outDat <- inDat
DARTbuildSizeXYZ <- 2
XYsize <- 400

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
                Z_centre = DARTbuildSizeXYZ / 2,
                Xscale = (Radius / DARTbuildSizeXYZ) * 2,
                Yscale = (Radius / DARTbuildSizeXYZ) * 2, 
                H = H / DARTbuildSizeXYZ) %>%
  #4) sort columns
  dplyr::select(objInd, X_centre, Y_centre, Z_centre, Xscale, Yscale, H, Xrot, Yrot, Rotation)
#5) write data
outDatAll <- list()
outDatAll$df <- outDat 
writeDARTdf(outDatAll, oDir = "C:/Users/micromet/Desktop/", fID = fID)
plot(outDat$Y_centre, XYsize - outDat$X_centre, main = "build centroids")
