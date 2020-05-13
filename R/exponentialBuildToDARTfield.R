library(tidyr)
library(dplyr)
library(tools)
source("R/functions.R")
inDirFull <- "sampleData/expBldsInput/"
inFilesFull <- list.files(inDirFull)
fID_raw <- unique(sapply(file_path_sans_ext(inFilesFull), function(x) strsplit(x, "_")[[1]][1]))[1]
fID <- buildingsFileID(fID_raw)
#the seed number is in the file name so I can't identify what is the actual data.
inFileFull <- "sampleData/expBldsInput/ZKS66_442000305.txt"
inDat <- read.table(inFileFull, header = TRUE)
inMetaFileFull <- "sampleData/expBldsInput/ZKS66_variables.txt"
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
