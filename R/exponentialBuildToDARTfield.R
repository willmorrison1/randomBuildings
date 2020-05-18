library(tidyr)
library(dplyr)
library(tools)
source("R/functions.R")
inDirFull <- "sampleData/expBldsInput/"
inFilesFull <- list.files(inDirFull, pattern = "centroids.txt", full.names = TRUE)

expBuildToDARTfield(inFileFull = inFilesFull[1])
