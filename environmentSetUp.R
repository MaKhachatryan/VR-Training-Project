# Please always source this script first to set up the environment needed for 
# this project.


## ----- Loading necessary packages -----
packages <- c("tidyr",
              "dplyr",
              "stringr",
              "ggplot2",
              "knitr",
              "readr",
              "ggthemes",
              "readxl",
              "pheatmap",
              "patchwork")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

## ----- Loading data sets----
dataSet <- list.files("Data/processedData", pattern = "\\.csv$", full.names = TRUE)
for (ds in dataSet) {
  var_name <- tools::file_path_sans_ext(basename(ds))
  assign(var_name, read_csv(ds))
}

## ----- Sourcing functions -----
source("Script/utils/fixPMD.R")
source("Script/utils/cleanData.R")
source("Script/utils/pivotedData.R")
functionFolder <- "Script"
scriptFiles <- list.files(path = functionFolder, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
lapply(scriptFiles, source)
