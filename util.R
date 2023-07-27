## ------------------------------------------------------------------------------------------------------------ ##
                                                      # Packages
## ------------------------------------------------------------------------------------------------------------ ##

util.packages.install <- function() {
  install.packages("dplyr")
  install.packages("tidyverse")
  install.packages("readr")
  install.packages("sqldf")
  install.packages("gtable")
  install.packages("grid")
  install.packages("gridExtra")
  install.packages("ggrepel")
  install.packages("ggplot2")
}

util.packages.load <- function() {
  library(dplyr)
  library(tidyverse)
  library(readr)
  library(sqldf)
  library(gtable)
  library(grid)
  library(gridExtra)
  library(ggrepel)
  library(ggplot2)
}

util.packages.install()
util.packages.load()