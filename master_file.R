# main file that manages ancillary files
library(plyr)
library(readr)
library(dplyr);library(xtable) # xtable only to write tables in latex
library(ggplot2)
library(gridExtra)
library(sjPlot)
library(sjlabelled)
library(stargazer)
library(effectsize)
rm(list=ls())

# define root directory
root <- "C:/Users/andrea/Dropbox/Werk - Projects/Innovation sharing/Data and Scripts/"
setwd(root)

# define working directories for outputs
## HP 1
hp1_dir <- paste(root, "HP1/Output/", sep="")
## HP 2
hp2_dir <- paste(root, "HP2/Output/", sep="")

# get data ready
## transform raw data into main dataframe
source("Data_management/data_management.R")

## create all the ancillary dataframes used in the analysis
source(file = "Data_management/upload_data_aggregated.R")
