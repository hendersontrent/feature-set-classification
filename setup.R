#------------------------------------------
# This script sets out to load all the 
# packages and folders necessary for the 
# project
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 29 October 2021
#-----------------------------------------

library(data.table)
library(dplyr)
library(tibble)
library(magrittr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(foreign)
library(Rcatch22)
library(theft)
library(reticulate)
library(ggpubr)

# Create important folders if none exist

if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')
if(!dir.exists('R')) dir.create('R')
