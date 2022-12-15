#-----------------------------------------
# This script sets out to load all the 
# packages and folders necessary for the 
# project
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 29 October 2021
#-----------------------------------------

library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(scales)
library(foreign)
library(theft)
library(Cairo)
library(scatterpie)

# Create important folders if none exist

if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('analysis/z-scored')) dir.create('analysis/z-scored')
if(!dir.exists('analysis/non-z-scored')) dir.create('analysis/non-z-scored')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('output/z-scored')) dir.create('output/z-scored')
if(!dir.exists('output/non-z-scored')) dir.create('output/non-z-scored')
if(!dir.exists('data')) dir.create('data')
if(!dir.exists('data/feature-calcs')) dir.create('data/feature-calcs')
if(!dir.exists('data/feature-calcs/z-scored')) dir.create('data/feature-calcs/z-scored')
if(!dir.exists('R')) dir.create('R')
if(!dir.exists('scribble')) dir.create('scribble')

# Re-usable "not in" operator

'%ni%' <- Negate('%in%')

# Load re-usable functions

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")

for(f in r_files){
  source(f)
}

rm(r_files)

# Suppress dplyr::summarise info

options(dplyr.summarise.inform = FALSE)

# Define reusable colour palette

mypal <- c("#69ef7b", "#cb1775", "#5e9222", "#d130ff", "#a4d483", 
           "#60249e", "#c7e11f", "#5e5698", "#fcd107", "#ee84ec")
