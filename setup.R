#------------------------------------------
# This script sets out to load all the 
# packages and folders necessary for the 
# project
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 29 October 2021
#-----------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(foreign)
library(theft)

# Create important folders if none exist

if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')
if(!dir.exists('data/feature-calcs')) dir.create('data/feature-calcs')
if(!dir.exists('R')) dir.create('R')

# Re-usable "not in" operator

'%ni%' <- Negate('%in%')
