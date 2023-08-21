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
library(e1071)
library(scatterpie)
library(patchwork)
library(correctR)

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
if(!dir.exists('data/feature-calcs/bound')) dir.create('data/feature-calcs/bound')
if(!dir.exists('data/case-studies')) dir.create('data/case-studies')
if(!dir.exists('data/individual-feature-classifiers')) dir.create('data/individual-feature-classifiers')
if(!dir.exists('R')) dir.create('R')
if(!dir.exists('utilities')) dir.create('utilities')

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

mypal <- c("#FF0029", "#377EB8", "#66A61E", "#984EA3", "#00D2D5", 
           "#FF7F00", "#AF8D00", "#7F80CD", "#B3E900")

# Fix Python environment to where the Python libraries are installed on my machine

reticulate::use_virtualenv("/Users/trenthenderson/Documents/Git/feature-set-classification/feature-sets")

# Define list of problems that previous work used

keepers <- c("ACSF1", "Adiac", "ArrowHead", "Beef", "BeetleFly", "BirdChicken", "BME",
             "Car", "CBF", "Chinatown", "ChlorineConcentration", "CinCECGTorso", "Coffee",
             "Computers", "CricketX", "CricketY", "CricketZ", "Crop", "DiatomSizeReduction",
             "DistalPhalanxOutlineAgeGroup", "DistalPhalanxOutlineCorrect", "DistalPhalanxTW",
             "Earthquakes", "ECG200", "ECG5000", "ECGFiveDays", "ElectricDevices", "EOGHorizontalSignal",
             "EOGVerticalSignal", "EthanolLevel", "FaceAll", "FaceFour", "FacesUCR", "FiftyWords", "Fish",
             "FordA", "FordB", "FreezerRegularTrain", "FreezerSmallTrain", "GunPoint", "GunPointAgeSpan", 
             "GunPointMaleVersusFemale", "GunPointOldVersusYoung", "Ham", "Haptics", "Herring", "HouseTwenty",
             "InlineSkate", "InsectEPGRegularTrain", "InsectEPGSmallTrain", "InsectWingbeatSound", 
             "ItalyPowerDemand", "LargeKitchenAppliances", "Lightning2", "Lightning7", "Mallat", "Meat",
             "MedicalImages", "MiddlePhalanxOutlineAgeGroup", "MiddlePhalanxOutlineCorrect", "MiddlePhalanxTW",
             "MixedShapesRegularTrain", "MixedShapesSmallTrain", "MoteStrain", "OliveOil", "OSULeaf", "PhalangesOutlinesCorrect",
             "Phoneme", "PigAirwayPressure", "PigArtPressure", "PigCVP", "Plane", "PowerCons", "ProximalPhalanxOutlineAgeGroup",
             "ProximalPhalanxOutlineCorrect", "ProximalPhalanxTW", "RefrigerationDevices", "Rock", "ScreenType",
             "SemgHandGenderCh2", "SemgHandMovementCh2", "SemgHandSubjectCh2", "ShapeletSim", "ShapesAll",
             "SmallKitchenAppliances", "SmoothSubspace", "SonyAIBORobotSurface1", "SonyAIBORobotSurface2",
             "StarLightCurves", "Strawberry", "SwedishLeaf", "Symbols", "SyntheticControl", "ToeSegmentation1",
             "ToeSegmentation2", "Trace", "TwoLeadECG", "TwoPatterns", "UMD", "UWaveGestureLibraryAll",
             "UWaveGestureLibraryX", "UWaveGestureLibraryY", "UWaveGestureLibraryZ", "Wafer", "Wine", "WordSynonyms",
             "Worms", "WormsTwoClass", "Yoga")
