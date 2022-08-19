#----------------------------------------
# This script sets out to analyse some
# case study problems to more deeply
# understand why a given feature set
# performs well or poorly on it
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 18 August 2022
#----------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method)) %>%
  filter(problem %in% c("Coffee", "ProximalPhalanxOutlineAgeGroup", "Plane")) # Case study problems of interest

# Load raw time series

load("data/TimeSeriesData.Rda")

TimeSeriesData_filt <- TimeSeriesData %>%
  filter(problem %in% c("Coffee", "ProximalPhalanxOutlineAgeGroup", "Plane")) 

rm(TimeSeriesData)

#------------------ Case study I: Coffee ------------------

#----------------------------------------
# PREMISE: All sets perform about average
# and we want to understand why
#----------------------------------------



#------------------ Case study II: ProximalPhalanxOutlineAgeGroup -----------------

#--------------------------------------------
# PREMISE: tsfresh performs far below average
# and we want to understand why
#--------------------------------------------



#------------------ Case study III: Plane ----------------

#--------------------------------------------
# PREMISE: catch22 performs far above average
# and we want to understand why
#--------------------------------------------


