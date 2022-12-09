#------------------------------------------
# This script produces a summary dataframe
# of descriptives for each dataset that
# can be easily called in correlated t-test
# analyses
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 02 December 2022
#------------------------------------------

load("data/TimeSeriesData.Rda")

# Summarise dataset

problem_summaries <- TimeSeriesData %>%
  dplyr::select(c(problem, set_split, id)) %>%
  distinct() %>%
  group_by(problem, set_split) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = "problem", names_from = "set_split", values_from = "counter") %>%
  dplyr::select(c(problem, Train, Test))

rm(TimeSeriesData)
