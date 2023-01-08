#----------------------------------------
# This script sets out to analyse some
# case study problems to more deeply
# understand why a given feature set
# performs well or poorly on it
#----------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 19 September 2022
#-------------------------------------------

# Load classification results

load("data/outputs_z.Rda")

outputs_z <- outputs_z %>%
  mutate(method = case_when(
    method == "tsfel" ~ "TSFEL",
    method == "kats"  ~ "Kats",
    TRUE              ~ method)) %>%
  filter(problem %in% c("EthanolLevel", "CricketY", "ChlorineConcentration", "ArrowHead")) # Case study problems of interest

# Load raw time series

load("data/TimeSeriesData.Rda")

EthanolLevel <- TimeSeriesData %>%
  filter(problem == "EthanolLevel")

CricketY <- TimeSeriesData %>%
  filter(problem == "CricketY")

ChlorineConcentration <- TimeSeriesData %>%
  filter(problem == "ChlorineConcentration")

ArrowHead <- TimeSeriesData %>%
  filter(problem == "ArrowHead")

rm(TimeSeriesData)

# Load feature calculations

load("data/feature-calcs/z-scored/EthanolLevel.Rda")
EthanolLevel_feats <- outs_z
load("data/feature-calcs/z-scored/CricketY.Rda")
CricketY_feats <- outs_z
load("data/feature-calcs/z-scored/ChlorineConcentration.Rda")
ChlorineConcentration_feats <- outs_z
load("data/feature-calcs/z-scored/ArrowHead.Rda")
ArrowHead_feats <- outs_z
rm(outs_z)

# Calculate overall mean performance for each problem and set

perform <- outputs_z %>%
  group_by(problem, method) %>%
  summarise(x = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup() 

#------------------ Case study I: EthanolLevel ----------------

# Draw plot

plot_samples(data = EthanolLevel, n = 2, seed = 123)

# Identify top features

EthanolLevel_top <- compute_top_features2(EthanolLevel_feats, 
                                          id_var = "id", 
                                          group_var = "group",
                                          num_features = 40, 
                                          method = "z-score",
                                          test_method = "svmLinear",
                                          use_balanced_accuracy = TRUE,
                                          use_k_fold = TRUE,
                                          num_folds = 10,
                                          use_empirical_null =  TRUE,
                                          null_testing_method = "ModelFreeShuffles",
                                          p_value_method = "gaussian",
                                          num_permutations = 1000,
                                          seed = 123)

save(EthanolLevel_top, file = "data/case-studies/EthanolLevel_top.Rda")

# Draw plots like in the catch22 paper

EthanolLevel_plot <- plot_all_ts(data = EthanolLevel)
print(EthanolLevel_plot)
ggsave("output/EthanolLevel_sample.pdf", plot = EthanolLevel_plot, units = "in", height = 12, width = 10)

#------------------ Case study II: CricketY ------------------

# Draw plot

plot_samples(data = CricketY, n = 3, seed = 123)

# Identify top features

CricketY_top <- compute_top_features2(CricketY_feats, 
                                      id_var = "id", 
                                      group_var = "group",
                                      num_features = 40,
                                      method = "z-score",
                                      test_method = "svmLinear",
                                      use_balanced_accuracy = TRUE,
                                      use_k_fold = TRUE,
                                      num_folds = 10,
                                      use_empirical_null =  TRUE,
                                      null_testing_method = "ModelFreeShuffles",
                                      p_value_method = "gaussian",
                                      num_permutations = 1000,
                                      seed = 123)

save(CricketY_top, file = "data/case-studies/CricketY_top.Rda")

# Draw plots like in the catch22 paper

CricketY_plot <- plot_all_ts(data = CricketY)
print(CricketY_plot)
ggsave("output/CricketY_sample.pdf", plot = CricketY_plot, units = "in", height = 12, width = 10)

#------------------ Case study III: ChlorineConcentration -----------------

# Draw plot

plot_samples(data = ChlorineConcentration, n = 2, seed = 123)

# Identify top features

ChlorineConcentration_top <- compute_top_features2(ChlorineConcentration_feats, 
                                                   id_var = "id", 
                                                   group_var = "group",
                                                   num_features = 40, 
                                                   method = "z-score",
                                                   test_method = "svmLinear",
                                                   use_balanced_accuracy = TRUE,
                                                   use_k_fold = TRUE,
                                                   num_folds = 10,
                                                   use_empirical_null =  TRUE,
                                                   null_testing_method = "ModelFreeShuffles",
                                                   p_value_method = "gaussian",
                                                   num_permutations = 1000,
                                                   seed = 123)

save(ChlorineConcentration_top, file = "data/case-studies/ChlorineConcentration_top.Rda")

# Draw plots like in the catch22 paper

ChlorineConcentration_plot <- plot_all_ts(data = ChlorineConcentration)
print(ChlorineConcentration_plot)
ggsave("output/ChlorineConcentration_sample.pdf", plot = ChlorineConcentration_plot, units = "in", height = 12, width = 10)

#------------------ Case study IV: ArrowHead -----------------

# Draw plot

plot_samples(data = ArrowHead, n = 2, seed = 123)

# Identify top features

ArrowHead_top <- compute_top_features2(ArrowHead_feats, 
                                       id_var = "id", 
                                       group_var = "group",
                                       num_features = 40, 
                                       method = "z-score",
                                       test_method = "svmLinear",
                                       use_balanced_accuracy = TRUE,
                                       use_k_fold = TRUE,
                                       num_folds = 10,
                                       use_empirical_null =  TRUE,
                                       null_testing_method = "ModelFreeShuffles",
                                       p_value_method = "gaussian",
                                       num_permutations = 1000,
                                       seed = 123)

save(ArrowHead_top, file = "data/case-studies/ArrowHead_top.Rda")

# Draw plots like in the catch22 paper

ArrowHead_plot <- plot_all_ts(data = ArrowHead)
print(ArrowHead_plot)
ggsave("output/ArrowHead_sample.pdf", plot = ArrowHead_plot, units = "in", height = 12, width = 10)
