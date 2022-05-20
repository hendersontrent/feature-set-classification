#-------------------------------------
# This script sets out to produce
# head-to-head analyses of performance
# between feature sets
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 19 May 2022
#-------------------------------------

# Load classification results

load("data/outputs.Rda")

#----------------- Scatterplots -------------------



#----------------- Lollipop plots -----------------

#' Function to draw lollipop comparison plots between the sets
#' @param set1 the string of a feature set
#' @param set2 the string of a comparison feature set
#' @return an object of class ggplot2
#' @author Trent Henderson
#' 

draw_lollipops <- function(set1, set2){
  
  xx
  
  p
  
  ggsave(paste0("output/lollipop_", set1, "_vs_", set2, ".pdf"), p)
}

# Get all pairwise combinations to map over

combns <- crossing(unique(outputs$Adiac$TestStatistics$method), unique(outputs$Adiac$TestStatistics$method),
                   .name_repair = "minimal") %>%
  rename(set1 = 1,
         set2 = 2) %>%
  filter(set1 != set2) %>% # Remove self-comparisons
  rowwise() %>% 
  mutate(alphabetical = paste(sort(unlist(strsplit(paste(set1, set2, sep = " "), " "))), collapse = " ")) %>% # Remove duplicates due to ordering
  distinct(alphabetical) %>%
  mutate(set1 = gsub(" .*", "\\1", alphabetical),
         set2 = gsub(".* ", "\\1", alphabetical)) %>% # Clean up to 2 columns to map over
  dplyr::select(-c(alphabetical))

# Draw plots

purrr::map2(combns$set1, combns$set2, draw_lollipops)
