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

# Remove NULL entries that errored

outputs_filtered <- outputs[!sapply(outputs, is.null)]

# Get main results by problem and rowbind

main_models <- 1:length(outputs_filtered) %>%
  purrr::map_df(~ pull_main_models(results = outputs_filtered, x = .x, raw = FALSE))

rm(outputs, outputs_filtered)

#----------------- Scatterplots -------------------



#----------------- Lollipop plots -----------------

#' Function to draw lollipop comparison plots between the sets
#' @param set1 the string of a feature set
#' @param set2 the string of a comparison feature set
#' @return an object of class ggplot2
#' @author Trent Henderson
#' 

draw_lollipops <- function(set1, set2){
  
  # Define names as new vars as I'll reuse set1 and set2 as column names later
  
  set1_name <- set1
  set2_name <- set2
  
  # Define coordinates for plot shading
  
  upper_half <- data.frame(x = c(0, 0, 100), y = c(0, 100, 100))
  
  # Filter to sets of interest, compute difference, and draw plot
  
  p <- main_models %>%
    filter(method %in% c(set1, set2)) %>%
    dplyr::select(c(problem, method, balanced_accuracy)) %>%
    mutate(balanced_accuracy = balanced_accuracy * 100) %>%
    pivot_wider(id_cols = "problem", names_from = "method", values_from = "balanced_accuracy") %>%
    rename(set1 = 2,
           set2 = 3) %>%
    mutate(difference = set1 - set2,
           winner = ifelse(difference > 0, paste0(set1_name, " better"), paste0(set2_name, " better"))) %>%
    ggplot(aes(x = problem, y = difference, colour = winner)) +
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymax = 60, ymin = 0,
             fill = "steelblue2", alpha = 0.2) +
    scale_x_discrete() +
    annotate("text", x = 10, y = -50, label = paste0(set2, " better")) +
    annotate("text", x = 10, y = 50, label = paste0(set1, " better")) +
    geom_segment(aes(x = problem, xend = problem, y = 0, yend = difference)) +
    geom_point(size = 2) +
    labs(title = paste0("Comparative balanced accuracy between ", set1_name, " and ", set2_name),
         x = "Problem",
         y = "Balanced classification accuracy (%) difference",
         colour = NULL) +
    scale_colour_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(-60, 60),
                       breaks = seq(from = -60, to = 60, by = 20),
                       labels = function(x)paste(x, "%")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  print(p)
  
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
