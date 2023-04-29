library(tidyverse)

data <- read_csv(here::here("../Data/contamination_survey_key.csv"))

data$Label <- ""
data$Mixture_Ratio <- ""
data$Confidence <- ""

num_judges <- 10
seeds <- seq(1, num_judges, by = 1)

for(seed in seeds){
  
  set.seed(seed)
  random_rows <- data[sample(nrow(data)),]
  write_csv(random_rows %>% filter(Glucose > 500), here::here(paste0("../Data/dextrose_randomized/", seed, "_d5_survey.csv")))
  write_csv(random_rows %>% filter(Glucose <= 500), here::here(paste0("../Data/saline_randomized/", seed, "_ns_survey.csv")))
  
}

