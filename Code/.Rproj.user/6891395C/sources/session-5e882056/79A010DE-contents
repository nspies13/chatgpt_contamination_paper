library(tidyverse)

sim <- qs::qread(here::here("../Data/contam_sim"))

sample <- 
  sim %>% 
    na.omit() %>% 
    filter(label %in% c("NS", "D5NS") & 
             (near(percent_fluid, 0.1) | near(percent_fluid, 0.25))) %>% 
    group_by(label, percent_fluid) %>% 
    slice_sample(n = 10)

real <- qs::qread(here::here("../Data/bmp_no_NA"))

real_sample <- real %>% unnest(comments) %>% filter(is.na(long_text) & chloride < 106 & calcium > 7.5) %>% slice_sample(n = 10)
real_no_contam <- real %>% arrange(patient_id, drawn_dt_tm) %>% filter(glucose > 300 & lag(glucose > 300) & lead(glucose > 300) & anion_gap > 12) %>% unnest(comments) %>% filter(!grepl("nlikely|ode 7|emolyz|ontam", long_text)) %>% distinct(specimen_id, .keep_all = T)

glucose_matched <- 
  apply(sample[which(sample$label == "D5NS"), "glucose"], 1, 
         function(glucose_to_match) real_no_contam %>% filter(near(glucose, glucose_to_match, tol = 50)) %>% slice_sample(n = 1)) %>% bind_rows() %>% slice_sample(prop = 0.5)

output <- bind_rows(real_sample, glucose_matched, sample) %>% select(sodium, chloride, potassium_plas, co2_totl, bun, creatinine, calcium, glucose, label, percent_fluid)

output$confidence <- ""

output %>% 
  rename(Sodium = sodium, Chloride = chloride, Potassium = potassium_plas, Bicarbonate = co2_totl, BUN = bun, Creatinine = creatinine, Calcium = calcium, Glucose = glucose, Label = label, Mixture_Ratio = percent_fluid, Confidence = confidence) %>% 
  write_csv(here::here("../Data/contamination_survey_key.csv"))
