library(tidyverse)
library(tidymodels)
library(readxl)
library(lme4)
library(ggridges)

# Global settings and variables
theme_ns <- theme(text = element_text(family = "Helvetica"),
                  title = element_text(face = "bold.italic", size = 18),
                  plot.title = element_text(hjust = 0.5),
                  axis.title = element_text(size = 18, face = "bold.italic"),
                  axis.ticks = element_blank(),
                  axis.text = element_text(size = 12), 
                  legend.title = element_text(face = "bold.italic", size = 12),
                  axis.line = element_line(),
                  panel.grid = element_blank(),
                  panel.background = element_blank(),
                  strip.text = element_text(size = 18, face = "bold.italic"),
                  strip.background = element_blank())
theme_set(theme_ns)

results_paths <- list.files("../Data/final_results", full.names = T)
csvs <- results_paths[which(grepl(".csv", results_paths))]
xls <- results_paths[which(grepl(".xls", results_paths))]

gpt_ns <-  list.files("../Results", full.names = T)[which(grepl("base_ns", list.files("../Results", full.names = T)))] %>% imap(~read_delim(.x) %>% mutate(Judge = paste0("GPT4_", .y)))
gpt_d5 <-  list.files("../Results", full.names = T)[which(grepl("base_d5", list.files("../Results", full.names = T)))] %>% imap(~read_delim(.x) %>% mutate(Judge = paste0("GPT4_", .y)))

data <- c(map(csvs, ~read_delim(.x, trim_ws = T)), map(xls, ~read_xlsx(.x)))
results_paths <- c(csvs, xls)
data <- map(data, ~mutate(.x, Label = str_to_upper(Label)))

judge <- gsub("_", "", str_match(results_paths, "\\d_"))
data <- map2(data, judge, ~mutate(.x, Judge = .y))

ns_results <- data[which(!grepl("d5", results_paths, ignore.case = T))] %>% c(gpt_ns)
d5_results <- data[which(grepl("d5", results_paths, ignore.case = T))] %>% c(gpt_d5)

key <- read_delim("../Data/contamination_survey_key.csv")
key <- key %>% rename("Truth" = "Label", "True_Ratio" = "Mixture_Ratio") %>% select(-Confidence)

ns_list <- map(ns_results, ~left_join(.x, key) %>% mutate(Truth = factor(Truth, levels = c("REAL", "NS")), Label = factor(Label, levels = c("REAL", "NS")), Experiment = factor("NS"), isGPT = as_factor(ifelse(grepl("GPT", Judge), "GPT4", "Expert"))))
d5_list <- map(d5_results, ~left_join(.x, key) %>% mutate(Truth = factor(Truth, levels = c("REAL", "D5NS")), Label = factor(Label, levels = c("REAL", "D5NS")), Experiment = factor("D5"), isGPT = as_factor(ifelse(grepl("GPT", Judge), "GPT4", "Expert"))))

results <- list(ns_list %>% bind_rows(), d5_list %>% bind_rows())
metrics <- metric_set(accuracy, mcc, sens, specificity, ppv, npv)

results_list <- map(results, ~mutate(.x, Correct = Truth == Label, Estimate_Error = Mixture_Ratio - True_Ratio))
results <- results_list %>% bind_rows()

metrics_summary <- map(results_list, ~.x %>% group_by(Judge, Experiment) %>% metrics(truth = Truth, estimate = Label, event_level = "second") %>% mutate(isGPT = as_factor(ifelse(grepl("GPT", Judge), "GPT4", "Expert")))) %>% bind_rows() %>% mutate(Metric = fct_rev(factor(.metric, levels = c("accuracy", "mcc", "sens", "specificity", "ppv", "npv"), labels = c("Accuracy", "MCC", "Sens", "Spec", "PPV", "NPV"))))

library(gtsummary)
metrics_summary %>% select(Fluid = Experiment, Metric, Value = .estimate, isGPT) %>% pivot_wider(names_from = Metric, values_from = Value) %>% unnest() %>%
  tbl_strata(strata = Fluid, .tbl_fun = ~ .x %>%
               tbl_summary(by = isGPT, type = list(everything() ~ "continuous")) %>%
               italicize_labels() %>%
               modify_header(all_stat_cols() ~ "***{level}***") %>%
               modify_footnote(update = all_stat_cols() ~ "Median: [95% CI]")) %>%
  as_gt() %>% gt::gtsave(filename = "../Figures/performance_metrics_table.pdf")

ggplot(metrics_summary, aes(.estimate, Metric, fill = isGPT)) + 
  geom_density_ridges(bandwidth = 0.05, from = 0, to = 1, alpha = 0.6, scale = 0.8, color = NA) +
  facet_wrap(~Experiment) + 
  scale_fill_viridis_d(option = "E", direction = -1, end = 0.8) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = c("0", "0.25", "0.5", "0.75", "1")) + 
  scale_y_discrete(expand = expansion(add = 0.1)) +
  theme(legend.position = c(0.1, 0.25), axis.text.y = element_text(face = "bold.italic", vjust = -1.5), legend.title = element_blank(), axis.title = element_blank())
ggsave("../Figures/metrics_ridgeline_base.pdf", width = 8.5, height = 4)

metrics_summary %>% group_by(isGPT, Experiment) %>% summarise(IQR = quantile(.estimate, probs = c(0.25, 0.75)))

judge_metadata <- read_delim("../Results/judge_metadata.txt") %>% rename(Pathology_Trained = Pathology, MD_Equivalent = MD_equivalent)

metrics_with_metadata <- left_join(metrics_summary, judge_metadata %>% mutate(Judge = as.character(Judge))) %>% filter(!isGPT == "GPT4")

metrics_with_metadata %>%
  select(Pathology_Trained, MD_Equivalent, Years_Training, Result = .estimate) %>%
    glm(Result ~ Pathology_Trained + MD_Equivalent + Years_Training, data = .) %>%
  tbl_regression(exponentiate = T, show_single_row = c(Pathology_Trained, MD_Equivalent)) %>% 
  as_gt() %>% gt::gtsave(filename = "../Figures/metadata_regression_table.pdf")

by_mixture_input <- map(results_list, ~.x %>% group_by(Judge, True_Ratio, Truth, Experiment) %>% summarise(Correct = sum(Label == Truth), prop = Correct/10)) %>% bind_rows() %>% mutate(True_Ratio = factor(True_Ratio), isGPT = factor(ifelse(grepl("GPT", Judge), "GPT4", "Expert")))

ggplot(by_mixture_input, aes(prop, fct_rev(True_Ratio), fill = isGPT)) + 
  geom_density_ridges(bandwidth = 0.1, from = 0, to = 1, alpha = 0.6, scale = 0.8, color = NA) +
  facet_wrap(~Experiment) +
  scale_fill_viridis_d(option = "E", direction = -1, end = 0.8) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = c("0", "0.25", "0.5", "0.75", "1")) + 
  scale_y_discrete(expand = expansion(add = 0.1), labels = c("0.25", "0.10", "Real")) +
  xlab("Accuracy") + ylab("Mixture Ratio") +
  theme(legend.position = c(0.1, 0.15), axis.text.y = element_text(vjust = -2.5, hjust = 1, margin = margin(.2,.2,.2,.2,"cm")), legend.title = element_blank())
ggsave("../Figures/mixture_ridgeline.pdf", width = 8.5, height = 4)

by_mixture_input %>% ungroup() %>%
tbl_strata(strata = c(Experiment, isGPT), .tbl_fun = ~ .x %>%
             tbl_summary(by = True_Ratio, include = prop, type = list(everything() ~ "continuous")) %>%
             italicize_labels() %>%
             modify_header(all_stat_cols() ~ "***{level}***") %>%
             modify_footnote(update = all_stat_cols() ~ "Median: [95% CI]"))
  

mixture_data <- results %>% mutate(True_Ratio = factor(case_when(True_Ratio == 0 ~ "Real", True_Ratio == 0.1 ~ "0.10", True_Ratio == "0.25" ~ "0.25"), level = c("Real", "0.10", "0.25")), isGPT = factor(ifelse(grepl("GPT", Judge), "GPT4", "Expert")))

mixture_data %>% filter(Correct & True_Ratio != "Real") %>%
  tbl_strata(strata = c(Experiment, True_Ratio, isGPT), .tbl_fun = ~ .x %>%
               tbl_summary(include = Estimate_Error, type = list(everything() ~ "continuous")) %>%
               italicize_labels() %>%
               modify_header(all_stat_cols() ~ "***{level}***") %>%
               modify_footnote(update = all_stat_cols() ~ "Median: [95% CI]")) %>%
  as_gt() %>% gt::gtsave(filename = "../Figures/estimate_error_table.pdf")


  ggplot(mixture_data %>% filter(Correct & True_Ratio != "Real"), aes(Estimate_Error, fct_rev(True_Ratio), fill = isGPT)) + 
    geom_density_ridges(bandwidth = 0.05, from = -0.25, to = 0.75, alpha = 0.6, scale = 1, color = NA) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(~Experiment) +
    scale_fill_viridis_d(option = "E", direction = -1, end = 0.8) + 
    scale_x_continuous(limits = c(-0.25, 0.75), breaks = seq(-0.25, 0.75, by = 0.25)) + 
    scale_y_discrete(expand = expansion(add = 0.1), labels = c("0.25", "0.10", "Real")) +
    xlab("Estimate Error") + ylab("True Mixture") + 
    theme(legend.position = c(0.9, 0.8), axis.text.y = element_text(vjust = -2.5, hjust = 1, margin = margin(.2,.2,.2,.2,"cm")), legend.title = element_blank())
  ggsave("../Figures/estimate_error_ridge_05_bandwidth.pdf", width = 8.5, height = 4)
    

gpt_prompted <- 
  list(
    ns_optimized = read_delim("../Results/gpt4_optimized_ns.txt") %>% mutate(Judge = "GPT_optimized", Experiment = "NS", isGPT = "GPT4") %>% left_join(key) %>% mutate(Truth = factor(Truth, levels = c("REAL", "NS")), Label = factor(Label, levels = c("REAL", "NS")), Experiment = factor("NS")),
    ns_with_split = read_delim("../Results/gpt4_with_split_ns.txt") %>% mutate(Judge = "GPT_with_splits", Experiment = "NS", isGPT = "GPT4") %>% left_join(key) %>% mutate(Truth = factor(Truth, levels = c("REAL", "NS")), Label = factor(Label, levels = c("REAL", "NS")), Experiment = factor("NS")),
    d5_optimized = read_delim("../Results/gpt4_optimized_d5.txt") %>% mutate(Judge = "GPT_optimized", Experiment = "D5", isGPT = "GPT4") %>% left_join(key) %>% mutate(Truth = factor(Truth, levels = c("REAL", "D5NS")), Label = factor(Label, levels = c("REAL", "D5NS")), Experiment = factor("D5")), 
    d5_with_split = read_delim("../Results/gpt4_with_split_d5.txt") %>% mutate(Judge = "GPT_with_splits", Experiment = "D5", isGPT = "GPT4") %>% left_join(key) %>% mutate(Truth = factor(Truth, levels = c("REAL", "D5NS")), Label = factor(Label, levels = c("REAL", "D5NS")), Experiment = factor("D5"))
    )


  gpt_prompted %>% 
    map(~ .x %>% group_by(Judge, Experiment) %>% metrics(truth = Truth, estimate = Label, event_level = "second")) %>%
    bind_rows() %>% mutate(Metric = fct_rev(factor(.metric, levels = c("accuracy", "mcc", "sens", "specificity", "ppv", "npv"), labels = c("Accuracy", "MCC", "Sens", "Spec", "PPV", "NPV")))) %>%
    bind_rows(metrics_summary %>% filter(isGPT == "GPT4") %>% 
                mutate(Metric = fct_rev(factor(.metric, levels = c("accuracy", "mcc", "sens", "specificity", "ppv", "npv"), labels = c("Accuracy", "MCC", "Sens", "Spec", "PPV", "NPV")))) %>% 
                group_by(Metric, Experiment) %>% 
                summarise(.estimate = median(.estimate), Metric, Judge = "GPT_base")) %>% 
      select(Judge, Experiment, Result = .estimate, Metric) %>% pivot_wider(names_from = Metric, values_from = Result, values_fn = median) %>%
      tbl_strata(Experiment, .tbl_fun = ~ .x %>%
                   tbl_summary(by = Judge, digits = list(everything() ~ 2), type = list(everything() ~ "continuous"), statistic = list(everything() ~ "{median}")) %>%
                   italicize_labels() %>%
                   modify_header(all_stat_cols() ~ "***{level}***") %>%
                   modify_footnote(update = everything() ~ NA)) %>% 
    as_gt() %>% gt::gtsave(filename = "../Figures/gpt_prompt_engineered_table.pdf")

  
  ggplot(results %>% mutate(Judge = ifelse(isGPT == "GPT4", "GPTs", Judge), Correct = ifelse(Correct, "Correct", "Incorrect")), aes(Confidence, fct_rev(Judge), fill = Correct)) + 
    geom_density_ridges(bandwidth = 0.08, from = 0, to = 1, alpha = 0.7, scale = 0.8, color = NA) +
    facet_wrap(~Experiment) +
    scale_fill_manual(values = c("Correct" = "grey20", "Incorrect" = "darkred")) + 
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), labels = c("0", "0.25", "0.5", "0.75", "1")) + 
    xlab("Confidence") + ylab("Judge") +
    theme(legend.position = c(0.1, 0.1), legend.background = element_blank(), axis.text.y = element_text(vjust = -1.5, hjust = 1, margin = margin(.2,.2,.2,.2,"cm")), legend.title = element_blank())
  ggsave("../Figures/confidence_ridges.pdf", width = 8.5, height = 6)  

  conf_with_metadata <- 
    results %>% mutate(Judge = as.numeric(ifelse(isGPT == "GPT4", "GPTs", Judge)), Correct = ifelse(Correct, "Correct", "Incorrect"), True_Mixture = True_Ratio, True_Label = Truth) %>%
    left_join(judge_metadata) 
  
  conf_lme <-
    conf_with_metadata %>% mutate(Target = ifelse(Correct == "Correct", 1, 0)) %>% filter(Judge != "GPTs") %>%
      lme(Confidence ~ Pathology_Trained + MD_Equivalent + Director + Years_Training, random = ~1|Judge, data = .) 
  summary(conf_lme)
  
  lme_conf <- 
    conf_with_metadata %>% mutate(Target = ifelse(Correct == "Correct", 1, 0)) %>% filter(Judge != "GPTs") %>%
    lmer(Confidence ~ Pathology_Trained + MD_Equivalent + Director + Years_Training + (1|Judge) + (1|Truth) + (1|True_Mixture), data = .)
  ci_conf <- confint(lme_conf) 
  
  lme_acc <- 
    conf_with_metadata %>% mutate(Target = ifelse(Correct == "Correct", 1, 0)) %>% filter(Judge != "GPTs") %>%
      lmer(Target ~ Confidence + Pathology_Trained + MD_Equivalent + Director + Years_Training + (1|Judge) + (1|Truth) + (1|True_Mixture), data = .)
  ci_acc <- confint(lme_acc)

  