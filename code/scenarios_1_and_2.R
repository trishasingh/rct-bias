library(tidyverse)
library(broom)
library(readr)
library(stringr)
library(ggplot2)

set.seed(2644)

n_sim <- 100

pupils_grp13 <- read_csv("~/Dropbox/EconThesis/data/Deworming_Kenya/for_r/pupils_grp13.csv")
site_vars_grp13 <- read_csv("~/Dropbox/EconThesis/data/Deworming_Kenya/for_r/site_vars_grp13.csv")

setwd("~/Dropbox/EconThesis/data/")

# Define some useful functions based on equations from notebook.
p1_calc <- function(perc, n1) {
  (perc * 20) / n1
}

p2_calc <- function(p1, n1) {
  (20 - n1 * p1) / (50 - n1)
}

stop_perc_calc <- function(n1) {
  round((n1 / 50), 1)
  # Will have to use different stop_perc for SAP.
}

model_true <- pupils_grp13 %>%
  lm(prs ~ t1 + elg98 + p1 + mk96_s + Y98sap1 + Y98sap2 + Y98sap3 + Y98sap4 + Istd1 
     + Istd2 + Istd3 + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
     + Isem1 + Isem2 + Isem3 + pop_3km_updated + popT_3km_updated + pop_36k_updated 
     + popT_36k_updated, weights = obs, data = .) %>%
  tidy() %>% 
  filter(grepl("t1", term))

true <- as.numeric(model_true[2])

compare_1_all <- NULL
compare_1_wt_all <- NULL

#Make dummy for whether site falls in convenience for scenario 1
site_vars_grp13 <- site_vars_grp13 %>% 
  mutate(n1_density = ifelse(incl_prob1_alt > .8, 1, 0))

# Define convenience thresholds such that n1 = 20
# here convenience sample is already defined.
#table(site_vars$incl_prob1_alt)

n1 <- 21

for (i in seq(.9, stop_perc_calc(n1), -.1)) {
# Simulation 1:  Convenience sampling school density
  
  simulation_results_1 <- NULL
  sim1_weighted <- NULL
  
for (k in 1:n_sim){
  site_vars <- site_vars_grp13 %>% 
    mutate(incl_prob = ifelse(n1_density == 1, i*20/n1, (20-i*20) / (50 - n1))) %>% 
    mutate(in_sample = rbinom(n = n(), size = 1, prob = incl_prob)) %>% 
    select(sch98v1, in_sample, incl_prob, treat_effect)
  pupils_grp <- pupils_grp13 %>% 
    left_join(., site_vars, by = "sch98v1")
  model_1 <- pupils_grp %>%
    lm(prs ~ t1 + elg98 + p1 + mk96_s + Y98sap1 + Y98sap2 + Y98sap3 + Y98sap4 + Istd1 
       + Istd2 + Istd3 + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
       + Isem1 + Isem2 + Isem3 + pop_3km_updated + popT_3km_updated + pop_36k_updated 
       + popT_36k_updated, weights = obs, data = ., subset = (in_sample == 1)) %>%
    tidy() %>% 
    filter(grepl("t1", term))
  
  model_1_wt <- pupils_grp %>%
    mutate(wt_obs = (1/incl_prob) * obs) %>% 
    lm(prs ~ t1 + elg98 + p1 + mk96_s + Y98sap1 + Y98sap2 + Y98sap3 + Y98sap4 + Istd1 
       + Istd2 + Istd3 + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
       + Isem1 + Isem2 + Isem3 + pop_3km_updated + popT_3km_updated + pop_36k_updated 
       + popT_36k_updated, weights = wt_obs, data = ., subset = (in_sample == 1)) %>%
    tidy() %>% 
    filter(grepl("t1", term)) %>% 
    rename(wt_estimate=estimate, wt_se=std.error, wt_tstat = statistic, wt_pval=p.value)
  
  ID_variables <- data.frame(iteration = k)
  simulation_results_1 <- simulation_results_1 %>%
    bind_rows(bind_cols(ID_variables, model_1, model_1_wt))
    
}

compare_1 <- simulation_results_1 %>% 
  mutate(emp_bias = estimate - true, se_bias = sd(emp_bias)/10) %>% 
  summarise(estimate = mean(estimate), p.value = mean(p.value),
            emp_bias = mean(emp_bias), se_bias = mean(se_bias)) %>% 
  mutate(ci_lower = emp_bias - 1.96*se_bias, ci_upper = emp_bias + 1.96*se_bias)


compare_1_wt <- 
  simulation_results_1 %>% 
  mutate(diff = abs(estimate - wt_estimate), se_diff = sd(diff)/10) %>%
  summarise(wt_estimate = mean(wt_estimate), wt_pval = mean(wt_pval),
            diff = mean(diff), se_diff = mean(se_diff)) %>% 
  mutate(ci_lower = diff - 1.96*se_diff, ci_upper = diff + 1.96*se_diff)


cv_p1 <- sd(site_vars$incl_prob, na.rm=TRUE)/ mean(site_vars$incl_prob, na.rm=TRUE)*100
var_te <- var(site_vars$treat_effect, na.rm=TRUE)
corr_p1_te <- cor(site_vars$incl_prob, site_vars$treat_effect)
bias1 <- - cv_p1 * var_te * corr_p1_te


ID_variables2_nw <- data.frame(iteration = i, type = "nw")
ID_variables2_w <- data.frame(iteration = i, type = "w")

compare_1_all <- compare_1_all %>%
  bind_rows(bind_cols(ID_variables2_nw, compare_1[1,], data.frame(bias1)))

compare_1_wt_all <- compare_1_wt_all %>%
  bind_rows(bind_cols(ID_variables2_w, compare_1_wt[1,], data.frame(bias1)))

}

compare_1_all <- compare_1_all %>% 
  rename(diff=emp_bias, se_diff=se_bias)

compare_1_wt_all <- compare_1_wt_all %>% 
  rename(estimate=wt_estimate, p.value=wt_pval)

data_viz1 <- bind_rows(compare_1_all, compare_1_wt_all) %>% 
  mutate(empirical_bias = estimate - 0.04666452)

wt_vs_unwt <- data_viz1 %>%
  filter(type=="w") %>% 
  ggplot(data = ., aes(x=iteration, y=diff)) + 
  geom_line() +
  geom_vline(xintercept=0.42, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, linetype="dashed") +
  annotate("text", x = 0.405, y = 0.015, angle = 90
           , label = "Population proportion of convenient sites"
           ) +
  labs(y = "Decrease in Bias", x = "Proportion of convenient sites in the population"
       , title = "Effect of Weighting by Inverse Inclusion Probability") +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))
  
ggsave("figures/final/weightedBias_density.png", width=7, height=4.5)

predicted_vs_empirical <- data_viz1 %>% 
  filter(type == "nw") %>% 
  ggplot(data = ., aes(x = iteration)) + 
  geom_line(aes(y = bias1, colour = "Predicted bias")) + 
  geom_line(aes(y = empirical_bias, colour = "Empirical bias")) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, color="red", linetype = "dashed") +
  geom_vline(xintercept=0.42, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept=true, linetype="dotted") +
  annotate("text", x = 0.405, y = 0.03, angle = 90, label = "Population proportion") +
  annotate("text", x = 0.552, y = 0.045, label = "Population parameter of impact") +
  labs(y = "Bias", x = "Proportion of convenient sites in the population"
       , title = "Empirical vs Predicted Bias") +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))

ggsave("figures/final/predictedBias_density.png", width=7, height=4.5)


# Simulation 2:  Convenience sampling NGO

#Make dummy for whether site falls in convenience for scenario 1
site_vars_grp13 <- site_vars_grp13 %>% 
  mutate(n1_sap = ifelse(incl_prob2 > .8, 1, 0))

n1 <- 15
n1_t1 <- 7
n1_t0 <- 8

compare_2_all <- NULL
compare_2_wt_all <- NULL

for (i in seq(.7, stop_perc_calc(n1), -.1)) {
  # Start from .7 because there are only 15 schools
  
  simulation_results_2 <- NULL
  sim2_weighted <- NULL
  
for (k in 1:n_sim){
  site_vars <- site_vars_grp13 %>% 
    mutate(incl_prob = ifelse(n1_sap == 1, i*20/n1, 
                              ifelse(n1_sap == 0 & t1 == 0, (10-i*n1_t0) / (25 - n1_t0),
                                     (10-i*n1_t1) / (25 - n1_t1)))) %>% 
    mutate(in_sample = rbinom(n = n(), size = 1, prob = incl_prob)) %>% 
    select(sch98v1, in_sample, incl_prob, treat_effect)
  pupils_grp <- pupils_grp13 %>% 
    left_join(., site_vars, by = "sch98v1")
  model_2 <- pupils_grp %>%
    lm(prs ~ t1 + elg98 + p1 + mk96_s + sap_any + Istd1 + Istd2 + Istd3 
       + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
       + pop_3km_updated + popT_3km_updated + pop_36k_updated 
       + popT_36k_updated, weights = obs, data = ., subset = (in_sample==1)) %>%
    tidy() %>% 
    filter(grepl("t1", term))
  # took out Isem1, 2, 3
  

  model_2_wt <- pupils_grp %>%
    mutate(wt_obs = (1/incl_prob2) * obs) %>% 
    lm(prs ~ t1 + elg98 + p1 + mk96_s + sap_any + Istd1 + Istd2 + Istd3 
       + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
       + pop_3km_updated + popT_3km_updated + pop_36k_updated 
       + popT_36k_updated, weights = wt_obs, data = ., subset = (in_sample == 1)) %>%
    tidy() %>% 
    filter(grepl("t1", term)) %>% 
    rename(wt_estimate=estimate, wt_se=std.error, wt_tstat = statistic, wt_pval=p.value)
  
  
  ID_variables <- data.frame(iteration = k)
  simulation_results_2 <- simulation_results_2 %>%
    bind_rows(bind_cols(ID_variables, model_2, model_2_wt))
  
}
  
  compare_2 <- simulation_results_2 %>% 
    mutate(emp_bias = estimate - true, se_bias = sd(emp_bias)/10) %>% 
    summarise(estimate = mean(estimate), p.value = mean(p.value),
              emp_bias = mean(emp_bias), se_bias = mean(se_bias)) %>% 
    mutate(ci_lower = emp_bias - 1.96*se_bias, ci_upper = emp_bias + 1.96*se_bias)
  
  
  compare_2_wt <- simulation_results_2 %>% 
    mutate(diff = abs(estimate - wt_estimate), se_diff = sd(diff)/10) %>%
    summarise(wt_estimate = mean(wt_estimate), wt_pval = mean(wt_pval),
              diff = mean(diff), se_diff = mean(se_diff)) %>% 
    mutate(ci_lower = diff - 1.96*se_diff, ci_upper = diff + 1.96*se_diff)
    
  
  cv_p2 <- sd(site_vars$incl_prob, na.rm=TRUE)/ mean(site_vars$incl_prob, na.rm=TRUE)*100
  var_te <- var(site_vars$treat_effect, na.rm=TRUE)
  corr_p2_te <- cor(site_vars$incl_prob, site_vars$treat_effect)
  bias2 <- - cv_p2 * var_te * corr_p2_te
    
  ID_variables2_nw <- data.frame(iteration = i, type = "nw")
  ID_variables2_w <- data.frame(iteration = i, type = "w")
  
  compare_2_all <- compare_2_all %>%
    bind_rows(bind_cols(ID_variables2_nw, compare_2[1,], data.frame(bias2)))
  
  compare_2_wt_all <- compare_2_wt_all %>%
    bind_rows(bind_cols(ID_variables2_w, compare_2_wt[1,], data.frame(bias2)))
  
}

compare_2_all <- compare_2_all %>% 
  rename(diff=emp_bias, se_diff=se_bias)

compare_2_wt_all <- compare_2_wt_all %>% 
  rename(estimate=wt_estimate, p.value=wt_pval) %>% 
  mutate(ci_lower=-ci_lower, ci_upper=-ci_upper)

data_viz2 <- bind_rows(compare_2_all, compare_2_wt_all) %>% 
  mutate(empirical_bias = estimate - 0.04666452)

 # data_viz2[3,10] <- -0.01019474
 # data_viz2[8,10] <- -0.00336388
# The above results were obtained by setting i = .5 and running the inner for loop with the same seed. 
# Weirdly enough this result is not showing up in the compare_2_all dataset.

wt_vs_unwt <- data_viz2 %>% 
  filter(type=="w") %>% 
  ggplot(data = ., aes(x=iteration, y=diff)) + 
  geom_line() +
  geom_vline(xintercept=0.3, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, linetype = "dashed") +
  annotate("text", x = 0.28, y = 0.009, angle = 90
           , label = "Population proportion of convenient sites") +
  labs(y = "Decrease in Bias", x = "Proportion of convenient sites in the population"
       , title = "Effect of Weighting by Inverse Inclusion Probability") +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))
  
ggsave("figures/final/weightedBias_sap.png", width=7, height=4.5)

predicted_vs_empirical <- data_viz2 %>% 
  filter(type == "nw") %>% 
  ggplot(data = ., aes(x = iteration)) + 
  geom_line(aes(y = -bias2, colour = "Predicted bias")) + 
  geom_line(aes(y = empirical_bias, colour = "Empirical bias")) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, color="red", linetype = "dashed") +
  geom_vline(xintercept=0.3, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept=-true, linetype="dotted") +
  annotate("text", x = 0.28, y = -0.0305, angle = 90, label = "Population proportion") +
  annotate("text", x = 0.425, y = -0.045, label = "-(Population parameter of impact)") +
  labs(y = "Bias", x = "Proportion of convenient sites in the population"
       , title = "Empirical vs Predicted Bias") +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))

ggsave("figures/final/predictedBias_sap2.png", width=7, height=4.5)




