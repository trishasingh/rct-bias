library(tidyverse)
library(broom)
library(readr)
library(stringr)
library(ggplot2)

set.seed(26464)

n_sim <- 100

pupils_grp13 <- read_csv("~/Dropbox/EconThesis/data/Deworming_Kenya/for_r/pupils_grp13.csv")
site_vars_grp13 <- read_csv("~/Dropbox/EconThesis/data/Deworming_Kenya/for_r/site_vars_new.csv")
# For roads number analysis (done first)
busia_roads_1 <- read_csv("~/Dropbox/EconThesis/data/kenya_shapefiles/created/busia_roads_1.csv")

# Code = 4 means main road with motorways

# Some useful functions:
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


zone_roads <- busia_roads_1 %>%
  mutate(main_road = ifelse(CODE==4, 1, 0)) %>% 
  mutate(tert_road = ifelse(CODE==5, 1, 0)) %>% 
  group_by(NAME) %>% 
  summarise(main_roads = sum(main_road), tert_roads = sum(tert_road)) %>% 
  mutate(join_zone = ifelse(NAME == "AGENG'A NANGUBA", 1,
                         ifelse(NAME == "BUNYALA CENTRAL", 2,
                                ifelse(NAME == "BUNYALA NORTH", 3,
                                       ifelse(NAME == "BUNYALA SOUTH", 4,
                                              ifelse(NAME == "BUNYALA WEST", 4,
                                                     ifelse(NAME == "BWIRI", 5,
                                                            ifelse(NAME == "NAMBOBOTO NAMBUKU", 7,
                                                                   ifelse(NAME == "NANGINA", 6, NA)
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
         ) %>% 
  select(-NAME) %>% 
  group_by(join_zone) %>% 
  summarise(main_roads = sum(main_roads), tert_roads = sum(tert_roads))

site_vars <- site_vars_grp13 %>% 
  mutate(join_zone = ifelse(zoneid == 8, 7, zoneid)) %>% 
  left_join(., zone_roads, by = "join_zone")
  
#zone_roads
# Defining dummies for each type of classification: number of main roads and number of main + tertiary roads

site_vars <- site_vars %>% 
  mutate(geq_9 = ifelse(main_roads >= 9, 1, 0))
# If I choose most in with 9 roads and above, that will make 11 sites.

# site_vars <- site_vars %>% 
#   mutate(incl_prob3 = ifelse(geq_9 == 1, .8, 
#                             ifelse(geq_9 == 0 & t1 == 1, .08,
#                                    ifelse(geq_9 == 0 & t1 == 0, .075, 0))))

# Set the threshold for defining convenient sites such that n1 ~= 20.
#site_vars %>% group_by(join_zone, t1) %>% summarise(main_roads = first(main_roads)
# , tert_roads = first(tert_roads), num_sites = n())
# Threshold is 9 primary sites, n1 = 22.

n1 <- 22

# Simulation begins

compare_3_all <- NULL
compare_3_wt_all <- NULL

model_true <- pupils_grp13 %>%
  lm(prs ~ t1 + elg98 + p1 + mk96_s + Y98sap1 + Y98sap2 + Y98sap3 + Y98sap4 + Istd1 
     + Istd2 + Istd3 + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
     + Isem1 + Isem2 + Isem3 + pop_3km_updated + popT_3km_updated + pop_36k_updated 
     + popT_36k_updated, weights = obs, data = .) %>%
  tidy() %>% 
  filter(grepl("t1", term))

# geq_9 is dummy for whether site falls in convenience.

# Simulation 1:  Convenience sampling road networks
for (i in seq(.9, stop_perc_calc(n1), -.1)) {
  simulation_results3 <- NULL
  sim3_weighted <- NULL
for (k in 1:n_sim){
  site_vars_1 <- site_vars %>% 
    mutate(incl_prob = ifelse(geq_9 == 1, i*20/n1, (20-i*20) / (50 - n1))) %>% 
    mutate(in_sample = rbinom(n = n(), size = 1, prob = incl_prob)) %>% 
    #mutate(in_sample = rbinom(n = n(), size = 1, prob = incl_prob3)) %>% 
    select(sch98v1, in_sample, incl_prob, treat_effect)
  pupils_grp <- pupils_grp13 %>% 
    left_join(., site_vars_1, by = "sch98v1")
  
  model_3 <- pupils_grp %>%
    lm(prs ~ t1 + elg98 + p1 + mk96_s + sap_any + Istd1 + Istd2 + Istd3 
       + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
       + Isem1 + Isem2 + Isem3 + pop_3km_updated + popT_3km_updated + pop_36k_updated 
       + popT_36k_updated, weights = obs, data = ., subset = (in_sample == 1)) %>%
    tidy() %>% 
    filter(grepl("t1", term))
  
  model_3_wt <- pupils_grp %>%
    mutate(wt_obs = (1/incl_prob) * obs) %>% 
    lm(prs ~ t1 + elg98 + p1 + mk96_s + sap_any + Istd1 + Istd2 + Istd3 
       + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
       + Isem1 + Isem2 + pop_3km_updated + popT_3km_updated + pop_36k_updated 
       + popT_36k_updated, weights = wt_obs, data = ., subset = (in_sample == 1)) %>%
    tidy() %>% 
    filter(grepl("t1", term)) %>% 
    rename(wt_estimate=estimate, wt_se=std.error, wt_tstat = statistic, wt_pval=p.value)
  
  ID_variables <- data.frame(iteration = k)
  simulation_results3 <- simulation_results3 %>%
    bind_rows(bind_cols(ID_variables, model_3, model_3_wt))

}
  compare_3 <- simulation_results3 %>% 
    mutate(emp_bias = estimate - true, se_bias = sd(emp_bias)/10) %>% 
    summarise(estimate = mean(estimate), p.value = mean(p.value),
              emp_bias = mean(emp_bias), se_bias = mean(se_bias)) %>% 
    mutate(ci_lower = emp_bias - 1.96*se_bias, ci_upper = emp_bias + 1.96*se_bias)
    
  
  compare_3_wt <- simulation_results3 %>% 
    mutate(diff = abs(estimate - wt_estimate), se_diff = sd(diff)/10) %>%
    summarise(wt_estimate = mean(wt_estimate), wt_pval = mean(wt_pval),
              diff = mean(diff), se_diff = mean(se_diff)) %>% 
    mutate(ci_lower = diff - 1.96*se_diff, ci_upper = diff + 1.96*se_diff)

  
  cv_p3 <- sd(site_vars_1$incl_prob, na.rm=TRUE)/ mean(site_vars_1$incl_prob, na.rm=TRUE)*100
  var_te <- var(site_vars_1$treat_effect, na.rm=TRUE)
  corr_p3_te <- cor(site_vars_1$incl_prob, site_vars_1$treat_effect)
  bias3 <-  cv_p3 * var_te * corr_p3_te
  

  ID_variables3_nw <- data.frame(iteration = i, type = "Not weighted")
  ID_variables3_w <- data.frame(iteration = i, type = "Weighted")
  
  compare_3_all <- compare_3_all %>%
    bind_rows(bind_cols(ID_variables3_nw, compare_3[1,], data.frame(bias3)))
  
  compare_3_wt_all <- compare_3_wt_all %>%
    bind_rows(bind_cols(ID_variables3_w, compare_3_wt[1,], data.frame(bias3)))
}

data_viz3 <- bind_rows(compare_3_all, compare_3_wt_all) %>% 
  mutate(empirical_bias = estimate - 0.04666452) 

wt_vs_unwt <- compare_3_wt_all %>% 
  ggplot(data = ., aes(x=iteration, y=diff)) + 
  geom_line() +
  geom_vline(xintercept=0.44, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, linetype = "dashed") +
  annotate("text", x = 0.42, y = 0.011, angle = 90
           , label = "Population proportion of convenient sites"
           ) +
  labs(y = "Decrease in Bias", x = "Proportion of convenient sites in the population"
       , title = "Effect of Weighting by Inverse Inclusion Probability") +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))
  
ggsave("figures/final/weightedBias_roads.png", width=7, height=4.5)


predicted_vs_empirical <- compare_3_all %>% 
  ggplot(data = ., aes(x = iteration)) + 
  geom_line(aes(y = bias3, colour = "Predicted bias")) + 
  geom_line(aes(y = emp_bias, colour = "Empirical bias")) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, color="red", linetype = "dashed") +
  geom_vline(xintercept=0.44, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept=true, linetype="dotted") +
  annotate("text", x = 0.42, y = 0.03, angle = 90
           , label = "Population proportion"
           ) +
  annotate("text", x = 0.6, y = 0.045, label = "Population parameter of impact") +
  labs(y = "Bias", x = "Proportion of convenient sites in the population"
       , title = "Empirical vs Predicted Bias"
       ) +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))
  

ggsave("figures/final/predictedBias_roads.png", width=7, height=4.5)


# SCENARIO 4: Distance to ICL Office

dist_ICL_1 <- read_csv("~/Dropbox/EconThesis/data/kenya_shapefiles/created/dist_ICL.csv")

distances <- dist_ICL_1 %>%
  mutate(join_zone = ifelse(InputID == "AGENG'A NANGUBA", 1,
                            ifelse(InputID == "BUNYALA CENTRAL", 2,
                                   ifelse(InputID == "BUNYALA NORTH", 3,
                                          ifelse(InputID == "BUNYALA SOUTH", 4,
                                                 ifelse(InputID == "BUNYALA WEST", 4,
                                                        ifelse(InputID == "BWIRI", 5,
                                                               ifelse(InputID == "NAMBOBOTO NAMBUKU", 7,
                                                                      ifelse(InputID == "NANGINA", 6, NA)
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            )
  )
  ) %>% 
  select(-InputID, -TargetID) %>% 
  group_by(join_zone) %>% 
  summarise(distance = mean(Distance))

site_vars <- site_vars_grp13 %>% 
  mutate(join_zone = ifelse(zoneid == 8, 7, zoneid)) %>% 
  left_join(., distances, by = "join_zone")

# Decide threshold for convenience
# site_vars %>% group_by(join_zone, t1) 
# %>% summarise(distance = first(distance), count = n()) 
# %>% arrange(desc(distance))

site_vars <- site_vars %>% 
  # threshold is distance greater than 31012.58
  mutate(n1_dist = ifelse(distance >= 30000, 1, 0))

n1 <- 22

# Simulation begins



compare_4_all <- NULL
compare_4_wt_all <- NULL

# geq_9 is dummy for whether site falls in convenience.

# Simulation 1:  Convenience sampling road networks
for (i in seq(.9, stop_perc_calc(n1), -.1)) {
  simulation_results4 <- NULL
  sim4_weighted <- NULL
  for (k in 1:n_sim){
    site_vars_1 <- site_vars %>% 
      mutate(incl_prob = ifelse(n1_dist == 1, i*20/n1, (20-i*20) / (50 - n1))) %>% 
      mutate(in_sample = rbinom(n = n(), size = 1, prob = incl_prob)) %>% 
      select(sch98v1, in_sample, incl_prob, treat_effect)
    pupils_grp <- pupils_grp13 %>% 
      left_join(., site_vars_1, by = "sch98v1")
    model_4 <- pupils_grp %>%
      lm(prs ~ t1 + elg98 + p1 + mk96_s + sap_any + Istd1 + Istd2 
         + Istd3 + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
         + Isem1 + Isem2 + pop_3km_updated + popT_3km_updated 
         + pop_36k_updated + popT_36k_updated, weights = obs, data = ., subset = (in_sample == 1)) %>%
      tidy() %>% 
      filter(grepl("t1", term))
    
    model_4_wt <- pupils_grp %>%
      mutate(wt_obs = (1/incl_prob) * obs) %>% 
      lm(prs ~ t1 + elg98 + p1 + mk96_s + Y98sap1 + Y98sap2 + Y98sap3 + Y98sap4 + Istd1 
         + Istd2 + Istd3 + Istd4 + Istd5 + Istd6 + Istd7 + Istd8 + Istd9
         + Isem1 + Isem2 + Isem3 + pop_3km_updated + popT_3km_updated + pop_36k_updated 
         + popT_36k_updated, weights = wt_obs, data = ., subset = (in_sample == 1)) %>%
      tidy() %>% 
      filter(grepl("t1", term)) %>% 
      rename(wt_estimate=estimate, wt_se=std.error, wt_tstat = statistic, wt_pval=p.value)
    
    ID_variables <- data.frame(iteration = k)
    simulation_results4 <- simulation_results4 %>%
      bind_rows(bind_cols(ID_variables, model_4, model_4_wt))

  }
  compare_4 <- simulation_results4 %>% 
    mutate(emp_bias = estimate - true, se_bias = sd(emp_bias)/10) %>% 
    summarise(estimate = mean(estimate), p.value = mean(p.value),
              emp_bias = mean(emp_bias), se_bias = mean(se_bias)) %>% 
    mutate(ci_lower = emp_bias - 1.96*se_bias, ci_upper = emp_bias + 1.96*se_bias)
    
  
  compare_4_wt <- simulation_results4 %>% 
    mutate(diff = abs(estimate - wt_estimate), se_diff = sd(diff)/10) %>%
    summarise(wt_estimate = mean(wt_estimate), wt_pval = mean(wt_pval),
              diff = mean(diff), se_diff = mean(se_diff)) %>% 
    mutate(ci_lower = diff - 1.96*se_diff, ci_upper = diff + 1.96*se_diff)

  
  cv_p4 <- sd(site_vars_1$incl_prob, na.rm=TRUE)/ mean(site_vars_1$incl_prob, na.rm=TRUE)*100
  var_te <- var(site_vars_1$treat_effect, na.rm=TRUE)
  corr_p4_te <- cor(site_vars_1$incl_prob, site_vars_1$treat_effect)
  bias4 <- - cv_p4 * var_te * corr_p4_te
  
  
  ID_variables4_nw <- data.frame(iteration = i, type = "Not weighted")
  ID_variables4_w <- data.frame(iteration = i, type = "Weighted")
  
  compare_4_all <- compare_4_all %>%
    bind_rows(bind_cols(ID_variables4_nw, compare_4[1,], data.frame(bias4)))
  
  compare_4_wt_all <- compare_4_wt_all %>%
    bind_rows(bind_cols(ID_variables4_w, compare_4_wt[1,], data.frame(bias4)))
}

compare_4_all <- compare_4_all %>% 
  rename(diff=emp_bias, se_diff=se_bias)

compare_4_wt_all <- compare_4_wt_all %>% 
  rename(estimate=wt_estimate, p.value=wt_pval)

data_viz4 <- bind_rows(compare_4_all, compare_4_wt_all) %>% 
  mutate(empirical_bias = estimate - 0.04666452)

wt_vs_unwt <- compare_4_wt_all %>% 
  ggplot(data = ., aes(x=iteration, y=diff)) + 
  geom_line() +
  geom_vline(xintercept=0.44, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, linetype="dashed") +
  annotate("text", x = 0.42, y = 0.015, angle = 90, label = "Population proportion") +
  labs(y = "Decrease in Bias", x = "Proportion of convenient sites in the population"
       , title = "Bias Reduction due to Reweighting") +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))

ggsave("figures/final/weightedBias_dist.png", width=7, height=4.5)

predicted_vs_empirical <- data_viz4 %>% 
  filter(type == "Not weighted") %>% 
  ggplot(data = ., aes(x = iteration)) + 
  geom_line(aes(y = bias_updated, colour = "Predicted bias")) + 
  geom_line(aes(y = diff, colour = "Empirical bias")) +
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.02, color="red", linetype = "dashed") +
  geom_vline(xintercept=0.44, linetype="dotted") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept= -true, linetype="dotted") +
  annotate("text", x = 0.42, y = -0.03, angle = 90, label = "Population proportion") +
  annotate("text", x = 0.595, y = -0.045, label = "-(Population parameter of impact)") +
  labs(y = "Bias", x = "Proportion of convenient sites in the population"
       , title = "Effect of Weighting by Inverse Inclusion Probability") +
  theme_minimal() +
  theme(legend.title=element_blank(), text = element_text(size=15)
        , axis.text = element_text(face = "bold"))

ggsave("figures/final/predictedBias_dist.png", width=7, height=4.5)
