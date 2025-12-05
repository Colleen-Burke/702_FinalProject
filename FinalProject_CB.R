library(tidyverse)
library(haven)
library(janitor)
library(broom)
library(DescTools)

# Load dataset
wave33 <- read_sav("ATP W33.sav")

#--- Data Cleaning ---
# Recode variables
analysis_df <- wave33 %>%
  mutate(
    # Predictor: personal impact of climate change (CLIM11_W33)
    climate_impact = case_when(
      CLIM11_W33 == 1 ~ 1,   # Yes, impacted personally
      CLIM11_W33 == 2 ~ 0,   # No, not impacted
      TRUE ~ NA_real_
    ),
    
    # Outcome: belief that regulations cannot be cut (ENVIR7_W33)
    envreg_support = case_when(
      ENVIR7_W33 == 2 ~ 1,   # NOT possible to cut regs (supports strong regulation)
      ENVIR7_W33 == 1 ~ 0,   # YES possible to cut regs (less support)
      TRUE ~ NA_real_
    ),
    
    # Stratifier: climate belief (CLIM1A_W33)
    clim_belief = case_when(
      CLIM1A_W33 == 1 ~ "Human",
      CLIM1A_W33 == 2 ~ "Natural",
      CLIM1A_W33 == 3 ~ "NoEvidence",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    clim_belief = factor(clim_belief, 
                         levels = c("Human", "Natural", "NoEvidence"))
  ) %>%
  drop_na(climate_impact, envreg_support, clim_belief)


#--- 2x2 table and chi-square test ---
table2x2 <- table(analysis_df$climate_impact, analysis_df$envreg_support)
table2x2

chisq_res <- chisq.test(table2x2)
chisq_res


#--- Logistic Regression ---
logit_mod <- glm(envreg_support ~ climate_impact,
                 data = analysis_df,
                 family = binomial)

tidy(logit_mod, exponentiate = TRUE, conf.int = TRUE)


#--- CMH Test ---
table3d <- xtabs(~ climate_impact + envreg_support + clim_belief,
                 data = analysis_df)

mantelhaen.test(table3d)


#--- Breslow-Day test for heterogeneity ---
BreslowDayTest(table3d)



#-- Stratum-specific OR ---
analysis_df %>%
  group_split(clim_belief) %>%
  map(~ epitools::oddsratio(table(.x$climate_impact, .x$envreg_support)))
