library(tidyverse)
library(haven)
library(broom)
library(DescTools)
library(epitools)

# Load Data --------------------------------------------------------------------
wave33 <- read_sav("ATP W33.sav")


# Data Cleaning ----------------------------------------------------------------
analysis_df <- wave33 %>%
  mutate(
    # Predictor: Personal climate change impact (1=Yes, 0=No)
    climate_impact = case_when(
      CLIM11_W33 == 1 ~ 1,
      CLIM11_W33 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Outcome: Support for strong environmental regulations (1=Cannot cut, 0=Can cut)
    envreg_support = case_when(
      ENVIR7_W33 == 2 ~ 1,
      ENVIR7_W33 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Stratifier: Climate change belief
    clim_belief = case_when(
      CLIM1A_W33 == 1 ~ "Human",
      CLIM1A_W33 == 2 ~ "Natural",
      CLIM1A_W33 == 3 ~ "NoEvidence",
      TRUE ~ NA_character_
    ),
    clim_belief = factor(clim_belief, levels = c("Human", "Natural", "NoEvidence"))
  ) %>%
  drop_na(climate_impact, envreg_support, clim_belief)


# Descriptive Statistics -------------------------------------------------------
nrow(analysis_df)  # Sample size
table(analysis_df$climate_impact)
table(analysis_df$envreg_support)
table(analysis_df$clim_belief)


# Output 1: Chi-Square Test ----------------------------------------------------
table2x2 <- table(analysis_df$climate_impact, analysis_df$envreg_support)
rownames(table2x2) <- c("Not Impacted", "Impacted")
colnames(table2x2) <- c("Can Cut Regs", "Cannot Cut Regs")

table2x2
prop.table(table2x2, margin = 1)  # Row percentages

chisq_result <- chisq.test(table2x2, correct = FALSE)
chisq_result


# Output 2: Logistic Regression ------------------------------------------------
logit_mod <- glm(envreg_support ~ climate_impact,
                 data = analysis_df,
                 family = binomial)

tidy(logit_mod, exponentiate = TRUE, conf.int = TRUE)


# Output 3: Cochran-Mantel-Haenszel Test ---------------------------------------
table3d <- xtabs(~ climate_impact + envreg_support + clim_belief,
                 data = analysis_df)

table3d
cmh_result <- mantelhaen.test(table3d, correct = FALSE)
cmh_result


# Output 4: Breslow-Day Test ---------------------------------------------------
bd_result <- BreslowDayTest(table3d)
bd_result


#-- Stratum-specific OR ---
analysis_df %>%
  group_by(clim_belief) %>%
  group_map(~ {
    tab <- table(.x$climate_impact, .x$envreg_support)
    or <- oddsratio(tab, method = "wald")
    or
  })
