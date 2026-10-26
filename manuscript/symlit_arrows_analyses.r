 #           _____  _____   ______          _______ 
 #     /\   |  __ \|  __ \ / __ \ \        / / ____|
 #    /  \  | |__) | |__) | |  | \ \  /\  / / (___  
 #   / /\ \ |  _  /|  _  /| |  | |\ \/  \/ / \___ \ 
 #  / ____ \| | \ \| | \ \| |__| | \  /\  /  ____) |
 # /_/    \_\_|  \_\_|  \_\\____/   \/  \/  |_____/ 
 #                                                  
 # SYMLIT ARROWS - GK 10/2026              

# READ ME

# This script creates all output necessary for inserting 
# tables, in-text reported valuses and illustrations 
# in to the manuscript and supplement
# All tables and values are saved in the "results" folder, all figures in "illustrations"
# and when knitting the markdown and sourced from there

# Load packages ----------------------------

# Data Wrangling
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
# Conventional Analyses
if (!require(lsr)) install.packages('lsr'); library(lsr) # Analysis, Cohen's D
if (!require(scales)) install.packages('scales'); library(scales) # 
# bayes
if (!require(brms)) install.packages('brms'); library(brms) # 
if (!require(tidybayes)) install.packages('tidybayes'); library(tidybayes) # 
if (!require(HDInterval)) install.packages('HDInterval'); library(HDInterval) # 
if (!require(posterior)) install.packages('posterior'); library(posterior) # 
# reliability
if (!require(irr)) install.packages('irr'); library(irr) # 
# plotting and illustrations
if (!require(patchwork)) install.packages('patchwork'); library(patchwork) # 
if (!require(magick)) install.packages('magick'); library(magick) # 
if (!require(cowplot)) install.packages('cowplot'); library(cowplot) # 
if (!require(png)) install.packages('png'); library(png) # 
if (!require(grid)) install.packages('grid'); library(grid) # 
if (!require(ggthemes)) install.packages('ggthemes'); library(ggthemes) 
if (!require(ggview)) install.packages('ggview'); library(ggview) # previewing
if (!require(ggtext)) install.packages('ggtext'); library(ggtext) # previewing
# Tables
if (!require(flextable)) install.packages('flextable'); library(flextable) 
if (!require(knitr)) install.packages('knitr'); library(knitr) # 
if (!require(kableExtra)) install.packages('kableExtra'); library(kableExtra) # 

# Read data ----------------------------

symlit.arrows.data <- readRDS("../data/symlit_arrows_data.rds") 

# Quality Control -------------------------------

# (at least 8 trials per participant) --> no problems
test_8trials <- symlit.arrows.data %>%
  filter(sample != "drop") %>%
  filter(trialtype != "fam") %>%
  filter(valid == 1) %>% 
  group_by(sample, study, subid) %>%
  summarize(sumtrials = n(),
            maxtrials = max(as.integer(trial))) %>%
  filter(sumtrials < 8)

# (75% familiarization correct --> no problems
test_fam <- symlit.arrows.data %>%
  filter(sample != "drop") %>%
  filter(trialtype == "fam") %>%
  filter(valid == 1) %>% 
  filter(!is.na(correct)) %>%
  mutate(correct = as.numeric(correct)) %>% 
  group_by(study, task, subid) %>%
  summarize(correct = round(mean(correct)*100)) %>%
  filter(correct < 66) %>%
  mutate(valid = as.character(0),
         drop = "fail_in_fam")

n_distinct(test_fam$subid)
n_distinct(test_8trials$subid)

rm(test_fam, test_8trials)

# All Participants contribute at least 8 Test Trials (8/16 50%)
# All participants in study 3 contribute at least 10 trials  (10/20 50%)
# All Participants Passed Familiarization 

# BAYES --------------------------------------------

## prepare data -------------------------------

S1.data.bm <- symlit.arrows.data %>%
  filter(study == "study1") %>% 
  filter(sample == "valid") %>% 
  filter(valid == "1") %>% 
  filter(trialtype == "test") %>%
  mutate(
    # Reference: marker
    task = factor(task, levels = c("marker", "arrow")), 
    # Reference: 0 = female
    sex  = factor(sex,  levels = c("0", "1")) ) %>% 
  mutate(
    ageinyears = aged / 365.25,
    # Alter in Jahren, zentriert
    c.age      = ageinyears - mean(ageinyears, na.rm = TRUE),
    # Trial-Nummer, zentriert
    c.trial    = trial - mean(trial, na.rm = TRUE)) %>% 
  select(task, subid, sex, aged, agem, ageinyears, correct, trial, Condition, c.age, c.trial)

S2.data.bm <- symlit.arrows.data %>%
  filter(study == "study2") %>% 
  filter(sample == "valid") %>% 
  filter(valid == "1") %>% 
  filter(trialtype == "test") %>%
  mutate(
    # Reference: congruent
    task = factor(task, levels = c("congruent", "incongruent")), 
    # Reference: 0 = female
    sex  = factor(sex,  levels = c("0", "1")) ) %>% 
  mutate(
    ageinyears = aged / 365.25,
    # Alter in Jahren, zentriert
    c.age      = ageinyears - mean(ageinyears, na.rm = TRUE),
    # Trial-Nummer, zentriert
    c.trial    = trial - mean(trial, na.rm = TRUE)) %>% 
  select(task, subid, sex, aged, agem, ageinyears, correct, trial, Condition, c.age, c.trial)


S3.data.bm <- symlit.arrows.data %>%
  filter(study == "study3") %>% 
  filter(sample == "valid") %>% 
  filter(valid == "1") %>% 
  filter(trialtype == "test") %>%
  mutate(
    # Reference: 0 = female
    sex  = factor(sex,  levels = c("0", "1")) ) %>% 
  mutate(
    ageinyears = aged / 365.25,
    # age in years, centered
    c.age      = ageinyears - mean(ageinyears, na.rm = TRUE),
    # Trial, centered
    c.trial    = trial - mean(trial, na.rm = TRUE)) %>% 
  select(task, subid, sex, aged, agem, ageinyears, correct, trial, Condition, c.age, c.trial)

## RUN & SAVE ALL MODELS ----------------------------------

# /$$$$$$                                      /$$$$$$$$ /$$                        
# /$$__  $$                                    |__  $$__/|__/                        
# | $$  \__/  /$$$$$$  /$$    /$$ /$$$$$$          | $$    /$$ /$$$$$$/$$$$   /$$$$$$ 
# |  $$$$$$  |____  $$|  $$  /$$//$$__  $$         | $$   | $$| $$_  $$_  $$ /$$__  $$
# \____  $$  /$$$$$$$ \  $$/$$/| $$$$$$$$         | $$   | $$| $$ \ $$ \ $$| $$$$$$$$
# /$$  \ $$ /$$__  $$  \  $$$/ | $$_____/         | $$   | $$| $$ | $$ | $$| $$_____/
# |  $$$$$$/|  $$$$$$$   \  $/  |  $$$$$$$         | $$   | $$| $$ | $$ | $$|  $$$$$$$
# \______/  \_______/    \_/    \_______/         |__/   |__/|__/ |__/ |__/ \_______/

# Running all models may take 30-60min depending on your machine
# All model outputs are also stored in the online repository so you can just load them

### S1 full and null model ---------------------------------------------

# preregistered: correct ~ task*z.age +z.trial +z.sex +(z.trial|id)
# we have decided to not scale sex to make interpretation more straightforward

### FULL
S1.full.bm <- brm(
  correct ~ task * c.age + c.trial + sex + (c.trial | subid),
  data   = S1.data.bm,
  family = bernoulli(),
  chains = 4,
  iter   = 5000,
  cores  = 4)
S1.full.bm <- add_criterion(S1.full.bm, c("loo", "waic"))

### NULL
S1.null.bm <- brm(
  correct ~ task + c.age + c.trial + sex + (c.trial | subid),
  data   = S1.data.bm,
  family = bernoulli(),
  chains = 4,
  iter   = 5000,
  cores  = 4)
S1.null.bm <- add_criterion(S1.null.bm, c("loo", "waic"))

### MODEL COMPARISON
S1.comp <- loo_compare(S1.null.bm, S1.full.bm, criterion = "waic")%>%as_tibble(rownames = "index")
S1.comp <- data.frame(S1.comp, row.names = "index")

S1.weights <- model_weights(S1.null.bm, S1.full.bm, criterion = "waic")
S1.weights <- S1.weights %>% as_tibble(rownames = "index")
S1.weights <- data.frame(S1.weights, row.names = "index")

### LOO COMPARISON
### essentially identical
S1.null.loo <- loo(S1.null.bm)
S1.full.loo <- loo(S1.full.bm)
S1.loo.comp <- loo_compare(S1.null.loo, S1.full.loo) %>%
  as_tibble(rownames = "model")

# SAVING
saveRDS(S1.full.bm, "../models/S1.full.bm.rds")
saveRDS(S1.null.bm, "../models/S1.null.bm.rds")

saveRDS(S1.comp, "../models/S1.comp.rds")
saveRDS(S1.comp, "../results/S1.comp.rds")

saveRDS(S1.weights, "../models/S1.weights.rds")
saveRDS(S1.weights, "../results/S1.weights.rds")

saveRDS(S1.loo.comp, "../models/S1.loo.comp.rds")
saveRDS(S1.loo.comp, "../results/S1.loo.comp.rds")

### S2 full and null model ---------------------------------------------

# preregistered: correct ~ task*z.age +z.trial +z.sex +(z.trial|id)
# we have now decided to not scale sex to make interpretation more straightforward

### FULL
S2.full.bm <- brm(
  correct ~ task * c.age + c.trial + sex + (c.trial | subid),
  data   = S2.data.bm,
  family = bernoulli(),
  chains = 4,
  iter   = 5000,
  cores  = 4)
S2.full.bm <- add_criterion(S2.full.bm, c("loo", "waic"))

### NULL
S2.null.bm <- brm(
  correct ~ task + c.age + c.trial + sex + (c.trial | subid),
  data   = S2.data.bm,
  family = bernoulli(),
  chains = 4,
  iter   = 5000,
  cores  = 4)
S2.null.bm <- add_criterion(S2.null.bm, c("loo", "waic"))

### MODEL COMPARISON
S2.comp <- loo_compare(S2.null.bm, S2.full.bm, criterion = "waic")%>%as_tibble(rownames = "index")
S2.comp <- data.frame(S2.comp, row.names = "index")

S2.weights <- model_weights(S2.null.bm, S2.full.bm, criterion = "waic")
S2.weights <- S2.weights %>% as_tibble(rownames = "index")
S2.weights <- data.frame(S2.weights, row.names = "index")

### LOO COMPARISON
### essentially identical
S2.null.loo <- loo(S2.null.bm)
S2.full.loo <- loo(S2.full.bm)
S2.loo.comp <- loo_compare(S2.null.loo, S2.full.loo) %>%
  as_tibble(rownames = "model")

# SAVING
saveRDS(S2.full.bm, "../models/S2.full.bm.rds")
saveRDS(S2.null.bm, "../models/S2.null.bm.rds")

saveRDS(S2.comp, "../models/S2.comp.rds")
saveRDS(S2.comp, "../results/S2.comp.rds")

saveRDS(S2.weights, "../models/S2.weights.rds")
saveRDS(S2.weights, "../results/S2.weights.rds")

saveRDS(S2.loo.comp, "../models/S2.loo.comp.rds")
saveRDS(S2.loo.comp, "../results/S2.loo.comp.rds")

### S3 full and null model ---------------------------------------------

# models specification in preregistration is incorrect. 
# Condition should not be in there in line with the reasoning in the text
# Model: condition*z.age +z.trial +z.sex +(age|itemtype)+(z.trial|id)

# FINAL VERSION USED HERE: 
# correct ~ c.age +c.trial + sex +(c.age|task)+(c.trial|subid)

### FULL
S3.full.bm <- brm(
  correct ~ c.age + c.trial + sex + (c.age | task) + (c.trial | subid),
  data   = S3.data.bm,
  family = bernoulli(),
  chains = 4,
  iter   = 5000,
  cores  = 4)
S3.full.bm <- add_criterion(S3.full.bm, c("loo", "waic"))

### NULL
S3.null.bm <- brm(
  correct ~ c.age + c.trial + sex + (c.trial | subid),
  data   = S3.data.bm,
  family = bernoulli(),
  chains = 4,
  iter   = 5000,
  cores  = 4)
S3.null.bm <- add_criterion(S3.null.bm, c("loo", "waic"))

### MODEL COMPARISON
S3.comp <- loo_compare(S3.null.bm, S3.full.bm, criterion = "waic")%>%as_tibble(rownames = "index")
S3.comp <- data.frame(S3.comp, row.names = "index")

S3.weights <- model_weights(S3.null.bm, S3.full.bm, criterion = "waic")
S3.weights <- S3.weights %>% as_tibble(rownames = "index")
S3.weights <- data.frame(S3.weights, row.names = "index")

### LOO COMPARISON
S3.null.loo <- loo(S3.null.bm)
S3.full.loo <- loo(S3.full.bm)
S3.loo.comp <- loo_compare(S3.null.loo, S3.full.loo) %>%
  as_tibble(rownames = "model")

# SAVING
saveRDS(S3.full.bm, "../models/S3.full.bm.rds")
saveRDS(S3.null.bm, "../models/S3.null.bm.rds")

saveRDS(S3.comp, "../models/S3.comp.rds")
saveRDS(S3.comp, "../results/S3.comp.rds")

saveRDS(S3.weights, "../models/S3.weights.rds")
saveRDS(S3.weights, "../results/S3.weights.rds")

saveRDS(S3.loo.comp, "../models/S3.loo.comp.rds")
saveRDS(S3.loo.comp, "../results/S3.loo.comp.rds")

### S3 exploratory model -------------------

# Variation of the null/reduced model including task (i.e. cue type) as fixed effect
S3task.null.bm<-brm(correct~task+c.age+c.trial+sex+(c.trial|subid),
                    data= S3.data.bm,
                    family=bernoulli(),
                    chains = 4,
                    iter= 5000,
                    cores= 4)
S3task.null.bm <- add_criterion(S3task.null.bm, c("loo", "waic"))

### MODEL COMPARISON Exploratory vs Reduced

S3task.comp.task.null <- loo_compare(S3task.null.bm, S3.null.bm, criterion = "waic")%>%as_tibble(rownames = "index")
S3task.comp.task.null <- data.frame(S3task.comp.task.null, row.names = "index")

S3task.weights.task.null <- model_weights(S3task.null.bm, S3.null.bm, criterion = "waic")
S3task.weights.task.null <- S3task.weights.task.null %>% as_tibble(rownames = "index")
S3task.weights.task.null <- data.frame(S3task.weights.task.null, row.names = "index")

# SAVE MODELS
saveRDS(S3task.null.bm, "../models/S3task.null.bm.rds")

saveRDS(S3task.weights.task.null, "../models/S3task.weights.task.null.rds")
saveRDS(S3task.weights.task.null, "../results/S3task.weights.task.null.rds")

saveRDS(S3task.comp.task.null, "../models/S3task.comp.task.null.rds")
saveRDS(S3task.comp.task.null, "../results/S3task.comp.task.null.rds")


# Study 1 ------------------------------

## S1 Read Models and Comparisons --------------
S1.full.bm <- readRDS("../models/S1.full.bm.rds")
S1.null.bm <- readRDS("../models/S1.null.bm.rds")
S1.comp <- readRDS("../models/S1.comp.rds")
S1.weights <- readRDS("../models/S1.weights.rds")
S1.loo.comp <- readRDS("../models/S1.loo.comp.rds")

## S1 ppc ess etc ----------

## Check plots ...very good
pp_check(S1.full.bm)
pp_check(S1.null.bm)
pp_check(S1.full.bm, type = "bars")
pp_check(S1.null.bm, type = "bars")

### EFFECTICE SAMPLE SIZES
### inspect rhat --> always 1 --> no problems
### inspect Bull_ESS --> always > 1000 --> no problems
summary(S1.full.bm)
summary(S1.null.bm)

# for reporting BULK_ESS and Coefficients
S1.full.coef <- summarise_draws(as_draws_df(S1.full.bm)) 

# NOTE: # as_draws_df() from posterior uses 90% CrIs
# we preregistered using 2.5% and 97.5% quantiles
draws_df <- as_draws_df(S1.full.bm)
quantiles_df <- as.data.frame(t(apply(draws_df, 2, quantile, probs = c(0.025, 0.975))))
quantiles_df <- tibble::rownames_to_column(quantiles_df, var = "variable")
names(quantiles_df)[2:3] <- c("q2.5", "q97.5")

S1.full.coef <- left_join(S1.full.coef, quantiles_df, by = "variable") 

S1.full.coef.table <- S1.full.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  mutate(variable = gsub("task", "", variable)) %>% 
  mutate(variable = gsub("([^ ])([A-Z])", "\\1 \\2", variable)) %>% 
  mutate(variable = gsub(":", " × ", variable)) %>% 
  mutate(variable = gsub("c.age", "Age*", variable)) %>% 
  mutate(variable = gsub("c.trial", "Trial*", variable)) %>% 
  mutate(variable = gsub("sex1", "Sex (Male)", variable)) %>%
  mutate(variable = str_to_sentence(variable)) %>% 
  rename(
    Predictor = variable,
    Estimate = mean,
    SD = sd,
    MAD = mad,
    `2.5% CrI` = q2.5, 
    `97.5% CrI` = q97.5, 
    `Bulk ESS` = ess_bulk,
    `Tail ESS` = ess_tail) %>%
  mutate(
    `95% CrI` = paste0("[", sprintf("%.2f", `2.5% CrI`), ", ", sprintf("%.2f", `97.5% CrI`), "]"),
    `Bulk ESS` = format(round(`Bulk ESS`), big.mark = ","),
    `Tail ESS` = format(round(`Tail ESS`), big.mark = ",")) %>%
  select(Predictor, Estimate, SD, MAD, `95% CrI`, `Bulk ESS`, `Tail ESS`)

# save for table
write_rds(S1.full.coef.table, "../results/S1.full.coef.table.rds")

# for reporting in text:
S1.full.coef <- S1.full.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  mutate(variable = gsub("task", "", variable)) %>% 
  data.frame(row.names = "variable")

# save same table for in-text reporting (first column as row names)
write_rds(S1.full.coef, "../results/S1.full.coef.rds")


S1.null.coef <- summarise_draws(as_draws_df(S1.null.bm)) %>%
  filter(grepl("^b_", variable))

# save null model coefficients
write_rds(S1.null.coef, "../results/S1.null.coef.rds")


## S1 prep Plot -------------

nd1 <- tibble(c.age = rep(seq(from = min(S1.data.bm$c.age),
                              to = max(S1.data.bm$c.age), length.out = 50),2),
              task = c(rep("marker",50), 
                       rep("arrow",50)), # the two conditions in the data
              sex = rep(0,100),
              c.trial = rep(0,100)) %>% 
  mutate(task = factor(task, levels = c("marker", "arrow")))


f1 <- fitted(S1.full.bm, 
             newdata = nd1, 
             re_formula = NA) %>% 
  as_tibble() %>%
  bind_cols(nd1)%>%
  mutate(age = c.age + mean(S1.data.bm$ageinyears)) %>% # convert age back to the original scale by adding the mean of the data
  mutate(task = factor(task, levels = c("marker", "arrow")))

d1 <- S1.data.bm%>%
  group_by(subid, ageinyears, task)%>%
  summarise(mean = mean(correct)) %>% 
  mutate(task = factor(task, levels = c("marker", "arrow")))

p1 <- f1 %>%
  group_by(task) %>%
  summarise(
    # Interpolate exact age (in years) where lower bound Q2.5 crosses 0.5
    age_cross = approx(x = Q2.5, y = age, xout = 0.5)$y,
    days = age_cross * 365.25,
    months = round(days / 30.5),
    years = round(age_cross, 2))

## S1 Plot ------------------------

# Basic PLot Basic PLot Basic Plot
img_labels <- c(
  "marker" = "<img src='../illustrations_raw/legend_S1_Marker.png' width='100'/>",
  "arrow"  = "<img src='../illustrations_raw/legend_S1_Arrow.png' width='100'/>"
)

S1_plot <- ggplot() +
  geom_hline(yintercept = .5, lty = 2, alpha = .75) +
  
  # Individual performance (Overall)
  geom_point(data = d1, aes(x = ageinyears, y = mean, color = task), alpha = .35, shape = 1) +
  
  # CrI
  geom_ribbon(data = f1, aes(x = age, ymin = Q2.5, ymax = Q97.5, fill = task), alpha = .18) +
  
  # Mean
  geom_line(data = f1, aes(x = age, y = Estimate, col = task), linewidth = 1, alpha = .5) +
  
  # Highlight Threshold Points
  geom_point(data = p1 %>% filter(task == "marker"), 
             aes(x = round(days / 365.25, 1), y = .51, color = task), size = 4) +
  geom_point(data = p1, aes(x = round(days / 365.25, 1), y = .49, color = task), size = 4) +
  
  # Highlight Threshold Text (Color fixed outside aes)
  geom_text(data = p1, aes(label = months, x = round(days / 365.25, 1), y = .23), 
            color = "#318ce7", angle = 90, size = 5, fontface = "bold") +
  geom_text(data = p1, aes(label = "months", x = round(days / 365.25, 1), y = .36), 
            color = "#318ce7", angle = 90, size = 5, fontface = "bold") +
  
  theme_minimal() +
  
  scale_x_continuous(limits = c(2, 5), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.05)) +
  
  scale_color_discrete(labels = img_labels) +
  scale_fill_discrete(labels = img_labels) +
  
  guides(
    color = guide_legend(ncol = 1, byrow = TRUE, title.position = "top"),
    fill = "none"
  ) +
  labs(
    x = "Age in Years", 
    y = "Proportion Correct",
    color = "Condition", 
    fill  = "Condition"
  ) +
  theme(
    legend.position = c(0.875, 0.185),
    legend.background = element_rect(fill = alpha("white", 1), 
                                     color = "black", linewidth = 0.5),
    legend.key.height = unit(1, "cm"), 
    legend.key.width  = unit(1, "cm"),
    legend.title = element_text(size = 11, face = "bold", vjust = 0.5),
    legend.text  = element_markdown(hjust = 0.5, vjust = 0.5)
  )

S1_plot + canvas(width = 27, height = 12, units = "cm", dpi = 600)

ggsave(
  filename = "../illustrations/S1_plot.svg",
  plot = S1_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "../illustrations/S1_plot.png",
  plot = S1_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

# Study 2 ----------------------------

## S2 Read Models and Comparisons -------------
S2.full.bm <- readRDS("../models/S2.full.bm.rds")
S2.null.bm <- readRDS("../models/S2.null.bm.rds")
S2.comp <- readRDS("../models/S2.comp.rds")
S2.weights <- readRDS("../models/S2.weights.rds")
S2.loo.comp <- readRDS("../models/S2.loo.comp.rds")

## S2 ppc ess etc ----------

## Check plots
pp_check(S2.full.bm)
pp_check(S2.null.bm)
pp_check(S2.full.bm, type = "bars")
pp_check(S2.null.bm, type = "bars")
 
### EFFECTICE SAMPLE SIZES
### inspect rhat --> always 1 --> no problems
### inspect Bulk_ESS --> always > 1000 --> no problems
summary(S2.full.bm)
summary(S2.null.bm)

# for reporting BULK_ESS and Coefficients
S2.full.coef <- summarise_draws(as_draws_df(S2.full.bm)) # ! standard is just 90% CrI

# NOTE: # as_draws_df() from posterior uses 90% CrIs
# we preregistered using 2.5% and 97.5% quantiles
draws_df <- as_draws_df(S2.full.bm)
quantiles_df <- as.data.frame(t(apply(draws_df, 2, quantile, probs = c(0.025, 0.975))))
quantiles_df <- tibble::rownames_to_column(quantiles_df, var = "variable")
names(quantiles_df)[2:3] <- c("q2.5", "q97.5")

S2.full.coef <- left_join(S2.full.coef, quantiles_df, by = "variable") 

S2.full.coef.table <- S2.full.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  mutate(variable = gsub("task", "", variable)) %>% 
  mutate(variable = gsub("([^ ])([A-Z])", "\\1 \\2", variable)) %>% 
  mutate(variable = gsub(":", " × ", variable)) %>% 
  mutate(variable = gsub("c.age", "Age*", variable)) %>% 
  mutate(variable = gsub("c.trial", "Trial*", variable)) %>% 
  mutate(variable = gsub("sex1", "Sex (Male)", variable)) %>%
  mutate(variable = str_to_sentence(variable)) %>% 
  rename(
    Predictor = variable,
    Estimate = mean,
    SD = sd,
    MAD = mad,
    `2.5% CrI` = q2.5, 
    `97.5% CrI` = q97.5, 
    `Bulk ESS` = ess_bulk,
    `Tail ESS` = ess_tail) %>%
  mutate(
    `95% CrI` = paste0("[", sprintf("%.2f", `2.5% CrI`), ", ", sprintf("%.2f", `97.5% CrI`), "]"),
    `Bulk ESS` = format(round(`Bulk ESS`), big.mark = ","),
    `Tail ESS` = format(round(`Tail ESS`), big.mark = ",")) %>%
  select(Predictor, Estimate, SD, MAD, `95% CrI`, `Bulk ESS`, `Tail ESS`)

write_rds(S2.full.coef.table, "../results/S2.full.coef.table.rds")

# for reporting in text:
S2.full.coef <- S2.full.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  mutate(variable = gsub("task", "", variable)) %>% 
  data.frame(row.names = "variable")

write_rds(S2.full.coef, "../results/S2.full.coef.rds")



S2.null.coef <- summarise_draws(as_draws_df(S2.null.bm)) %>%
  filter(grepl("^b_", variable))

write_rds(S2.null.coef, "../results/S2.null.coef.rds")

## S2 prep Plot -------------

nd2 <- tibble(c.age = rep(seq(from = min(S2.data.bm$c.age),
                              to = max(S2.data.bm$c.age), length.out = 50),2),
              task = c(rep("congruent",50), 
                       rep("incongruent",50)), # the two conditions in the data
              sex = rep(0,100),
              c.trial = rep(0,100)) %>% 
  mutate(task = factor(task, levels = c("congruent", "incongruent")))


f2 <- fitted(S2.full.bm, 
             newdata = nd2, 
             re_formula = NA) %>% 
  as_tibble() %>%
  bind_cols(nd2)%>%
  mutate(age = c.age + mean(S2.data.bm$ageinyears)) %>% # convert age back to the original scale by adding the mean of the data
  mutate(task = factor(task, levels = c("congruent", "incongruent")))

d2 <- S2.data.bm%>%
  group_by(subid, ageinyears, task)%>%
  summarise(mean = mean(correct)) %>% 
  mutate(task = factor(task, levels = c("congruent", "incongruent")))

p2 <- f2 %>%
  group_by(task) %>%
  summarise(
    # Interpolate exact age (in years) where lower bound Q2.5 crosses 0.5
    age_cross = approx(x = Q2.5, y = age, xout = 0.5)$y,
    days = age_cross * 365.25,
    months = round(days / 30.5),
    years = round(age_cross, 2))


## S2 Plot ------------------------

img_labels <- c(
  "congruent"   = "<img src='../illustrations_raw/legend_S2_Congruent.png' width='100'/>",
  "incongruent" = "<img src='../illustrations_raw/legend_S2_Incongruent.png' width='100'/>")

S2_plot <- ggplot() +
  geom_hline(yintercept = .5, lty = 2, alpha = .75) +
  
  # Individual performance (Overall)
  geom_point(data = d2, aes(x = ageinyears, y = mean, color = task), alpha = .35, shape = 1) +
  
  # CrI
  geom_ribbon(data = f2, aes(x = age, ymin = Q2.5, ymax = Q97.5, fill = task), alpha = .18) +
  
  # Mean
  geom_line(data = f2, aes(x = age, y = Estimate, col = task), linewidth = 1, alpha = .5) +
  
  # Highlight Threshold
  geom_point(data = p2, aes(x = round(days / 365.25, 2), y = .5, color = task), size = 4) +
  geom_text(data = p2, aes(label = months, x = round(days / 365.25, 2), y = .23, color = task), 
            angle = 90, size = 5, fontface = "bold") +
  geom_text(data = p2, aes(label = "months", x = round(days / 365.25, 2), y = .36, color = task), 
            angle = 90, size = 5, fontface = "bold") +
  
  theme_minimal() +
  
  scale_x_continuous(limits = c(2, 6), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.05)) +
  
  # Use scale_*_discrete with labels rather than scale_*_manual if you don't want custom colors
  scale_color_discrete(labels = img_labels) +
  scale_fill_discrete(labels = img_labels) +
  
  guides(
    color = guide_legend(ncol = 1, byrow = TRUE, title.position = "top" ),
    fill = "none",) +
  labs(
    x = "Age in Years", 
    y = "Proportion Correct",
    color = "Condition", 
    fill  = "Condition"   ) +
  theme(
    legend.position = c(0.875, 0.185),
    legend.background = element_rect(fill = alpha("white", 1), 
                                     color = "black", linewidth = 0.5),
    legend.key.height = unit(1, "cm"), 
    legend.key.width  = unit(1, "cm"),
    legend.title = element_text(size = 11, face = "bold", vjust = 0.5),
    legend.text  = element_markdown(hjust = 0.5, vjust = 0.5))

S2_plot + canvas(width = 27, height = 12, units = "cm", dpi = 600)

ggsave(
  filename = "../illustrations/S2_plot.svg",
  plot = S2_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "../illustrations/S2_plot.png",
  plot = S2_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

# Study 3 ----------------------------

## S3 Read Models and Comparisons --------------------
S3.full.bm <- readRDS("../models/S3.full.bm.rds")
S3.null.bm <- readRDS("../models/S3.null.bm.rds")
S3.comp <- readRDS("../models/S3.comp.rds")
S3.weights <- readRDS("../models/S3.weights.rds")
S3.loo.comp <- readRDS("../models/S3.loo.comp.rds")

S3task.null.bm <- readRDS("../models/S3task.null.bm.rds")
S3task.weights.task.null <- readRDS("../results/S3task.weights.task.null.rds")
S3task.comp.task.null <- readRDS("../results/S3task.comp.task.null.rds")

## S3 ppc ess etc ----------

## Check plots ...very good but no surprise in binomial model
pp_check(S3.full.bm)
pp_check(S3.null.bm)
pp_check(S3task.null.bm)

pp_check(S3.full.bm, type = "bars")
pp_check(S3.null.bm, type = "bars")
pp_check(S3task.null.bm, type = "bars")

### EFFECTICE SAMPLE SIZES
### inspect rhat --> always 1 --> no problems
### inspect Bull_ESS --> always > 1000 --> no problems
summary(S3.full.bm)
summary(S3.null.bm)
summary(S3task.null.bm)

### REDUCED MODEL IS REPORTED !!! i.e. S3.null.bm

# for reporting BULK_ESS and Coefficients
S3.null.coef <- summarise_draws(as_draws_df(S3.null.bm)) 

# NOTE: # as_draws_df() from posterior uses 90% CrIs
# we preregistered using 2.5% and 97.5% quantiles
draws_df <- as_draws_df(S3.null.bm)
quantiles_df <- as.data.frame(t(apply(draws_df, 2, quantile, probs = c(0.025, 0.975))))
quantiles_df <- tibble::rownames_to_column(quantiles_df, var = "variable")
names(quantiles_df)[2:3] <- c("q2.5", "q97.5")

S3.null.coef <- left_join(S3.null.coef, quantiles_df, by = "variable") 


S3.null.coef.table <- S3.null.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  # mutate(variable = gsub("task", "", variable)) %>% 
  mutate(variable = gsub("([^ ])([A-Z])", "\\1 \\2", variable)) %>% 
  mutate(variable = gsub(":", " × ", variable)) %>% 
  mutate(variable = gsub("c.age", "Age*", variable)) %>% 
  mutate(variable = gsub("c.trial", "Trial*", variable)) %>% 
  mutate(variable = gsub("sex1", "Sex (Male)", variable)) %>%
  mutate(variable = str_to_sentence(variable)) %>% 
  rename(
    Predictor = variable,
    Estimate = mean,
    SD = sd,
    MAD = mad,
    `2.5% CrI` = q2.5, 
    `97.5% CrI` = q97.5, 
    `Bulk ESS` = ess_bulk,
    `Tail ESS` = ess_tail) %>%
  mutate(
    `95% CrI` = paste0("[", sprintf("%.2f", `2.5% CrI`), ", ", sprintf("%.2f", `97.5% CrI`), "]"),
    `Bulk ESS` = format(round(`Bulk ESS`), big.mark = ","),
    `Tail ESS` = format(round(`Tail ESS`), big.mark = ",")) %>%
  select(Predictor, Estimate, SD, MAD, `95% CrI`, `Bulk ESS`, `Tail ESS`)

write_rds(S3.null.coef.table, "../results/S3.null.coef.table.rds")

# for reporting in text:
S3.null.coef <- S3.null.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  #  mutate(variable = gsub("task", "", variable)) %>% 
  data.frame(row.names = "variable")

write_rds(S3.null.coef, "../results/S3.null.coef.rds")

##############################
## full model
################################


# for reporting BULK_ESS and Coefficients
S3.full.coef <- summarise_draws(as_draws_df(S3.full.bm)) 

# NOTE: # as_draws_df() from posterior uses 90% CrIs
# we preregistered using 2.5% and 97.5% quantiles
draws_df <- as_draws_df(S3.full.bm)
quantiles_df <- as.data.frame(t(apply(draws_df, 2, quantile, probs = c(0.025, 0.975))))
quantiles_df <- tibble::rownames_to_column(quantiles_df, var = "variable")
names(quantiles_df)[2:3] <- c("q2.5", "q97.5")

S3.full.coef <- left_join(S3.full.coef, quantiles_df, by = "variable") 


S3.full.coef.table <- S3.full.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  # mutate(variable = gsub("task", "", variable)) %>% 
  mutate(variable = gsub("([^ ])([A-Z])", "\\1 \\2", variable)) %>% 
  mutate(variable = gsub(":", " × ", variable)) %>% 
  mutate(variable = gsub("c.age", "Age*", variable)) %>% 
  mutate(variable = gsub("c.trial", "Trial*", variable)) %>% 
  mutate(variable = gsub("sex1", "Sex (Male)", variable)) %>%
  mutate(variable = str_to_sentence(variable)) %>% 
  rename(
    Predictor = variable,
    Estimate = mean,
    SD = sd,
    MAD = mad,
    `2.5% CrI` = q2.5, 
    `97.5% CrI` = q97.5, 
    `Bulk ESS` = ess_bulk,
    `Tail ESS` = ess_tail) %>%
  mutate(
    `95% CrI` = paste0("[", sprintf("%.2f", `2.5% CrI`), ", ", sprintf("%.2f", `97.5% CrI`), "]"),
    `Bulk ESS` = format(round(`Bulk ESS`), big.mark = ","),
    `Tail ESS` = format(round(`Tail ESS`), big.mark = ",")) %>%
  select(Predictor, Estimate, SD, MAD, `95% CrI`, `Bulk ESS`, `Tail ESS`)

write_rds(S3.full.coef.table, "../results/S3.full.coef.table.rds")

# for reporting in text:
S3.full.coef <- S3.full.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  #  mutate(variable = gsub("task", "", variable)) %>% 
  data.frame(row.names = "variable")

write_rds(S3.full.coef, "../results/S3.full.coef.rds")


## S3 Exploratory ppc ess etc ----------

# for reporting BULK_ESS and Coefficients
S3task.null.coef <- summarise_draws(as_draws_df(S3task.null.bm)) # ! standard is just 90% CrI

# NOTE: # as_draws_df() from posterior uses 90% CrIs
# we preregistered using 2.5% and 97.5% quantiles
draws_df <- as_draws_df(S3task.null.bm)
quantiles_df <- as.data.frame(t(apply(draws_df, 2, quantile, probs = c(0.025, 0.975))))
quantiles_df <- tibble::rownames_to_column(quantiles_df, var = "variable")
names(quantiles_df)[2:3] <- c("q2.5", "q97.5")

S3task.null.coef <- left_join(S3task.null.coef, quantiles_df, by = "variable") 


S3task.null.coef.table <- S3task.null.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  # mutate(variable = gsub("task", "", variable)) %>% 
  mutate(variable = gsub("([^ ])([A-Z])", "\\1 \\2", variable)) %>% 
  mutate(variable = gsub(":", " × ", variable)) %>% 
  mutate(variable = gsub("c.age", "Age*", variable)) %>% 
  mutate(variable = gsub("c.trial", "Trial*", variable)) %>% 
  mutate(variable = gsub("sex1", "Sex (Male)", variable)) %>%
  mutate(variable = str_to_title(variable)) %>% 
  rename(
    Predictor = variable,
    Estimate = mean,
    SD = sd,
    MAD = mad,
    `2.5% CrI` = q2.5, 
    `97.5% CrI` = q97.5, 
    `Bulk ESS` = ess_bulk,
    `Tail ESS` = ess_tail) %>%
  mutate(
    `95% CrI` = paste0("[", sprintf("%.2f", `2.5% CrI`), ", ", sprintf("%.2f", `97.5% CrI`), "]"),
    `Bulk ESS` = format(round(`Bulk ESS`), big.mark = ","),
    `Tail ESS` = format(round(`Tail ESS`), big.mark = ",")) %>%
  select(Predictor, Estimate, SD, MAD, `95% CrI`, `Bulk ESS`, `Tail ESS`)

write_rds(S3task.null.coef.table, "../results/S3task.null.coef.table.rds")

# for reporting in text:
S3task.null.coef <- S3task.null.coef %>% 
  filter(grepl("^b_", variable)) %>% 
  mutate(variable = gsub("b_", "", variable)) %>% 
  #  mutate(variable = gsub("task", "", variable)) %>% 
  data.frame(row.names = "variable")

write_rds(S3task.null.coef, "../results/S3task.null.coef.rds")


## S3 prep Plot -------------

nd3 <- tibble(c.age = rep(seq(from = min(S3.data.bm$c.age),
                              to = max(S3.data.bm$c.age), length.out = 50),2),
              sex = rep(0,100),
              c.trial = rep(0,100))

f3 <- fitted(S3.null.bm,
             newdata = nd3, 
             re_formula = NA) %>% 
  as_tibble() %>%
  bind_cols(nd3)%>%
  mutate(age = c.age + mean(S3.data.bm$ageinyears))

d3 <- S3.data.bm%>%
  group_by(subid, ageinyears)%>%
  summarise(mean = mean(correct)) 

p3 <- f3 %>%
  summarise(
    # Interpolate exact age (in years) where lower bound Q2.5 crosses 0.5
    age_cross = approx(x = Q2.5, y = age, xout = 0.5, ties = mean)$y,
    days = age_cross * 365.25,
    months = round(days / 30.5),
    years = round(age_cross, 2))


## S3 Plot ------------------------

## 
dark_violet <- "#3D0C5A"

S3_plot <- ggplot() +
  geom_hline(yintercept = .5, lty = 2, alpha = .75) +
  
  # Individual performance 
  geom_point(data = d3, aes(x = ageinyears, y = mean), color = dark_violet, alpha = .5, shape = 1) +
  
  # CrI 
  geom_ribbon(data = f3, aes(x = age, ymin = Q2.5, ymax = Q97.5), fill = dark_violet, alpha = .25) +
  
  # Mean
  geom_line(data = f3, aes(x = age, y = Estimate), color = dark_violet, linewidth =1.3) +
  
  # Highlight Threshold
  geom_point(data = p3, aes(x = round(days / 365.25, 2), y = .5), color = dark_violet, size = 4) +
  geom_text(data = p3, aes(label = months, x = round(days / 365.25, 2), y = .24), color = dark_violet, angle = 90, size = 5) +
  geom_text(data = p3, aes(label = "months", x = round(days / 365.25, 2), y = .36), color = dark_violet, angle = 90, size = 5) +
  
  theme_minimal() +
  
  labs(x = "Age in Years", y = "Proportion Correct") +
  coord_cartesian(xlim = c(3, 5), ylim = c(0, 1)) +
  
  theme(
    # legend (wird hier ggf. nicht mehr angezeigt, da keine Variablen mehr in aes() gemappt sind, 
    # was korrekt ist, wenn du nur eine Gruppe/Farbe hast)
    legend.position = c(0.88, 0.15),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.key.size = unit(0.6, "cm"),       
    legend.text = element_text(size = 12))

S3_plot + canvas(width = 27, height = 12, units = "cm", dpi = 600)

ggsave(
  filename = "../illustrations/S3_plot.svg",
  plot = S3_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "../illustrations/S3_plot.png",
  plot = S3_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

# Preregistration

## S3task prep plot -------------------

nd3task <- tibble(c.age = rep(seq(from = min(S3.data.bm$c.age), 
                                  to = max(S3.data.bm$c.age), 
                                  length.out = 50),5),
                  task = c(rep("A",50), 
                           rep("B",50), 
                           rep("C",50), 
                           rep("D",50), 
                           rep("E",50)),
                  sex = rep(0,250),
                  c.trial = rep(0,250))


f3task <- fitted(S3task.null.bm, newdata = nd3task, re_formula = NA) %>%
  as_tibble() %>%
  bind_cols(nd3task)%>%
  mutate(age = c.age + mean(S3.data.bm$ageinyears))


d3task <- S3.data.bm%>%
  group_by(subid, ageinyears, task)%>%
  summarise(mean = mean(correct))

p3task <- f3task %>%
  group_by(task) %>%
  summarise(
    # Interpolate exact age (in years) where lower bound Q2.5 crosses 0.5
    age_cross = approx(x = Q2.5, y = age, xout = 0.5, ties = mean)$y,
    days = age_cross * 365.25,
    months = round(days / 30.5),
    years = round(age_cross, 2))

## load images
img_labels <- c(
  A = "<img src='../illustrations_raw/legend_S3_A.png' width='38'/>",
  B = "<img src='../illustrations_raw/legend_S3_B.png' width='38'/>",
  C = "<img src='../illustrations_raw/legend_S3_C.png' width='38'/>",
  D = "<img src='../illustrations_raw/legend_S3_D.png' width='38'/>",
  E = "<img src='../illustrations_raw/legend_S3_E.png' width='38'/>")


## prep palette with dummy
task_palette <- c(
  "Overall" = "#3D0C5A", # Dark Violet
  "A"       = "#FFD700", # Bright Gold/Yellow
  "B"       = "#FF5722", # Deep Orange
  "C"       = "#00BCD4", # Bright Cyan / Turquoise
  "D"       = "#2E7D32", # Vivid Green
  "E"       = "#E91E63")  # Hot Pink / Magenta 

## dummy label Overall/MEan
all_labels <- c(
  "Overall" = "<span style='font-size: 14px;'><b>Mean</b></span>",
  img_labels)

dark_violet <- "#3D0C5A"

## S3task Plot -------------------
S3_Cue_plot <- ggplot() +
  geom_hline(yintercept = .5, lty = 2, alpha = .75) +
  
  # Individual performance (Overall)
  geom_point(data = d3, aes(x = ageinyears, y = mean), color = dark_violet, alpha = .35, shape = 1) +
  
  # CrI (Overall)
  geom_ribbon(data = f3, aes(x = age, ymin = Q2.5, ymax = Q97.5), fill = dark_violet, alpha = .18) +
  
  # Mean INDIVIDUAL TASKS
  geom_line(data = f3task, aes(x = age, y = Estimate, col = task), linewidth = 0.6, alpha = 0.5) +
  
  # Mean OVERALL (Mapped to aes(col = "Overall") so it enters the legend)
  geom_line(data = f3, aes(x = age, y = Estimate, col = "Overall"), linewidth = 1.2, alpha = 0.8) +
  
  # Highlight Threshold OVERALL
  geom_point(data = p3, aes(x = round(days / 365.25, 2), y = .5), color = dark_violet, size = 6) +
  geom_text(data = p3, aes(label = months, x = round(days / 365.25, 2), y = .23), 
            color = dark_violet, angle = 90, size = 5, fontface = "bold", alpha = 0.8) +
  geom_text(data = p3, aes(label = "months", x = round(days / 365.25, 2), y = .36), 
            color = dark_violet, angle = 90, size = 5, fontface = "bold", alpha = 0.8) +
  # Highlight Threshold INDIVIDUAL
  geom_point(
    data = p3task, 
    aes(x = round(days / 365.25, 8), y = .5, fill = task, col = task), 
    size = 3, 
    alpha = 0.95,
    position = position_jitter(width = 0.01, height = 0, seed = 42525))+
  
  theme_minimal() +
  
  # Color & Fill Scales
  scale_color_manual(values = task_palette, labels = all_labels, limits = c("Overall", "A", "B", "C", "D", "E")) +
  scale_fill_manual(values = task_palette, labels = all_labels, limits = c("Overall", "A", "B", "C", "D", "E")) +
  
  scale_x_continuous(limits = c(3, 5), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.05)) +
  
  guides(
    color = guide_legend(
      ncol = 1, 
      byrow = TRUE, 
      title.position = "top", 
      override.aes = list(linewidth = c(9, 1, 1, 1, 1, 1)) # Controlled line weight in legend
    ),
    fill = guide_legend(ncol = 1, byrow = TRUE, title.position = "top")
  ) +
  labs(
    x = "Age in Years", 
    y = "Proportion Correct",
    color = "Cue Type", 
    fill  = "Cue Type"   
  ) +
  theme(
    legend.position = c(0.915, 0.44),
    legend.background = element_rect(fill = alpha("white", 1), 
                                     color = "black", linewidth = 0.5),
    legend.key.height = unit(0.80, "cm"), 
    legend.key.width  = unit(0.80, "cm"),
    
    legend.title = element_text(size = 11, face = "bold", vjust = 0.5),
    legend.text  = element_markdown(hjust = 0.5, vjust = 0.5)
  )

S3_Cue_plot + canvas(width = 27, height = 12, units = "cm", dpi = 600)

ggsave(
  filename = "../illustrations/S3_Cue_plot.svg",
  plot = S3_Cue_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

ggsave(
  filename = "../illustrations/S3_Cue_plot.png",
  plot = S3_Cue_plot,
  width = 27,
  height = 12,
  units = "cm",
  dpi = 600)

# PARTICIPANTS -----------------------------

## Participants reporting -------------------

age.report.data   <- symlit.arrows.data %>% #select data
  filter(sample != "drop") %>% 
  filter(valid == "1")  %>%
  distinct(subid, .keep_all = TRUE) %>% 
  group_by(study) %>% 
  summarize(
    N = length(subid),
    female = sum(sex == "0"),
    Mean = round(mean(agem), 2),
    Min = min(agem),
    Max = max(agem),
    SD = round(sd(agem), 2))

age.report.data <- data.frame(age.report.data, row.names = "study")

saveRDS(age.report.data, "../results/age.report.data.rds")


## Participants table ------------

age.table.data  <- symlit.arrows.data %>% #select data
  filter(valid == "1")  %>%
  distinct(subid, .keep_all = TRUE) %>% # select only one line per participant
  group_by(study, age_group) %>% 
  summarize(
    N = length(subid),
    female = sum(sex == "0"),
    Mean = round(mean(agem), 2),
    Min = min(agem),
    Max = max(agem),
    SD = round(sd(agem), 2)) %>% 
  rename("Age Group" = age_group) %>% 
  rename(Study = study) %>% 
  mutate(Study = str_to_sentence(Study)) %>% 
  mutate(Study = str_replace(Study, "(?=\\d)", " ")) %>% 
  # Remove duplicate labels sequentially
  ungroup() %>% 
  mutate(
    Study = ifelse(duplicated(Study), "", Study))

saveRDS(age.table.data, "../results/age.table.data.rds")

## Participants Plot -------

age.plot.data <- symlit.arrows.data %>%
  filter(sample != "drop") %>% 
  filter(valid  == "1")  %>%
  distinct(subid, .keep_all = TRUE) %>% 
  # group_by(study, agem) %>% 
  # summarise(N = n())
  mutate(sex = as.factor(sex))  %>%
  mutate(sex = fct_recode(sex, "female" = "0","male"="1")) %>% 
  rename(Sex = sex) %>% 
  mutate(study = case_when(
    study == "study1" ~ "Study 1",
    study == "study2" ~ "Study 2",
    study == "study3" ~ "Study 3",
    TRUE ~ NA_character_))

age.plot <- ggplot(age.plot.data, aes(x = agem, fill = Sex)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all") +  
  coord_fixed(ratio = 4, xlim = c(24, 72), ylim = c(0, 1.01)) +  # combine here
  theme_bw() +
  scale_x_continuous(name = "Age in Months", breaks = seq(24, 72, 12), minor_breaks = seq(24, 72, 1)) +
  scale_y_continuous(name = "Participants", breaks = NULL) +
  facet_wrap(~ study, ncol = 1) +
  labs(fill = NULL) +
  guides(fill = guide_legend(ncol =2, byrow = TRUE)) +  
  theme_minimal() +
  theme(
    # base_size = 13,
    panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 2),
    panel.grid.major.x = element_line(linewidth = 1, linetype = 1),
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1),
    strip.background = element_rect(colour = "grey30", size = 1),
    strip.text = element_text(face = "bold", size = rel(.9), 
                              margin = margin(t = 5, b = 8)),
    axis.title = element_text(face = "bold", size = rel(.9)),
    axis.text = element_text(size = rel(0.9)),
    legend.position = c(1, -0.06),              
    legend.justification = c(1, 1),
    legend.text = element_text(size = rel(.9)),
    panel.grid.minor = element_blank())

age.plot + canvas(width=27, height= 11, units="cm", dpi = 600)

ggsave(
  filename = "../illustrations/plot_participants.png",
  plot = age.plot,
  device = "png",
  width = 27, 
  height = 11,
  dpi = 600,
  units = "cm",
  bg = "white")

ggsave(
  filename = "../illustrations/plot_participants.svg",
  plot = age.plot,
  device = "png",
  width = 27, 
  height = 11,
  dpi = 600,
  units = "cm",
  bg = "white")

## Drops Reporting -----------------------------

drop.report.data.S1  <- symlit.arrows.data %>%
  distinct(subid, .keep_all = TRUE) %>% # select only one line per participant
  filter(sample == "drop") %>%
  filter(study == "study1") %>% 
  group_by(whydrop) %>%
  summarize(
    N = length(subid),
    female = sum(sex=="0"),
    Mean = mean(agem),
    Min = min(agem),
    Max = max(agem))
drop.report.data.S1 <- data.frame(drop.report.data.S1, row.names = "whydrop")

saveRDS(drop.report.data.S1, "../results/drop.report.data.S1.rds")

drop.report.data.S2  <- symlit.arrows.data %>%
  distinct(subid, .keep_all = TRUE) %>% # select only one line per participant
  filter(sample == "drop") %>%
  filter(study == "study2") %>% 
  group_by(whydrop) %>%
  summarize(
    N = length(subid),
    female = sum(sex=="0"),
    Mean = mean(agem),
    Min = min(agem),
    Max = max(agem))
drop.report.data.S2 <- data.frame(drop.report.data.S2, row.names = "whydrop")

saveRDS(drop.report.data.S2, "../results/drop.report.data.S2.rds")

drop.report.data.S3  <- symlit.arrows.data %>%
  distinct(subid, .keep_all = TRUE) %>% # select only one line per participant
  filter(sample == "drop") %>%
  filter(study == "study3") %>% 
  group_by(whydrop) %>%
  summarize(
    N = length(subid),
    female = sum(sex=="0"),
    Mean = mean(agem),
    Min = min(agem),
    Max = max(agem))
drop.report.data.S3 <- data.frame(drop.report.data.S3, row.names = "whydrop")

saveRDS(drop.report.data.S3, "../results/drop.report.data.S3.rds")

## Drops Plot ---------------

drop.plot.data <- symlit.arrows.data %>% #select data
  distinct(subid, .keep_all = TRUE) %>% # select only one line per participant
  filter(sample == "drop") %>% 
  mutate(study = case_when(
    study == "study1" ~ "Study 1",
    study == "study2" ~ "Study 2",
    study == "study3" ~ "Study 3",
    TRUE ~ NA_character_)) %>% 
  mutate(agem = if_else(agem <= 24, 24, agem)) %>% 
  mutate(whydrop = case_when(
    whydrop == "fail_in_fam" ~ "< 75% Correct in Familiarization",
    whydrop == "fussy_child" ~ "Fussy Child", 
    whydrop == "language" ~ "Low Language Proficiency", 
    whydrop == "quit_early" ~ "Quit Early",
    whydrop == "technical" ~ "Technical Problem",
    TRUE ~ NA_character_)) %>%  
  mutate(whydrop = factor(whydrop, levels = c(
    "< 75% Correct in Familiarization",
    "Quit Early",
    "Fussy Child",
    "Low Language Proficiency",
    "Technical Problem"))) %>% 
  mutate(sex = as.factor(sex))  %>%
  mutate(sex = fct_recode(sex, "female" = "0","male"="1")) %>% 
  rename(Sex = sex)

drop.plot <- ggplot(drop.plot.data, aes(x = agem, fill = whydrop)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all") +  
  coord_fixed(ratio = 4, xlim = c(24, 72), ylim = c(0, 1.01)) +  # combine here
  theme_bw() +
  scale_x_continuous(name = "Age in Months", breaks = seq(24, 72, 12), minor_breaks = seq(24, 72, 1)) +
  scale_y_continuous(name = "Participants", breaks = NULL) +
  facet_wrap(~ study, ncol = 1) +
  labs(fill = NULL) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +  
  theme_minimal() +
  theme(
    # base_size = 13,
    panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 2),
    panel.grid.major.x = element_line(linewidth = 1, linetype = 1),
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1),
    strip.background = element_rect(colour = "grey30", size = 1),
    strip.text = element_text(face = "bold", size = rel(.9), 
                              margin = margin(t = 5, b = 8)),
    axis.title = element_text(face = "bold", size = rel(.9)),
    axis.text = element_text(size = rel(0.8)),
    legend.position = c(1, -0.06),              
    legend.justification = c(1, 1),
    legend.text = element_text(size = rel(.9)),
    panel.grid.minor = element_blank())

drop.plot + canvas(width=27, height= 11, units="cm", dpi = 600)


# Save 
ggsave(
  filename = "../illustrations/plot_drops.png",
  plot = drop.plot,
  device = "png",
  width = 27,
  height = 11,
  dpi = 600,
  units = "cm",
  bg = "white")

ggsave(
  filename = "../illustrations/plot_drops.svg",
  plot = drop.plot,
  device = "png",
  width = 27,
  height = 11,
  dpi = 600,
  units = "cm",
  bg = "white")

# cleanup
rm(
  age.plot.data,
  drop.plot.data,
  age.plot,
  drop.plot)

## Test dates reporting ----------------------

testdates <- symlit.arrows.data %>% 
  mutate(testdate = as.Date(testdate, format = "%d/%m/%Y")) %>% 
  group_by(study) %>% 
  summarise(
    start = min(testdate, na.rm = TRUE),
    end   = max(testdate, na.rm = TRUE))

saveRDS(testdates, "../results/testdates.rds")

# study1 July 2021 - April 2022
# study2 July 2021 - June 2022
# study3 November 2021 - May 2022

rm(testdates)

# RELIABILITY --------------------------------------------

reli.data <- symlit.arrows.data %>%
  filter(reli == "reli") %>%
  filter(!is.na(valid)) %>%
  filter(sample != "drop") %>% 
  select(study, sample, subid, valid, dropcategory, correct, reli_valid, reli_dropcategory, reli_correct,
         task, sign, position, sidecorrect, order, trial)

## Reliability - valid ----

agree_valid_results <- reli.data %>%
  filter(sample != "drop") %>% 
  group_by(study) %>% 
  summarize(
    total_trials = n(),
    agreed_trials = sum(valid == reli_valid, na.rm = TRUE),
    percent_agree = mean(valid == reli_valid, na.rm = TRUE) * 100) %>% 
  column_to_rownames(var = "study")

kappa_valid_results.S1 <- reli.data %>%
  filter(study == "study1") %>% 
  select(valid, reli_valid) %>%
  kappa2()

kappa_valid_results.S2 <- reli.data %>%
  filter(study == "study2") %>% 
  select(valid, reli_valid) %>%
  kappa2()

kappa_valid_results.S3 <- reli.data %>%
  filter(study == "study3") %>% 
  select(valid, reli_valid) %>%
  kappa2()


## Reliability - correct ----
agree_correct_results <- reli.data %>%
  group_by(study) %>% 
  filter(valid == 1) %>% 
  summarize(
    total_trials = n(),
    agreed_trials = sum(correct == reli_correct, na.rm = TRUE),
    percent_agree = mean(correct == reli_correct, na.rm = TRUE) * 100) %>% 
    column_to_rownames(var = "study")

kappa_correct_results.S1 <- reli.data %>%
  filter(study == "study1") %>% 
  filter(valid == 1) %>% 
  select(correct, reli_correct) %>%
  kappa2()

kappa_correct_results.S2 <- reli.data %>%
  filter(study == "study2") %>% 
  filter(valid == 1) %>% 
  select(correct, reli_correct) %>%
  kappa2()

kappa_correct_results.S3 <- reli.data %>%
  filter(study == "study3") %>% 
  filter(valid == 1) %>% 
  select(correct, reli_correct) %>%
  kappa2()



## Reliability - reporting -----------------------------------------

count.sampletrials <- symlit.arrows.data %>%
  filter(sample != "drop") %>%
  group_by(study) %>% 
  count() %>% 
  column_to_rownames(var = "study")

count.relitrials <- symlit.arrows.data %>%
  filter(sample != "drop") %>%
  filter(reli == "reli") %>%
  group_by(study) %>% 
  count() %>% 
  column_to_rownames(var = "study")

perc.relitrials <- round((count.relitrials/count.sampletrials)*100,2)

count.testtrials <- symlit.arrows.data %>%
  filter(sample != "drop") %>% 
  filter(trialtype == "test") %>%
  group_by(study) %>% 
  count() %>% 
  column_to_rownames(var = "study")

excludedtrials <- symlit.arrows.data %>%
  filter(sample != "drop") %>%
  filter(valid == 0) %>%
  group_by(study) %>% 
  count() %>% 
  column_to_rownames(var = "study")

perc.excludedtrials <- round((excludedtrials/count.sampletrials)*100,2)

# reli valid --> moderate
kappa.valid.S1 <- round(kappa_valid_results.S1$value,2)
kappa.valid.S2 <- round(kappa_valid_results.S2$value,2)
kappa.valid.S3 <- round(kappa_valid_results.S3$value,2)

kappa.p.valid.S1 <- round(kappa_valid_results.S1$p.value,2)
kappa.p.valid.S2 <- round(kappa_valid_results.S2$p.value,2)
kappa.p.valid.S3 <- round(kappa_valid_results.S3$p.value,2)

# reli correct --> almost perfect
kappa.correct.S1 <- round(kappa_correct_results.S1$value,2)
kappa.correct.S2 <- round(kappa_correct_results.S2$value,2)
kappa.correct.S3 <- round(kappa_correct_results.S3$value,2)

kappa.p.correct.S1 <- round(kappa_correct_results.S1$p.value,2)
kappa.p.correct.S2 <- round(kappa_correct_results.S2$p.value,2)
kappa.p.correct.S3 <- round(kappa_correct_results.S3$p.value,2)

# reli correct
# kappa < 0.20  ~ "slight",
# kappa <= 0.40 ~ "fair",
# kappa <= 0.60 ~ "moderate",
# kappa <= 0.80 ~ "substantial",
# kappa <= 1.00 ~ "almost perfect"

rm(
  reli.data,
  kappa_correct_results.S1,
  kappa_correct_results.S2,
  kappa_correct_results.S3,
  kappa_valid_results.S1,
  kappa_valid_results.S2,
  kappa_valid_results.S3)

write_rds(agree_correct_results, "../results/agree_correct_results.rds")
write_rds(agree_valid_results, "../results/agree_valid_results.rds")
write_rds(count.relitrials, "../results/count.relitrials.rds")
write_rds(count.sampletrials, "../results/count.sampletrials.rds")
write_rds(count.testtrials, "../results/count.testtrials.rds")
write_rds(excludedtrials, "../results/excludedtrials.rds")
write_rds(perc.excludedtrials, "../results/perc.excludedtrials.rds")
write_rds(kappa.correct.S1, "../results/kappa.correct.S1.rds")
write_rds(kappa.correct.S2, "../results/kappa.correct.S2.rds")
write_rds(kappa.correct.S3, "../results/kappa.correct.S3.rds")
write_rds(kappa.p.correct.S1, "../results/kappa.p.correct.S1.rds")
write_rds(kappa.p.correct.S2, "../results/kappa.p.correct.S2.rds")
write_rds(kappa.p.correct.S3, "../results/kappa.p.correct.S3.rds")
write_rds(kappa.valid.S1, "../results/kappa.valid.S1.rds")
write_rds(kappa.valid.S2, "../results/kappa.valid.S2.rds")
write_rds(kappa.valid.S3, "../results/kappa.valid.S3.rds")
write_rds(kappa.p.correct.S1, "../results/kappa.p.correct.S1.rds")
write_rds(kappa.p.correct.S2, "../results/kappa.p.correct.S2.rds")
write_rds(kappa.p.correct.S3, "../results/kappa.p.correct.S3.rds")

# Inter-rater reliability was assessed across $N = XXX$ trials (xxx percent of sample).
# Coder agreement was high, $\kappa = .XX$, $p < .XXX$ (overall agreement $= XXX\%$).

# DESCRIPTIVES & CONVENTIONAL ---------------------------------------

library(scales) # due to conflicts it needs to be reloaded here

## Descriptives - Binomial -----------------

# aggregate data
arrows_agg_data <- symlit.arrows.data %>% 
  filter(sample != "drop" & Condition != "fam" & !is.na(correct)) %>% 
  group_by(study, age_group, Condition, subid) %>%
  summarise(
    sum_correct = sum(correct),
    sum_trials  = n(),
    prop_correct = sum_correct / sum_trials,
    pct_correct  = prop_correct * 100,
    # Binomial tests against chance (0.5)
    p_below   = binom.test(sum_correct, sum_trials, p = 0.5, alternative = "less")$p.value,
    p_above   = binom.test(sum_correct, sum_trials, p = 0.5, alternative = "greater")$p.value,
    # flag if sig
    sig_below = if_else(p_below < 0.05, 1, 0),
    sig_above = if_else(p_above < 0.05, 1, 0),
    .groups   = "drop")


# descriptives
arrows.descr.data <- arrows_agg_data %>%
  group_by(study, age_group, Condition) %>%
  summarise(
    N            = n_distinct(subid),
    mean_trials  = mean(sum_trials),
    M            = mean(prop_correct),
    SD           = sd(prop_correct),
    t_stat       = t.test(prop_correct, mu = 0.5)$statistic[["t"]],
    df           = t.test(prop_correct, mu = 0.5)$parameter[["df"]],
    p_val        = t.test(prop_correct, mu = 0.5)$p.value,
    # Cohen's d for one-sample t-test: (M - mu) / SD
    cohen_d      = round(cohensD((prop_correct), mu = 0.5), 2), 
    # Individual binomial counts
    below_chance = sum(sig_below),
    above_chance = sum(sig_above),
    # Percentages
    pct_below    = (below_chance / N) * 100,
    pct_above    = (above_chance / N) * 100,
    .groups      = "drop")

# prep table
arrows.descr.data <- arrows.descr.data %>%
  arrange(study, Condition, age_group) %>%
  mutate(
    # Format strings for table display
    Study        = str_to_sentence(study) %>% str_replace("(?=\\d)", " "),
    `Age Group`  = age_group,
    `trials/N`   = round(mean_trials, 2),
    `M (%)`      = round(M * 100, 2),
    `SD (%)`     = round(SD * 100, 2),
    `N suc` = round(above_chance,0),
    `% suc` = round(pct_above,2),
    t            = round(t_stat, 2),
    df           = round(df,0),
    p            = scales::pvalue(p_val, accuracy = .001),
    d            = round(cohen_d, 2)
  ) %>%
  select(
    Study, 
    Condition, 
    `Age Group`, 
    N, 
    `trials/N`,
    `M (%)`, 
    `SD (%)`, 
    `N suc`,
    `% suc`,
    t, df, p, d) %>%
  # delete duplicate labels
  mutate(
    Study     = if_else(duplicated(Study), "", Study),
    Condition = if_else(duplicated(paste(Condition)), "", Condition))


write_rds(arrows.descr.data, "../results/arrows.descr.data.rds")

# Tidy up workspace -------------------------------------

rm(list = ls())

# save all results as csv too

list.files("../results", pattern = "\\.rds$", full.names = TRUE) %>% 
  set_names(~ tools::file_path_sans_ext(basename(.x))) %>% 
  map(read_rds) %>% 
  list2env(envir = .GlobalEnv)

df_names <- Filter(function(x) is.data.frame(get(x)), ls())

for (name in df_names) {
  write.csv(get(name), file = file.path("../results", paste0(name, ".csv")), row.names = FALSE)
}

rm(list = ls())
