library(tidyverse)

# READ DATA
data = readRDS("data/final.RDS")

# PLOT FERTILITY 
fertility_long = data %>%
  rename(TFR = tfr, CCF50 = ccf50) %>%
  pivot_longer(
    cols = c(CCF50, TFR), 
    names_to = "Metric", 
    values_to = "Rate") 

fertility_plot = fertility_long %>%
  ggplot(aes(x = year, y = Rate, color = Metric)) +
  geom_line(show.legend = TRUE) + 
  geom_hline(yintercept = 2.1, linetype = "dashed") +
  facet_wrap(~country, ncol = 4) +
  theme_bw() + 
  labs(
    title = "TFR vs. CCF50 of Phase III Countries, 1982-2018",
    caption = "TFR is obtained by IHME's GBD 2019 data. 
    CCF50 is obtained by Human Fertility Database. 
    Dashed line indicates replacement level (TFR = 2.1).",
    x = NULL) + 
  theme(
    legend.position = c(0.3, -0.07),
    legend.direction = "horizontal",
    legend.background = element_blank())

ggsave(plot = fertility_plot, "plots/fertility_all.png", width = 7, height = 7)

# PLOT COVARIATES 
covariates_long = data %>%
  rename(Education = edu, `Met-need` = mn) %>%
  pivot_longer(
    cols = c(Education, `Met-need`), 
    names_to = "Covariate", 
    values_to = "Estimate") 

covariates_plot = covariates_long %>%
  ggplot(aes(x = year, y = Estimate, color = Covariate)) +
  geom_line(show.legend = TRUE) + 
  facet_wrap(~country, ncol = 4) +
  theme_bw() + 
  labs(
    title = "Covariates for Phase III Countries, 1982-2018",
    subtitle = "Maternal Education in years. Contraceptive met-need.",
    caption = "Data source: IHME GBD 2019",
    x = NULL) +
  theme(
    legend.position = c(0.3, -0.05), 
    legend.direction = "horizontal",
    legend.background = element_blank())

ggsave(plot = covariates_plot, "plots/covariates_all.png", width = 7, height = 7)



