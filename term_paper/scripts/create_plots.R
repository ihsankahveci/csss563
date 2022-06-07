library(tidyverse)

# READ DATA
data = readRDS("data/final.RDS")
projections = readRDS("data/cov_projections.RDS")

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

# PLOT MET NEED PROJECTIONS
metneed_plot = projections %>% 
  select(country, year, pred, IHME = mn, UN = un_mn) %>%
  mutate(UN = UN/100) %>%
  pivot_longer(c(IHME, UN)) %>%
  ggplot(aes(x=year, y=value, color = name, linetype = pred)) + 
  geom_line() + 
  facet_wrap(~country) +
  guides(linetype = "none",
         color = guide_legend(title = NULL)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  theme(legend.position = c(0.85, 0.15),
        legend.key.height = unit(1, "cm")) +
  labs(x = NULL, y = NULL,
       title = "Contraceptive Met-need Projections 2018-2030") 

ggsave(plot = metneed_plot, "plots/contraceptive_projections.png", width = 7, height = 7)

# PLOT EDU PROJECTIONS
education_plot = projections %>% 
  select(country, year, pred, IHME = edu, WCDE = wcde_edu) %>%
  pivot_longer(c(IHME, WCDE)) %>%
  ggplot(aes(x=year, y=value, color = name, linetype = pred)) + 
  geom_line() + 
  facet_wrap(~country) +
  guides(linetype = "none",
         color = guide_legend(title = NULL)) +
  theme_bw() + 
  theme(legend.position = c(0.85, 0.15),
        legend.key.height = unit(1, "cm")) +
  labs(x = NULL, y = NULL,
       title = "Years of Education at 25, Females, 2018-2030")

ggsave(plot = education_plot, "plots/education_projections.png", width = 7, height = 7)


