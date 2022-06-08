
# cleaning and combining the following data sources:
# IHME, GBD 2019, covariates and TFR
# HFD, complete cohort certility
# UN, Contrapcetive demand satisfied
# Wittgenstein Center female education
source("scripts/prep_data.R")

# projecting IHME covariates as a function of UN and Wittgenstein estimates
source("scripts/project_covariates.R")

# creating summary plots of observed andestimates and covariates projected 
source("scripts/create_plots.R")

# testing simple OLS models with country-level fixed effects. 
source("scripts/ols_models.R")

# running run.tfr.mcmc, run.tfr3.mcmc, and tfr.predict.
# caution takes long time, make sure parelelized
source("scripts/bayesTFR_phase3.R")