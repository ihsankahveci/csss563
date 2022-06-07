library(tidyverse)
library(splines)
#library(lme4)
library(nlme)
library(modelsummary)

data = readRDS("data/cov_projections.RDS")

train = data %>% filter(pred == 0)
test = data %>% filter(pred == 1) %>% select(-tfr, -ccf50)

my_lme = function(formula, data){
  # Estimate a random effects AR(I)MA(p,q) model using lme (Restricted ML)
  lme.res <- nlme::lme(# A formula object including the response,
    # the fixed covariates, and any grouping variables
    data = data, 
    fixed = formula,
    # The random effects component
    random = ~ 1 | country,
    # The TS dynamics: specify the time & group variables,
    # and the order of the ARMA(p,q) process
    correlation = corARMA(form = ~ year | country,
                          p = 1,  # AR(p) order
                          q = 0   # MA(q) order
    ) 
  )
  return(lme.res)
}

m1 = my_lme(formula = ccf50 ~ mn + edu, data = train)
m2 = my_lme(formula = ccf50 ~ mn + ns(edu, df = 1), data = train)
m3 = my_lme(formula = ccf50 ~ mn + ns(edu, df = 2), data = train)
m4 = my_lme(formula = ccf50 ~ mn + ns(edu, df = 3), data = train)
m5 = my_lme(formula = ccf50 ~ mn + ns(edu, df = 4), data = train)
lme_models = list(m1, m2, m3, m4, m5)

modelsummary(lme_models, statistic = NULL, fmt = 2,
             estimate = "[{conf.low}, {conf.high}]")

# BEST MODEL ccf50 ~ mn + ns(edu)

model = m2
test$ccf50 = predict(model, newdata = test)

final = bind_rows(train, test)
  
final %>% 
  ggplot(aes(x=year, y=ccf50, linetype = pred)) +
  geom_line() + 
  facet_wrap(~country) + 
  theme_bw()













lme_models = list(m0, m1, m2, m3, m4, m5)

m0 <- nlme::lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  data = train, 
  fixed = ccf50 ~ 1,
  # The random effects component
  random = ~ 1 | country,
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ year | country,
                        p = 1,  # AR(p) order
                        q = 0   # MA(q) order
  ) 
)

m1 <- nlme::lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  data = train, 
  fixed = ccf50 ~ mn + edu,
  # The random effects component
  random = ~ 1 | country,
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ year | country,
                        p = 1,  # AR(p) order
                        q = 0   # MA(q) order
  ) 
)


m2 <- nlme::lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  data = train, 
  fixed = ccf50 ~ mn + ns(edu),
  # The random effects component
  random = ~ 1 | country,
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ year | country,
                        p = 1,  # AR(p) order
                        q = 0   # MA(q) order
  ) 
)

m3 <- nlme::lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  data = train, 
  fixed = ccf50 ~ mn + ns(edu, df = 2),
  # The random effects component
  random = ~ 1 | country,
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ year | country,
                        p = 1,  # AR(p) order
                        q = 0   # MA(q) order
  ) 
)

m4 <- nlme::lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  data = train, 
  fixed = ccf50 ~ mn + ns(edu, df = 3),
  # The random effects component
  random = ~ 1 | country,
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ year | country,
                        p = 1,  # AR(p) order
                        q = 0   # MA(q) order
  ) 
)

m5 <- nlme::lme(# A formula object including the response,
  # the fixed covariates, and any grouping variables
  data = train, 
  fixed = ccf50 ~ mn + ns(edu, df = 4),
  # The random effects component
  random = ~ 1 | country,
  # The TS dynamics: specify the time & group variables,
  # and the order of the ARMA(p,q) process
  correlation = corARMA(form = ~ year | country,
                        p = 1,  # AR(p) order
                        q = 0   # MA(q) order
  ) 
)


