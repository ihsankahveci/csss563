
# Load libraries ----------------------------------------------------------

library(methods)
library(demest)


# Load data ---------------------------------------------------------------

death_counts       <- readRDS("data/prepped/death_counts.RDS")
pop_counts         <- readRDS("data/prepped/pop_counts.RDS")
region_concordance <- readRDS("data/prepped/region_concordance.RDS")
region_ex_direct   <- readRDS("data/results/region_ex_direct.rds")


# Modeling prep -----------------------------------------------------------

set.seed(256)

run_model <- function(model_spec, out_file) {

  estimateModel(
    model = model_spec,
    y = death_counts,
    exposure = pop_counts,
    filename = out_file,
    nBurnin = 40000,
    nSim = 40000,
    nChain = 4,
    nThin = 80
  )

}

gen_model_spec <- function(agg = NULL) {

  Model(
    y ~ Poisson(mean ~ age * sex + location),
    age ~ DLM(damp = NULL, error = Error(robust = TRUE)),
    aggregate = agg,
    jump = 0.11
  )

}


# No benchmark model ------------------------------------------------------

model_none <- gen_model_spec(agg = NULL)
model_none_filename <- "data/results/model_none.est"
# run_model(model_none, model_none_filename)


# Benchmarked model -------------------------------------------------------

aggregate <- AgLife(
  value = region_ex_direct,
  sd = 0.005,
  concordances = list(location = region_concordance)
)

model_bench <- gen_model_spec(agg = aggregate)
model_bench_filename <- "data/results/model_bench.est"
# run_model(model_bench, model_bench_filename)


# Get model summaries -----------------------------------------------------

model_none_summary <- fetchSummary(model_none_filename)
model_bench_summary <- fetchSummary(model_bench_filename)


# Save modeled outputs ----------------------------------------------------

model_none_mx <- fetch(
  filename = model_none_filename,
  where = c("model", "likelihood", "rate")
)

model_bench_mx <- fetch(
  filename = model_bench_filename,
  where = c("model", "likelihood", "rate")
)

model_both_mx <- dbind(
  None = model_none_mx,
  Benchmarks = model_bench_mx,
  along = "variant"
)

saveRDS(model_both_mx, "data/results/model_mx_both.RDS")
