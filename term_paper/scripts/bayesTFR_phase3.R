library(bayesTFR)

# this is required for running Phase III models
mcmc = run.tfr.mcmc(
  start.year = 1982, present.year = 2018,
  my.tfr.file = "data/my.tfr.file.txt",
  annual = TRUE, ar.phase2 = TRUE,
  nr.chains = 3, parellel = TRUE, 
  thin = TRUE, iter = 10000, replace.output = TRUE)

# fitting an Bayesian Hierarchical AR(1) model 
mcmc3 = run.tfr3.mcmc(
  sim.dir = "bayesTFR.output/",
  start.year = 1982, present.year = 2018,
  my.tfr.file = "data/my.tfr.file.txt",
  annual = TRUE, nr.chains = 3, iter = 10000, 
  parellel = TRUE, thin = TRUE, replace.output = TRUE)


# Projection annually up to 2030.
tfr_projections = tfr.predict(
  sim.dir = "bayesTFR.output/",
  end.year = 2030, nr.traj = 100,
  burnin = 1000, burnin3 = 1000,
  use.tfr3 = TRUE, replace.output = TRUE)
  
# plot the results for all countries
tfr.pred = get.tfr.prediction("bayesTFR.output/")
bayesTFR::tfr.trajectories.plot.all(tfr.pred, output.type = "pdf")



