source("~/lspm/inst/section4_1network_c.R")

# 20 nodes ----------------------------------------------------------------

# Fitting 5D LSPM on 20 nodes
node20results_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node20network_c[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5, family = "Poisson",
                                  step_size = c(5,.4),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node20results_c <-  append(node20results_c, lspm_single_result)
}
class(node20results_c) <- "LSPM"

# save(node20results_c, file = "node20results_c.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node20results_c[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node20results_c[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# thinning 20 nodes result
node20results_c_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node20results_c[[paste0("seed",(seed))]], burnin=5e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node20results_c_thin <-  append(node20results_c_thin, lspm_single_result)
}
class(node20results_c_thin) <- "LSPM"

# save(node20results_c_thin, file = "node20results_c_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node20results_c_thin[[paste0("seed",(seed))]])
# }


# 50 nodes ----------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes
node50results_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network_c[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5, family = "Poisson",
                                  step_size = c(5,0.4),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results_c <-  append(node50results_c, lspm_single_result)
}
class(node50results_c) <- "LSPM"

# save(node50results_c, file = "node50results_c.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node50results_c[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node50results_c[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# thinning 50 nodes result
node50results_c_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results_c[[paste0("seed",(seed))]], burnin=5e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results_c_thin <-  append(node50results_c_thin, lspm_single_result)
}
class(node50results_c_thin) <- "LSPM"

# save(node50results_c_thin, file = "node50results_c_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node50results_c_thin[[paste0("seed",(seed))]])
# }


# 100 nodes ---------------------------------------------------------------

# Fitting 5D LSPM on 100 nodes
node100results_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network_c[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5, family = "Poisson",
                                  step_size = c(5,.4),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results_c <-  append(node100results_c, lspm_single_result)
}
class(node100results_c) <- "LSPM"

# save(node100results_c, file = "node100results_c.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results_c[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results_c[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }


# thinning 100 nodes result
node100results_c_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results_c[[paste0("seed",(seed))]], burnin=20e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results_c_thin <-  append(node100results_c_thin, lspm_single_result)
}
class(node100results_c_thin) <- "LSPM"

# save(node100results_c_thin, file = "node100results_c_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node100results_c_thin[[paste0("seed",(seed))]])
# }


# 200 nodes ---------------------------------------------------------------

# Fitting 5D LSPM on 200 nodes
node200results_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node200network_c[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5, family = "Poisson",
                                  step_size = c(5,.4),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node200results_c <-  append(node200results_c, lspm_single_result)
}
class(node200results_c) <- "LSPM"

# save(node200results_c, file = "node200results_c.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node200results_c[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node200results_c[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# thinning 200 nodes result
node200results_c_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node200results_c[[paste0("seed",(seed))]], burnin=30e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node200results_c_thin <-  append(node200results_c_thin, lspm_single_result)
}
class(node200results_c_thin) <- "LSPM"

# save(node200results_c_thin, file = "node200results_c_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node200results_c_thin[[paste0("seed",(seed))]])
# }

