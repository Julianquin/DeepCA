source("~/lspm/inst/section4_1network.R")

# 20 nodes ----------------------------------------------------------------

# Fitting 5D LSPM on 20 nodes
node20results_nonT <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM_nonT(node20network[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,1.1),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node20results_nonT <-  append(node20results_nonT, lspm_single_result)
}
class(node20results_nonT) <- "LSPM"

# save(node20results, file = "node20results.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node20results[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node20results[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# thinning 20 nodes result
node20results_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node20results[[paste0("seed",(seed))]], burnin=5e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node20results_thin <-  append(node20results_thin, lspm_single_result)
}
class(node20results_thin) <- "LSPM"

# save(node20results_thin, file = "node20results_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node20results_thin[[paste0("seed",(seed))]])
# }


# 50 nodes ----------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes
node50results <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,1.1),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results <-  append(node50results, lspm_single_result)
}
class(node50results) <- "LSPM"

# save(node50results, file = "node50results.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node50results[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node50results[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# thinning 50 nodes result
node50results_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results[[paste0("seed",(seed))]], burnin=5e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results_thin <-  append(node50results_thin, lspm_single_result)
}
class(node50results_thin) <- "LSPM"

# save(node50results_thin, file = "node50results_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node50results_thin[[paste0("seed",(seed))]])
# }


# 100 nodes ---------------------------------------------------------------

# Fitting 5D LSPM on 100 nodes
node100results_nonT <- list()
for(seed in seed_number[1]) {
  lspm_single_result <- list(LSPM_nonT(node100network[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,1),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results_nonT <-  append(node100results_nonT, lspm_single_result)
}
class(node100results_nonT) <- "LSPM"

# save(node100results, file = "node100results.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }


# thinning 100 nodes result
node100results_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results[[paste0("seed",(seed))]], burnin=20e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results_thin <-  append(node100results_thin, lspm_single_result)
}
class(node100results_thin) <- "LSPM"

# save(node100results_thin, file = "node100results_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node100results_thin[[paste0("seed",(seed))]])
# }


# 200 nodes ---------------------------------------------------------------

# Fitting 5D LSPM on 200 nodes
node200results <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node200network[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(2.9,1.3),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node200results <-  append(node200results, lspm_single_result)
}
class(node200results) <- "LSPM"

# save(node200results, file = "node200results.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node200results[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node200results[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# thinning 200 nodes result
node200results_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node200results[[paste0("seed",(seed))]], burnin=20e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node200results_thin <-  append(node200results_thin, lspm_single_result)
}
class(node200results_thin) <- "LSPM"

# save(node200results_thin, file = "node200results_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node200results_thin[[paste0("seed",(seed))]])
# }

