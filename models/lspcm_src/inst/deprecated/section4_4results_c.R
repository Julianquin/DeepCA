
source("~/lspm/inst/section4_4network_c.R")

# Low Overdispersion -------------------------------------------------

# Fitting 5D LSPM on 100 nodes network with low overdispersion
count2d0_result <- list()
for(seed in seed_number) {
  lspm_single_result = 0
  lspm_single_result <- list(LSPM(count2d0_network[[paste0("seed",(seed))]]$network,
                                   n_dimen= 5, iter=5e5, family = "Poisson",
                                   step_size = c(5,1.3),
                                   burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d0_result <-  append(count2d0_result, lspm_single_result)
}
class(count2d0_result) <- "LSPM"

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(count2d0_result[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(count2d0_result[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# save(count2d0_result, file = "count2d0_result.Rdata")


# Thinning the LSPM result
count2d0_result_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(count2d0_result[[paste0("seed",(seed))]], burnin=30e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d0_result_thin <-  append(count2d0_result_thin, lspm_single_result)
}
class(count2d0_result_thin) <- "LSPM"

# save(count2d0_result_thin, file = "count2d0_result_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(count2d0_result_thin[[paste0("seed",(seed))]])
# }
#
# # Check delta
# for(seed in seed_number) {
#   plot(count2d0_result_thin[[paste0("seed",(seed))]], parameter="deltas")
# }


# Moderate Overdispersion -------------------------------------------------

# Fitting 5D LSPM on 100 nodes network with moderate overdispersion
count2d1_result <- list()
for(seed in seed_number) {
  lspm_single_result = 0
  lspm_single_result <- list(LSPM(count2d1_network[[paste0("seed",(seed))]]$network,
                                   n_dimen= 5, iter=5e5, family = "Poisson",
                                   step_size = c(5,.8),
                                   burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d1_result <-  append(count2d1_result, lspm_single_result)
}
class(count2d1_result) <- "LSPM"

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(count2d1_result[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(count2d1_result[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# save(count2d1_result, file = "count2d1_result.Rdata")

# Thinning the LSPM result
count2d1_result_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(count2d1_result[[paste0("seed",(seed))]], burnin=30e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d1_result_thin <-  append(count2d1_result_thin, lspm_single_result)
}
class(count2d1_result_thin) <- "LSPM"

# save(count2d1_result_thin, file = "count2d1_result_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(count2d1_result_thin[[paste0("seed",(seed))]])
# }
#
# # Check delta
# for(seed in seed_number) {
#   plot(count2d1_result_thin[[paste0("seed",(seed))]], parameter="deltas")
# }


# High Overdispersion -------------------------------------------------

# Fitting 5D LSPM on 100 nodes network with high overdispersion
count2d5_result <- list()
for(seed in seed_number) {
  lspm_single_result = 0
  lspm_single_result <- list(LSPM(count2d5_network[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5, family = "Poisson",
                                  step_size = c(5,.17),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d5_result <-  append(count2d5_result, lspm_single_result)
}
class(count2d5_result) <- "LSPM"

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(count2d5_result[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(count2d5_result[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# save(count2d5_result, file = "count2d5_result.Rdata")

# Thinning the LSPM result
count2d5_result_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(count2d5_result[[paste0("seed",(seed))]], burnin=30e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  count2d5_result_thin <-  append(count2d5_result_thin, lspm_single_result)
}
class(count2d5_result_thin) <- "LSPM"

# save(count2d5_result_thin, file = "count2d5_result_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(count2d5_result_thin[[paste0("seed",(seed))]])
# }
#
# # Check delta
# for(seed in seed_number) {
#   plot(count2d5_result_thin[[paste0("seed",(seed))]], parameter="deltas")
# }
