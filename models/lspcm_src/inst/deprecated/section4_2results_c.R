source("~/lspm/inst/section4_2network_c.R")

# LSPM 3D -----------------------------------------------------------------

# Fitting 3D LSPM on network
node100results4d3_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d_c[[paste0("seed",(seed))]]$network,
                                  n_dimen= 3, iter=5e5, family = "Poisson",
                                  step_size = c(5,0.2),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_c <-  append(node100results4d3_c, lspm_single_result)
}
class(node100results4d3_c) <- "LSPM"

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results4d3_c[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results4d3_c[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# save(node100results4d3_c, file = "node100results4d3_c.Rdata")

# Thinning the LSPM result
node100results4d3_c_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results4d3_c[[paste0("seed",(seed))]], burnin=2e5, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_c_thin <-  append(node100results4d3_c_thin, lspm_single_result)
}
class(node100results4d3_c_thin) <- "LSPM"

# save(node100results4d3_c_thin, file = "node100results4d3_c_thin.Rdata")


# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node100results4d3_c_thin[[paste0("seed",(seed))]])
# }


# LSPM 4D ------------------------------------------------------------------

# Fitting 4D LSPM on network
node100results4d4_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d_c[[paste0("seed",(seed))]]$network,
                                  n_dimen= 4, iter=5e5, family = "Poisson",
                                  step_size = c(5,0.2),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d4_c <-  append(node100results4d4_c, lspm_single_result)
}
class(node100results4d4_c) <- "LSPM"

# save(node100results4d4_c, file = "node100results4d4_c.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results4d4_c[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results4d4_c[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# Thinning the LSPM result
node100results4d4_c_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results4d4_c[[paste0("seed",(seed))]], burnin=2e5, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d4_c_thin <-  append(node100results4d4_c_thin, lspm_single_result)
}
class(node100results4d4_c_thin) <- "LSPM"

# save(node100results4d4_c_thin, file = "node100results4d4_c_thin.Rdata")

# Check diagnostic
for(seed in seed_number) {
  diagLSPM(node100results4d4_c_thin[[paste0("seed",(seed))]])
}


# LSPM 8D -----------------------------------------------------------------

# Fitting 4D LSPM on network
node100results4d8_c <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d_c[[paste0("seed",(seed))]]$network,
                                  n_dimen= 8, iter=5e5, family = "Poisson",
                                  step_size = c(5,0.2),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d8_c <-  append(node100results4d8_c, lspm_single_result)
}
class(node100results4d8_c) <- "LSPM"

# save(node100results4d8_c, file = "node100results4d8_c.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results4d8_c[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results4d8_c[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# Thinning the LSPM result
node100results4d8_c_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results4d8_c[[paste0("seed",(seed))]], burnin=3.5e5, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d8_c_thin <-  append(node100results4d8_c_thin, lspm_single_result)
}
class(node100results4d8_c_thin) <- "LSPM"

# save(node100results4d8_c_thin, file = "node100results4d8_c_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node100results4d8_c_thin[[paste0("seed",(seed))]])
# }

