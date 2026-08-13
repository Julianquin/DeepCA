source("~/lspm/inst/section4_2network.R")

# LSPM 3D -----------------------------------------------------------------

# Fitting 3D LSPM on network
node100results4d3 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
                                  n_dimen= 3, iter=5e5,
                                  step_size = c(2.5,1.2),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3 <-  append(node100results4d3, lspm_single_result)
}
class(node100results4d3) <- "LSPM"

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results4d3[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results4d3[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# save(node100results4d3, file = "node100results4d3.Rdata")

# Thinning the LSPM result
node100results4d3_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results4d3[[paste0("seed",(seed))]], burnin=5e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d3_thin <-  append(node100results4d3_thin, lspm_single_result)
}
class(node100results4d3_thin) <- "LSPM"

# save(node100results4d3_thin, file = "node100results4d3_thin.Rdata")


# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node100results4d3_thin[[paste0("seed",(seed))]])
# }


# LSPM 4D ------------------------------------------------------------------

# Fitting 4D LSPM on network
node100results4d4 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
                                  n_dimen= 4, iter=5e5,
                                  step_size = c(2.5,1.2),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d4 <-  append(node100results4d4, lspm_single_result)
}
class(node100results4d4) <- "LSPM"

# save(node100results4d4, file = "node100results4d4.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results4d4[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results4d4[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# Thinning the LSPM result
node100results4d4_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results4d4[[paste0("seed",(seed))]], burnin=5e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d4_thin <-  append(node100results4d4_thin, lspm_single_result)
}
class(node100results4d4_thin) <- "LSPM"

# save(node100results4d4_thin, file = "node100results4d4_thin.Rdata")

# Check diagnostic
for(seed in seed_number) {
  diagLSPM(node100results4d4_thin[[paste0("seed",(seed))]])
}


# LSPM 8D -----------------------------------------------------------------

# Fitting 4D LSPM on network
node100results4d8 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node100network4d[[paste0("seed",(seed))]]$network,
                                  n_dimen= 8, iter=5e5,
                                  step_size = c(2.5,1.2),
                                  initial_adjust = c(1.3,0.01),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d8 <-  append(node100results4d8, lspm_single_result)
}
class(node100results4d8) <- "LSPM"

# save(node100results4d8, file = "node100results4d8.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(node100results4d8[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(node100results4d8[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# Thinning the LSPM result
node100results4d8_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node100results4d8[[paste0("seed",(seed))]], burnin=5e4, thin=2e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node100results4d8_thin <-  append(node100results4d8_thin, lspm_single_result)
}
class(node100results4d8_thin) <- "LSPM"

# save(node100results4d8_thin, file = "node100results4d8_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node100results4d8_thin[[paste0("seed",(seed))]])
# }

