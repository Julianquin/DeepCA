source("~/lspm/inst/section4_3network.R")

# alpha = 0 ----------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes network with alpha = 0
node50results3d5_0 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_0[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,3),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_0 <-  append(node50results3d5_0, lspm_single_result)
}
class(node50results3d5_0) <- "LSPM"

# save(node50results3d5_0, file = "node50results3d5_0.Rdata")

# Checks acceptance ratio
for(seed in seed_number) {
  cat(node50results3d5_0[[paste0("seed",(seed))]]$aca/5e5)
  cat(" ")
}
for(seed in seed_number) {
  cat(node50results3d5_0[[paste0("seed",(seed))]]$acz/5e5)
  cat(" ")
}

# thinning results
node50results3d5_0_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results3d5_0[[paste0("seed",(seed))]], burnin=10e4, thin=4e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_0_thin <-  append(node50results3d5_0_thin, lspm_single_result)
}
class(node50results3d5_0_thin) <- "LSPM"

# save(node50results3d5_0_thin, file = "node50results3d5_0_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node50results3d5_0_thin[[paste0("seed",(seed))]])
# }



# alpha = 1 ----------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes network with alpha = 1
node50results3d5_1 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_1[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,2.2),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_1 <-  append(node50results3d5_1, lspm_single_result)
}
class(node50results3d5_1) <- "LSPM"

# save(node50results3d5_1, file = "node50results3d5_1.Rdata")

# Checks acceptance ratio
for(seed in seed_number) {
  cat(node50results3d5_1[[paste0("seed",(seed))]]$aca/5e5)
  cat(" ")
}
for(seed in seed_number) {
  cat(node50results3d5_1[[paste0("seed",(seed))]]$acz/5e5)
  cat(" ")
}

# thinning results
node50results3d5_1_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results3d5_1[[paste0("seed",(seed))]], burnin=5e4, thin=3.5e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_1_thin <-  append(node50results3d5_1_thin, lspm_single_result)
}
class(node50results3d5_1_thin) <- "LSPM"

# save(node50results3d5_1_thin, file = "node50results3d5_1_thin.Rdata")

# Check diagnostic
for(seed in seed_number) {
  diagLSPM(node50results3d5_1_thin[[paste0("seed",(seed))]])
}



# alpha = 5 ---------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes network with alpha = 5
node50results3d5_5 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_5[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,1),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_5 <-  append(node50results3d5_5, lspm_single_result)
}
class(node50results3d5_5) <- "LSPM"

# save(node50results3d5_5, file = "node50results3d5_5.Rdata")

# Checks acceptance ratio
for(seed in seed_number) {
  cat(node50results3d5_5[[paste0("seed",(seed))]]$aca/5e5)
  cat(" ")
}
for(seed in seed_number) {
  cat(node50results3d5_5[[paste0("seed",(seed))]]$acz/5e5)
  cat(" ")
}

# thinning results
node50results3d5_5_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results3d5_5[[paste0("seed",(seed))]], burnin=5e4, thin=3.5e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_5_thin <-  append(node50results3d5_5_thin, lspm_single_result)
}
class(node50results3d5_5_thin) <- "LSPM"

# save(node50results3d5_5_thin, file = "node50results3d5_5_thin.Rdata")
#
# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node50results3d5_5_thin[[paste0("seed",(seed))]])
# }




# alpha = 10 ----------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes network with alpha = 0
node50results3d5_10 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_10[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,0.7),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_10 <-  append(node50results3d5_10, lspm_single_result)
}
class(node50results3d5_10) <- "LSPM"

# save(node50results3d5_10, file = "node50results3d5_10.Rdata")

# Checks acceptance ratio
for(seed in seed_number) {
  cat(node50results3d5_10[[paste0("seed",(seed))]]$aca/5e5)
  cat(" ")
}
for(seed in seed_number) {
  cat(node50results3d5_10[[paste0("seed",(seed))]]$acz/5e5)
  cat(" ")
}

# thinning results
node50results3d5_10_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results3d5_10[[paste0("seed",(seed))]], burnin=5e4, thin=3.5e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_10_thin <-  append(node50results3d5_10_thin, lspm_single_result)
}
class(node50results3d5_10_thin) <- "LSPM"

# save(node50results3d5_10_thin, file = "node50results3d5_10_thin.Rdata")

# Check diagnostic
for(seed in seed_number) {
  diagLSPM(node50results3d5_10_thin[[paste0("seed",(seed))]])
}


# alpha = 20 ----------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes network with alpha = 0
node50results3d5_20 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_20[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=.05e5,
                                  step_size = c(3,1),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_20 <-  append(node50results3d5_20, lspm_single_result)
}
class(node50results3d5_20) <- "LSPM"

# save(node50results3d5_20, file = "node50results3d5_20.Rdata")

# Checks acceptance ratio
for(seed in seed_number) {
  cat(node50results3d5_20[[paste0("seed",(seed))]]$aca/.05e5)
  cat(" ")
}
for(seed in seed_number) {
  cat(node50results3d5_20[[paste0("seed",(seed))]]$acz/.05e5)
  cat(" ")
}

# thinning results
node50results3d5_20_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results3d5_20[[paste0("seed",(seed))]], burnin=5e4, thin=3.5e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_20_thin <-  append(node50results3d5_20_thin, lspm_single_result)
}
class(node50results3d5_20_thin) <- "LSPM"

# save(node50results3d5_20_thin, file = "node50results3d5_20_thin.Rdata")

# # Check diagnostic
# for(seed in seed_number) {
#   diagLSPM(node50results3d5_20_thin[[paste0("seed",(seed))]])
# }

# alpha = 30 ----------------------------------------------------------------

# Fitting 5D LSPM on 50 nodes network with alpha = 0
node50results3d5_30 <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(node50network3d5_30[[paste0("seed",(seed))]]$network,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,2.3),
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_30 <-  append(node50results3d5_30, lspm_single_result)
}
class(node50results3d5_30) <- "LSPM"

# save(node50results3d5_30, file = "node50results3d5_30.Rdata")

# Checks acceptance ratio
for(seed in seed_number) {
  cat(node50results3d5_30[[paste0("seed",(seed))]]$aca/5e5)
  cat(" ")
}
for(seed in seed_number) {
  cat(node50results3d5_30[[paste0("seed",(seed))]]$acz/5e5)
  cat(" ")
}

# thinning results
node50results3d5_30_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(node50results3d5_30[[paste0("seed",(seed))]], burnin=5e4, thin=3.5e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  node50results3d5_30_thin <-  append(node50results3d5_30_thin, lspm_single_result)
}
class(node50results3d5_30_thin) <- "LSPM"

# save(node50results3d5_30_thin, file = "node50results3d5_30_thin.Rdata")

# Check diagnostic
for(seed in seed_number) {
  diagLSPM(node50results3d5_30_thin[[paste0("seed",(seed))]])
}

