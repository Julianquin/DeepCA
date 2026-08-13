# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 30)

# 20 nodes ----------------------------------------------------------------

# generating 20 nodes network
node20network_c <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 20, alpha = 3, deltas = c(0.5,1.1), seed = seed,
                             symmetric = F, type = "count"))
  names(network) <- paste0("seed",(seed))
  node20network_c <- append(node20network_c, network)
}

# generating 50 nodes network
node50network_c <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 3, deltas = c(0.5,1.1), seed = seed,
                             symmetric = F, type = "count"))
  names(network) <- paste0("seed",(seed))
  node50network_c <- append(node50network_c, network)
}

# generating 100 nodes network
node100network_c <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 100, alpha = 3, deltas = c(0.5,1.1), seed = seed,
                             symmetric = F, type = "count"))
  names(network) <- paste0("seed",(seed))
  node100network_c <- append(node100network_c, network)
}

# generating 200 nodes network
node200network_c <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 200, alpha = 3, deltas = c(0.5,1.1), seed = seed,
                             symmetric = F, type = "count"))
  names(network) <- paste0("seed",(seed))
  node200network_c <- append(node200network_c, network)
}
