# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 30)

# generating 100 nodes network with low overdispersion
count2d0_network <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 100, alpha = 0.5, deltas = c(1.5,1.5),
                             seed = seed, symmetric = F, type = "count"))
  names(network) <- paste0("seed",(seed))
  count2d0_network <- append(count2d0_network, network)
}

# generating 100 nodes network with moderate overdispersion
count2d1_network <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 100, alpha = 1.5, deltas = c(.5,1.5),
                             seed = seed, symmetric = F, type= "count"))
  names(network) <- paste0("seed",(seed))
  count2d1_network <- append(count2d1_network, network)
}

# generating 100 nodes network with high overdispersion
count2d5_network <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 100, alpha = 5, deltas = c(0.1,1.5),
                             seed = seed, symmetric = F, type= "count"))
  names(network) <- paste0("seed",(seed))
  count2d5_network <- append(count2d5_network, network)
}
