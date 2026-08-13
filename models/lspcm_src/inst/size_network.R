# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 30)

# generating 20 nodes network
node20network <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 20, alpha = 3, deltas = c(0.5,1.1), seed = seed,symmetric = F))
  names(network) <- paste0("seed",(seed))
  node20network <- append(node20network, network)
}

# generating 50 nodes network
node50network <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 3, deltas = c(0.5,1.1), seed = seed, symmetric = F))
  names(network) <- paste0("seed",(seed))
  node50network <- append(node50network, network)
}

# generating 100 nodes network
node100network <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 100, alpha = 3, deltas = c(0.5,1.1), seed = seed, symmetric = F))
  names(network) <- paste0("seed",(seed))
  node100network <- append(node100network, network)
}

# generating 200 nodes network
node200network <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 200, alpha = 3, deltas = c(0.5,1.1), seed = seed, symmetric = F))
  names(network) <- paste0("seed",(seed))
  node200network <- append(node200network, network)
}
