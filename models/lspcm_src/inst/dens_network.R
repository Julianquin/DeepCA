# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 30)

# generating 50 nodes network with alpha = 0
node50network3d5_0 <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 0, deltas = c(0.5,1.1, 1.05), seed = seed,symmetric = F))
  names(network) <- paste0("seed",(seed))
  node50network3d5_0 <- append(node50network3d5_0, network)
}

# generating 50 nodes network with alpha = 1
node50network3d5_1 <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 1, deltas = c(0.5,1.1, 1.05), seed = seed,symmetric = F))
  names(network) <- paste0("seed",(seed))
  node50network3d5_1 <- append(node50network3d5_1, network)
}

# generating 50 nodes network with alpha = 5
node50network3d5_5 <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 5, deltas = c(0.5,1.1, 1.05), seed = seed,symmetric = F))
  names(network) <- paste0("seed",(seed))
  node50network3d5_5 <- append(node50network3d5_5, network)
}

# generating 50 nodes network with alpha = 10
node50network3d5_10 <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 10, deltas = c(0.5,1.1, 1.05), seed = seed,symmetric = F))
  names(network) <- paste0("seed",(seed))
  node50network3d5_10 <- append(node50network3d5_10, network)
}

# generating 50 nodes network with alpha = 20
node50network3d5_20 <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 20, deltas = c(0.5,1.1, 1.05), seed = seed,symmetric = F))
  names(network) <- paste0("seed",(seed))
  node50network3d5_20 <- append(node50network3d5_20, network)
}

# generating 50 nodes network with alpha = 30
node50network3d5_30 <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 50, alpha = 30, deltas = c(0.5,1.1, 1.05), seed = seed,symmetric = F))
  names(network) <- paste0("seed",(seed))
  node50network3d5_30 <- append(node50network3d5_30, network)
}
