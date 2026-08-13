set.seed(1234)
seed_number <- sample(1:1e6, 30)

# Generate 4d network with 100 nodes
node100network4d <- list()
for(seed in seed_number) {
  network <- list(networkMTGP(n = 100, alpha = 6, deltas = c(0.5,1.1, 1.05, 1.15),
                             seed = seed, symmetric = F))
  names(network) <- paste0("seed",(seed))
  node100network4d <- append(node100network4d, network)
}
