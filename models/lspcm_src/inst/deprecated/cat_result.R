library(igraph)

catadj <- as.matrix(as_adjacency_matrix(read.graph("~/lspm/inst/mixed.species_brain_1.graphml", format="graphml")))

set.seed(1234)
seed_number <- sample(1:1e6, 10)

# Fitting 5D LSPM on cat connectome
cat_lspm5d <- list()
for(seed in seed_number) {
  lspm_single_result <- list(LSPM(catadj,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,1.3), initial_adjust = c(1.3,0.02), seed = seed,
                                  burnin= 1, thin=1))
  names(lspm_single_result) <- paste0("seed",(seed))
  cat_lspm5d <-  append(cat_lspm5d, lspm_single_result)
}
class(cat_lspm5d) <- "LSPM"

# save(cat_lspm5d, file = "cat_lspm5d.Rdata")

# Thinning the LSPM result
cat_lspm5d_thin <- list()
for(seed in seed_number) {
  lspm_single_result <- list(thinLSPM(cat_lspm5d[[paste0("seed",(seed))]], burnin=5e4, thin=3e3))
  names(lspm_single_result) <- paste0("seed",(seed))
  cat_lspm5d_thin <-  append(cat_lspm5d_thin, lspm_single_result)
}
class(cat_lspm5d_thin) <- "LSPM"

# save(cat_lspm5d_thin, file = "cat_lspm5d_thin.Rdata")

for(seed in seed_number) {
  diagLSPM(cat_lspm5d_thin[[paste0("seed",(seed))]])
}

cat_lpm2d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(catadj ~ euclidean(d=2), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  cat_lpm2d_full <-  append(cat_lpm2d_full, lpm_single_result)
}

# save(cat_lpm2d_full, file = "cat_lpm2d_full.Rdata")


cat_lpm3d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(catadj ~ euclidean(d=3), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  cat_lpm3d_full <-  append(cat_lpm3d_full, lpm_single_result)
}

# save(cat_lpm3d_full, file = "cat_lpm3d_full.Rdata")


cat_lpm1d <- ergmm(catadj ~ euclidean(d=1), seed = 115)
cat_lpm2d <- ergmm(catadj ~ euclidean(d=2), seed = 115)
cat_lpm3d <- ergmm(catadj ~ euclidean(d=3), seed = 115)
cat_lpm4d <- ergmm(catadj ~ euclidean(d=4), seed = 115)
cat_lpm5d <- ergmm(catadj ~ euclidean(d=5), seed = 115)

summary(cat_lpm1d)$bic$overall
summary(cat_lpm2d)$bic$overall
summary(cat_lpm3d)$bic$overall
summary(cat_lpm4d)$bic$overall
summary(cat_lpm5d)$bic$overall

cat_lpm = list(oneD = cat_lpm1d, twoD = cat_lpm2d, threeD = cat_lpm3d, fourD = cat_lpm4d, fiveD = cat_lpm5d)
# save(cat_lpm, file = "cat_lpm.Rdata")

lpm_bic = do.call(rbind, lapply(cat_lpm, function(x) summary(x)$bic))
plot(do.call(rbind,lpm_bic[,6]), pch=20, col="blue", ylab="BIC", xlab="Dimension")

# save(cat_lpm2d, file = "cat_lpm2d.Rdata")

library(coda)
gelman.plot(mcmc.list(mcmc(cat_lspm5d_thin$seed761680$deltas),
                      mcmc(cat_lspm5d_thin$seed630678$deltas),
                      mcmc(cat_lspm5d_thin$seed304108$deltas),
                      mcmc(cat_lspm5d_thin$seed932745$deltas),
                      mcmc(cat_lspm5d_thin$seed295846$deltas),
                      mcmc(cat_lspm5d_thin$seed126055$deltas),
                      mcmc(cat_lspm5d_thin$seed382554$deltas),
                      mcmc(cat_lspm5d_thin$seed345167$deltas),
                      mcmc(cat_lspm5d_thin$seed867188$deltas),
                      mcmc(cat_lspm5d_thin$seed871806$deltas)))
title(main="Cat Connectome")



# open the pdf file
cairo_pdf("~/lspm/inst/figures/alphatrace.pdf", width = 4, height = 3)
par(mar = c(4.1, 4.6, 1.1, 1.1))
plot(cat_lpsm5d_thin$seed871806$alpha, type="l", ylab= expression(paste("Posterior ", alpha, " samples")), xlab="Iteration")
dev.off()
cairo_pdf("~/lspm/inst/figures/liketrace.pdf", width = 4, height = 3)
par(mar = c(4.1, 4.6, 1.1, 1.1))
plot(cat_lpsm5d_thin$seed871806$like, type="l", ylab= "Posterior loglikelihood samples", xlab="Iteration")
dev.off()
cairo_pdf("~/lspm/inst/figures/alphaacf.pdf", width = 4, height = 3)
par(mar = c(4.1, 4.6, 1.1, 1.1))
acf(cat_lpsm5d_thin$seed871806$alpha, main="")
dev.off()
cairo_pdf("~/lspm/inst/figures/likeacf.pdf", width = 4, height = 3)
par(mar = c(4.1, 4.6, 1.1, 1.1))
acf(cat_lpsm5d_thin$seed871806$like, main="")
dev.off()

par(mar = c(5.1, 4.1, 4.1, 2.1))
