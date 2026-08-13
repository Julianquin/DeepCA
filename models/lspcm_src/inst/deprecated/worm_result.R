# male neuron -------------------------------------------------------------

wormadj <- as.matrix(as_adjacency_matrix(read.graph("~/lspm/inst/c.elegans_neural.male_1.graphml", format="graphml")))

worm_lpsm5d <- LPSM(wormadj,
                      n_dimen= 5, iter=10e5,
                      step_size = c(2.4,2.5), initial_adjust = c(1.3,0.02), seed = 15,
                      burnin= 1, thin=1) # unclear 2d/4d

worm_lpsm_thin5d <- thinLPSM(worm_lpsm5d, burnin=30e4, thin=4.5e3)

diagLPSM(worm_lpsm_thin5d)
plot(worm_lpsm_thin5d, parameter='deltas')

set.seed(1234)
seed_number <- sample(1:1e6, 10)

# Fitting 5D LPSM on worm
worm_lpsm5d <- list()
for(seed in seed_number[4:10]) {
  lpsm_single_result <- list(LPSM(wormadj,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(2.4,2.7), initial_adjust = c(1,0.02),
                                  seed = seed,
                                  burnin= 1, thin=1))
  names(lpsm_single_result) <- paste0("seed",(seed))
  worm_lpsm5d <-  append(worm_lpsm5d, lpsm_single_result)
}
class(worm_lpsm5d) <- "LPSM"

# save(worm_lpsm5d, file = "worm_lpsm5d.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(worm_lpsm5d[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(worm_lpsm5d[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# Thinning the LPSM result
worm_lpsm5d_thin <- list()
for(seed in seed_number) {
  lpsm_single_result <- list(thinLPSM(worm_lpsm5d[[paste0("seed",(seed))]], burnin=15e4, thin=3e3))
  names(lpsm_single_result) <- paste0("seed",(seed))
  worm_lpsm5d_thin <-  append(worm_lpsm5d_thin, lpsm_single_result)
}
class(worm_lpsm5d_thin) <- "LPSM"

# save(worm_lpsm5d_thin, file = "worm_lpsm5d_thin.Rdata")


for(seed in seed_number) {
  diagLPSM(worm_lpsm5d_thin[[paste0("seed",(seed))]])
}
plot(worm_lpsm5d_thin, parameter='deltas')

library(latentnet)

worm_lpm1d <- ergmm(wormadj ~ euclidean(d=1), seed = 115)
worm_lpm2d <- ergmm(wormadj ~ euclidean(d=2), seed = 115)
worm_lpm3d <- ergmm(wormadj ~ euclidean(d=3), seed = 115)
worm_lpm4d <- ergmm(wormadj ~ euclidean(d=4), seed = 115)
worm_lpm5d <- ergmm(wormadj ~ euclidean(d=5), seed = 115)
worm_lpm6d <- ergmm(wormadj ~ euclidean(d=6), seed = 115)
worm_lpm7d <- ergmm(wormadj ~ euclidean(d=7), seed = 115)


worm_lpm4d_full <- list()
for(seed in seed_number) {
  lpm_single_result <- list(ergmm(wormadj ~ euclidean(d=4), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  worm_lpm4d_full <-  append(worm_lpm4d_full, lpm_single_result)
}
# save(worm_lpm4d_full, file = "worm_lpm4d_full.Rdata")

worm_lpm5d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(wormadj ~ euclidean(d=5), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  worm_lpm5d_full <-  append(worm_lpm5d_full, lpm_single_result)
}
save(worm_lpm5d_full, file = "worm_lpm5d_full.Rdata")

worm_lpm3d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(wormadj ~ euclidean(d=3), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  worm_lpm3d_full <-  append(worm_lpm3d_full, lpm_single_result)
}
save(worm_lpm3d_full, file = "worm_lpm3d_full.Rdata")


lpmbic<- c(
  summary(worm_lpm1d)$bic$overall,
  summary(worm_lpm2d)$bic$overall,
  summary(worm_lpm3d_full$seed761680)$bic$overall,
  summary(worm_lpm4d_full$seed761680)$bic$overall, # suggest 4d
  summary(worm_lpm5d)$bic$overall,
  summary(worm_lpm6d)$bic$overall,
  summary(worm_lpm7d)$bic$overall
)


lpmbic <- c(summary(worm_lpm1d)$bic$overall,
            summary(worm_lpm2d)$bic$overall,
            summary(worm_lpm3d)$bic$overall,
            summary(worm_lpm4d)$bic$overall, # suggest 4d
            summary(worm_lpm5d)$bic$overall)
par(mfrow=c(1,2))

# open the pdf file
cairo_pdf("~/lspm/inst/figures/wormdelta.pdf", width = 5, height = 4)
par(mar=c(4.1, 4.1, 1, 1.1))
plot(worm_lspm5d_thin, parameter='deltas') # plot shrinkage strength
legend("bottomright", legend=c('Posterior distribution', 'Average posterior mean'), pch=c(20,20), col=c("black", "green"), cex=1.2)
dev.off()

# open the pdf file
cairo_pdf("~/lspm/inst/figures/wormlpmbic.pdf", width = 5, height = 4)
par(mar=c(4.1, 4.1, 1, 1.1))
plot(lpmbic, ylab="BIC", xlab="Dimension", pch=20, col=4,cex=2)
dev.off()


palette('default')
