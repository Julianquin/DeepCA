library(latentnet)

# # Import data
# zach_raw <- read.csv("~/out.ucidata-zach_lpsmary", sep="")
# zachary <- as.matrix(as_adjacency_matrix(graph_from_edgelist(as.matrix(zach_raw))))

library(igraphdata)
data(karate)
zachary <- as.matrix(as_adjacency_matrix(karate))

set.seed(1234)
seed_number <- sample(1:1e6, 10)

# Fitting 5D LPSM on zachary karate club
zach_lspm1d <- list()
for(seed in seed_number[6:10]) {
  lpsm_single_result <- list(LSPM(zachary,
                                  n_dimen= 1, iter=5e5,
                                  step_size = c(3,.7),
                                  burnin= 5e4, thin=5e3))
  names(lpsm_single_result) <- paste0("seed",(seed))
  zach_lspm1d <-  append(zach_lspm1d, lpsm_single_result)
}
class(zach_lspm1d) <- "LSPM"

zach_lspm2d <- list()
for(seed in seed_number[6:10]) {
  lpsm_single_result <- list(LSPM(zachary,
                                  n_dimen= 2, iter=5e5,
                                  step_size = c(3,.7),
                                  burnin= 5e4, thin=5e3))
  names(lpsm_single_result) <- paste0("seed",(seed))
  zach_lspm2d <-  append(zach_lspm2d, lpsm_single_result)
}
class(zach_lspm2d) <- "LSPM"


# Fitting 5D LPSM on zachary karate club
zach_lpsm <- list()
for(seed in seed_number) {
  lpsm_single_result <- list(LPSM(zachary,
                                  n_dimen= 5, iter=5e5,
                                  step_size = c(3,.7),
                                  burnin= 1, thin=1))
  names(lpsm_single_result) <- paste0("seed",(seed))
  zach_lpsm <-  append(zach_lpsm, lpsm_single_result)
}
class(zach_lpsm) <- "LPSM"

# save(zach_lpsm, file = "zach_lpsm.Rdata")

# # Checks acceptance ratio
# for(seed in seed_number) {
#   cat(zach_lpsm[[paste0("seed",(seed))]]$aca/5e5)
#   cat(" ")
# }
# for(seed in seed_number) {
#   cat(zach_lpsm[[paste0("seed",(seed))]]$acz/5e5)
#   cat(" ")
# }

# Thinning the LPSM result
zach_lpsm_thin <- list()
for(seed in seed_number) {
  lpsm_single_result <- list(thinLPSM(zach_lpsm[[paste0("seed",(seed))]], burnin=5e4, thin=5e3))
  names(lpsm_single_result) <- paste0("seed",(seed))
  zach_lpsm_thin <-  append(zach_lpsm_thin, lpsm_single_result)
}
class(zach_lpsm_thin) <- "LPSM"

# save(zach_lpsm_thin, file = "zach_lpsm_thin.Rdata")

# Check diagnostic
for(seed in seed_number) {
  diagLPSM(zach_lpsm_thin[[paste0("seed",(seed))]])
}

# Running 1D LPM on zachary
zach_lpm_net1 <- ergmm(zachary ~ euclidean(d=1), seed = 115)
# save(zach_lpm_net1, file = "zach_lpm_net1.Rdata")


zach_lpm1d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(zachary ~ euclidean(d=1), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  zach_lpm1d_full <-  append(zach_lpm1d_full, lpm_single_result)
}
do.call(c, lapply(zach_lpm1d_full, function(x) summary(x)$bic$overall))

zach_lpm2d_full <- list()
for(seed in seed_number[1:10]) {
  lpm_single_result <- list(ergmm(zachary ~ euclidean(d=2), seed = seed))
  names(lpm_single_result) <- paste0("seed",(seed))
  zach_lpm2d_full <-  append(zach_lpm2d_full, lpm_single_result)
}
save(zach_lpm2d_full, file = "zach_lpm2d_full.Rdata")

load(file = "~/lpsm/inst/extdata/binary/zach_lpsm_thin.Rdata")

# Check diagnostic
zachcorr <- c()
for(seed in seed_number[1:10]) {
  zachcorr <- c(zachcorr, (protest(zach_lpm_net1$mcmc.mle$Z, zach_lpsm_thin[[paste0("seed",(seed))]]$z_mean[,1, drop=F])$scale))
}
mean(zachcorr)
sd(zachcorr)

par(mfrow=c(1,1))
plot(zach_lpsm_thin, parameter='deltas')

# open the pdf file
cairo_pdf("~/lspm/inst/figures/zachdelta.pdf", width = 3.33, height = 4)

par(mar=c(4.1, 4.1, 1, 1.1))
vioplot(cbind(zach_lpsm_thin[[paste0("seed",(seed_number[1]))]]$deltas),
        ylim=c(0,10), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.25, cex.main=1.25, cex.axis=1.25,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(xlab=expression(paste("Dimension")), cex.lab=1.25, line=2.5)
title(ylab=expression(paste("Shrinkage strength")), line=2.2, cex.lab=1.25)
for(seed in seed_number[2:10]) {
  vioplot(zach_lpsm_thin[[paste0("seed",(seed))]]$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(apply(t(as.data.frame(lapply(zach_lpsm_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")
legend("topleft", legend=c('Posterior distribution', 'Average posterior mean'), pch=c(20,20), col=c("black", "green"), cex=1)
dev.off()

par(mfrow=c(2,1))
par(mar=c(4.1, 4.1, 1, 2.1))
plot(NA, type='n', ylim=c(-1,1), xlim=c(-4.5,7), yaxt='n', ylab="", xlab="LPM Dimension 1")
text(zach_lpm_net1$mcmc.mle$Z[,1], y=rep(-.3,34),col=vertex.attributes(karate)$color, cex=1)
legend("topleft", legend = c("Mr. Hi", "John"), col=c(1,2), pch=20)

plot(NA, type='n', ylim=c(-1,1), xlim=c(-1.5,1.2), yaxt='n', ylab="", xlab="LSPM Dimension 1")
text(zach_lpsm_thin$seed126055$z_mean[,1], y=rep(-.3,34),col=vertex.attributes(karate)$color, cex=1)
legend("topleft", legend = c("Mr. Hi", "John"), col=c(1,2), pch=20)

###
# open the pdf file
cairo_pdf("~/lspm/inst/figures/zachlpm.pdf", width = 3.33, height = 4)

par(mar=c(4.1, 1.1, 1, 1.1))
plot(NA, type='n', ylim=c(-1,1), xlim=c(-7,4.5), yaxt='n', ylab="", xlab="Dimension 1", cex.lab=1.5, cex.axis=1.5)
text(zach_lpm_net1$mcmc.mle$Z[,1], y=seq(.95,-.95, length.out = 34),col=vertex.attributes(karate)$color, cex=1.2)
legend("topright", legend = c("Mr. Hi", "John"), col=c(1,2), pch=20, cex=1.2)

dev.off()

# open the pdf file
cairo_pdf("~/lspm/inst/figures/zachlspm.pdf", width = 3.33, height = 4)
par(mar=c(4.1, 1.1, 1, 1.1))
plot(NA, type='n', ylim=c(-1,1), xlim=c(-1.5,1.3), yaxt='n', ylab="", xlab="Dimension 1", cex.lab=1.5, cex.axis=1.5)
text(zach_lpsm_thin$seed126055$z_mean[,1], y=seq(.95,-.95, length.out = 34),col=vertex.attributes(karate)$color, cex=1.2)
legend("topright", legend = c("Mr. Hi", "John"), col=c(1,2), pch=20, cex=1.2)

dev.off()
### group connection
for(i in order(vertex.attributes(karate)$color)[1:16]) print(sum(zachary[,i][order(vertex.attributes(karate)$color)[17:34]]))

for(i in order(vertex.attributes(karate)$color)[17:34]) print(sum(zachary[,i][order(vertex.attributes(karate)$color)[1:16]]))
