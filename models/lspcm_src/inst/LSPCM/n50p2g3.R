# # setting seed number for run samples
# set.seed(1234)
# seed_number <- sample(1:1e6, 10)
#
# n50p2g3seed <- c(43663,14723,20765,80416,19734,86824,39726,64074,14379,77824)
#
# generating 50 nodes network with 4 clusters
sim_n50_d2 <- list()
for(seed in seed_number) {
  network <- list(networkMTGP_cluster(50,2,3,alpha=6,
                                      cluster_mean = matrix(c(0,0,
                                                              -4,0,
                                                              -4,4
                                      ), ncol=2, byrow=T), seed = seed,
                                      deltas = matrix(rep(c(1,1.05), 3), nrow=3, byrow=TRUE)
  )
  )
  names(network) <- paste0("seed",(seed))
  sim_n50_d2 <- append(sim_n50_d2, network)
}
#
#
# Checking the simulated networks
for(seed in seed_number) {
  # pairs(sim_n50_d2[[paste0("seed",(seed))]]$positions, col=sim_n50_d2[[paste0("seed",(seed))]]$cluster)
  plot(as.network(sim_n50_d2[[paste0("seed",(seed))]]$network), vertex.col=sim_n50_d2[[paste0("seed",(seed))]]$cluster, edge.col="gray")
  cat(gden(sim_n50_d2[[paste0("seed",(seed))]]$network), " ")
  # print(table(sim_n50_d2[[paste0("seed",(seed))]]$cluster))
}

# result_n50_d2 <- list()
# for(seed in seed_number[2]) {
#   lspcm_single_result <- list(LSPCM(sim_n50_d2[[paste0("seed",(seed))]]$network,
#                                         n_dimen=4, G=10,adapt_param = c(4, 1e-5),
#                                         iter=10e5, burnin=1e5, thin=3000,dim_threshold = c(0.8,0.9,5),
#                                         step_size = c(3.3,1), mix_prior = 0.05))
#   names(lspcm_single_result) <- paste0("seed",(seed))
#   result_n50_d2 <-  append(result_n50_d2, lspcm_single_result)
# }
# class(result_n50_d2) <- "LSPCM"
# # save(result_n50_d2, file="result_n50_d2.Rdata")

# load("/home/scratch/15202688/lspm/inst/extdata/lspcm/sim_n50_d2.Rdata")
# load("/home/scratch/15202688/lspm/inst/extdata/lspcm/result_n50_d2.Rdata")

load("sim_n50_d2.Rdata")
load("result_n50_d2.Rdata")

# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 10)
library(mcclust)
library(mcclust.ext)
library(mclust)
library(network)
library(vegan)

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_G)), c(0.025,0.975))
)

# label switching ---------------------------------------------------------

n50p2g3_relab_full <- c()
n50p2g3_rand <- c()
for(seed in seed_number[1:10]) {
  k <- apply(result_n50_d2[[paste0("seed",(seed))]]$cluster,1, function(cl) length(table(cl))) # number of active cluster across iteration
  max.k <- as.numeric(names(table(k))[which.max(table(k))]) # posterior modal cluster
  n50p2g3_relab <- relabel(result_n50_d2[[paste0("seed",(seed))]]$cluster[k==max.k,]) # relabel condition on postterior modal cluster
  n50p2g3_relab_full <- append(n50p2g3_relab_full,list(n50p2g3_relab))
  n50p2g3_rand <- c(n50p2g3_rand, adjustedRandIndex(n50p2g3_relab$cl, sim_n50_d2[[paste0("seed",(seed))]]$cluster)) # saving data

  # plot(as.network(sim_n50_d2[[paste0("seed",(seed))]]$network), vertex.col=n50p2g3_relab$cl, edge.col="gray")
}
names(n50p2g3_relab_full) <- paste0("seed",(seed_number))


# Mean adjusted rand index
mean(n50p2g3_rand)

# 95% adjusted rand index
quantile(n50p2g3_rand, c(0.025,0.975))

cat(paste0(round(mean(n50p2g3_rand),2), " (",
           round(quantile(n50p2g3_rand, 0.025), 2), ", ",
           round(quantile(n50p2g3_rand, 0.975), 2), ")" ))

# alternate label switching -----------------------------------------------
n50p2g3_relab_mbind_full <- c()
n50p2g3_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  n50p2g3_relab_mbind <- minbinder.ext(comp.psm(n50p2g3_relab_full[[paste0("seed",(seed))]]$cls),
                                   n50p2g3_relab_full[[paste0("seed",(seed))]]$cls,
                                   method="all", max.k = calculate_mode(result_n50_d2[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n50p2g3_relab_mbind_full <- append(n50p2g3_relab_mbind_full,list(n50p2g3_relab_mbind))
  n50p2g3_rand_mbind <- c(n50p2g3_rand_mbind, adjustedRandIndex(n50p2g3_relab_mbind$cl[1,], sim_n50_d2[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n50p2g3_rand_mbind$cl, edge.col="gray")
}
names(n50p2g3_relab_mbind_full) <- paste0("seed",(seed_number))


# Mean adjusted rand index for mbind
mean(n50p2g3_rand_mbind)

# 95% adjusted rand index for mbind
quantile(n50p2g3_rand_mbind, c(0.025,0.975))

cat(paste0(round(mean(n50p2g3_rand_mbind),2), " (",
           round(quantile(n50p2g3_rand_mbind, 0.025), 2), ", ",
           round(quantile(n50p2g3_rand_mbind, 0.975), 2), ")" ))

n50p2g3_relab_mpear_full <- c()
n50p2g3_rand_mpear <- c()
for(seed in seed_number[1:10]) {
  n50p2g3_relab_mpear <- maxpear(comp.psm(n50p2g3_relab_full[[paste0("seed",(seed))]]$cls),
                                 n50p2g3_relab_full[[paste0("seed",(seed))]]$cls,
                                 method="all", max.k = calculate_mode(result_n50_d2[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n50p2g3_relab_mpear_full <- append(n50p2g3_relab_mpear_full,list(n50p2g3_relab_mpear))
  n50p2g3_rand_mpear <- c(n50p2g3_rand_mpear, adjustedRandIndex(n50p2g3_relab_mpear$cl[1,], sim_n50_d2[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n50p2g3_relab_mpear$cl, edge.col="gray")
}
names(n50p2g3_relab_mpear_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index for mpear
mean(n50p2g3_rand_mpear)

# 95% adjusted rand index for mpear
quantile(n50p2g3_rand_mpear, c(0.025,0.975))

cat(paste0(round(mean(n50p2g3_rand_mpear),2), " (",
           round(quantile(n50p2g3_rand_mpear, 0.025), 2), ", ",
           round(quantile(n50p2g3_rand_mpear, 0.975), 2), ")" ))

n50p2g3_relab_VI_full <- c()
n50p2g3_rand_VI <- c()
for(seed in seed_number[1:10]) {
  n50p2g3_relab_VI <- minVI(comp.psm(n50p2g3_relab_full[[paste0("seed",(seed))]]$cls),
                                 n50p2g3_relab_full[[paste0("seed",(seed))]]$cls,
                                 method="all", max.k = calculate_mode(result_n50_d2[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n50p2g3_relab_VI_full <- append(n50p2g3_relab_VI_full,list(n50p2g3_relab_VI))
  n50p2g3_rand_VI <- c(n50p2g3_rand_VI, adjustedRandIndex(n50p2g3_relab_VI$cl[1,], sim_n50_d2[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n50p2g3_relab_VI$cl, edge.col="gray")
}
names(n50p2g3_relab_VI_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index for VI
mean(n50p2g3_rand_VI)

# 95% adjusted rand index for VI
quantile(n50p2g3_rand_VI, c(0.025,0.975))

cat(paste0(round(mean(n50p2g3_rand_VI),2), " (",
           round(quantile(n50p2g3_rand_VI, 0.025), 2), ", ",
           round(quantile(n50p2g3_rand_VI, 0.975), 2), ")" ))


# Cluster plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_cluster_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_G)))/ length(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_G)))
        , ylim=c(0,1), xlab = expression(G), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_dim_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_d)))/ length(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n50_d2, function(x) x$iter_d)), c(0.025,0.975))
)

# Procrustes correlation
n50p2g3_protest <- c()
for(seed in seed_number){
  # n50p2g3_mean = apply(result_n50_d2[[paste0("seed",(seed))]]$positions[,1:2,], c(1,2), mean, na.rm=T)
  n50p2g3_mean = apply(result_n50_d2[[paste0("seed",(seed))]]$positions[,1:2,result_n50_d2[[paste0("seed",(seed))]]$iter_d==2], c(1,2), mean, na.rm=T)
  n50p2g3_protest_single <- protest(n50p2g3_mean,
                                       sim_n50_d2[[paste0("seed",(seed))]]$positions)$scale
  n50p2g3_protest <- c(n50p2g3_protest, n50p2g3_protest_single)
}

mean(n50p2g3_protest)
quantile(n50p2g3_protest, c(0.025, 0.975))
sd(n50p2g3_protest)

# Check diagnosis plot
for(seed in seed_number) {
  diagLSPM(result_n50_d2[[paste0("seed",(seed))]], n_dimen = calculate_mode(result_n50_d2[[paste0("seed",(seed))]]$iter_d))
}

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_pmd.pdf", width = 2.5, height = 3)

par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(result_n50_d2, parameter="deltas", n_dimen=2, true_values = c(.5,1.05))
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_pmv.pdf", width = 2.5, height = 3)

par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(result_n50_d2, parameter="variances", n_dimen=2, true_values = 1/cumprod(c(.5,1.05)))
legend("topleft", legend=c('Posterior distribution',"True value"), pch=c(20,4), col=c("black","red"), cex=0.75)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_alpha.pdf", width = 2.5, height = 3)

par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(result_n50_d2, parameter="alpha", n_dimen=2, true_values = 7)
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()


# Plot LSPM results for shrinkage strength, alpha = 0.
vioplot(result_n50_d2$seed761680$deltas[result_n50_d2$seed761680$iter_d==3,1:3],
        ylim=c(0,30), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(main=expression(paste(alpha, " = 0")), line=2.3, cex.main=1.5)
title(main = ("(2-5% e.d.)"),  line = 0.5, cex.main = 1.3, font.main=1)
for(seed in seed_number) {
  lapply(result_n50_d2[[paste0("seed",(seed))]], function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                           colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}

for(seed in seed_number) {
  lapply(result_n50_d2[[paste0("seed",(seed))]], function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                     colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2, cex.lab=1.75)
# for(seed in seed_number[2:15]) {
#   vioplot(node100results4d3_adapt2[[paste0("seed",(seed))]]$mcmc_chain$"4D"$deltas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
#           colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03))
# }
points(c(0.5,1.1,1.05), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node20results_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")

legend("topleft", legend=c('Posterior distribution', "True value"), pch=c(20,4), col=c("black","red"), cex=1.2)


# position side by side ---------------------------------------------------
par(mfrow=c(2,2))
for(seed in seed_number[1:10]) {
  plot(sim_n50_d2[[paste0("seed",(seed))]]$positions, pch=20,
       col = sim_n50_d2[[paste0("seed",(seed))]]$cluster,
       xlab = "Dimension 1", ylab = "Dimension 2", main = paste0("True seed",(seed)))
  # plot(result_n50_d2[[paste0("seed",(seed))]],
  #      parameter="position", n_dimen=2,pch=20,
  #      col=n50p2g3_relab_mpear_full[[paste0("seed",(seed))]]$cl)

  n50p2g3_mean = apply(result_n50_d2[[paste0("seed",(seed))]]$positions[,1:2,result_n50_d2[[paste0("seed",(seed))]]$iter_d==calculate_mode(result_n50_d2[[paste0("seed",(seed))]]$iter_d)], c(1,2), mean, na.rm=T)
  n50p2g3_Yrot_single <- protest(sim_n50_d2[[paste0("seed",(seed))]]$positions,
                                 n50p2g3_mean)$Yrot

  plot(n50p2g3_Yrot_single, pch=20,
       col = n50p2g3_relab_mpear_full[[paste0("seed",(seed))]]$cl,
       xlab = "Dimension 1", ylab = "Dimension 2", main = paste0("LSPCM seed",(seed)))
}
par(mfrow=c(1,1))

# reingold side by side ---------------------------------------------------
par(mfrow=c(2,2))
for(seed in seed_number) {
  set.seed(111)
  plot(as.network(sim_n50_d2[[paste0("seed",(seed))]]$network), vertex.col = sim_n50_d2[[paste0("seed",(seed))]]$cluster, edge.col="gray", main="True")

  # k <- apply(result_n50_d2[[paste0("seed",(seed))]]$cluster,1, function(cl) length(table(cl))) # number of active cluster across iteration
  # max.k <- as.numeric(names(table(k))[which.max(table(k))]) # posterior modal cluster
  # n50p2g3_relab <- relabel(result_n50_d2[[paste0("seed",(seed))]]$cluster[k==max.k,]) # relabel condition on postterior modal cluster
  set.seed(111)
  plot(as.network(sim_n50_d2[[paste0("seed",(seed))]]$network), vertex.col = n50p2g3_relab_mpear_full[[paste0("seed",(seed))]]$cl, edge.col="gray", main="LSPCM")

}
par(mfrow=c(1,1))

# 0.005 -------------------------------------------------------------------

result_n50_d2_005 <- list()
for(seed in seed_number[6:10]) {
  lspcm_single_result <- list(LSPCM_asd(sim_n50_d2[[paste0("seed",(seed))]]$network,
                                    n_dimen=4, G=10,adapt_param = c(5, 1e-5),
                                    iter=10e5, burnin=1e5, thin=3000,dim_threshold = c(0.8,0.9,5),
                                    step_size = c(3.3,1), mix_prior = 0.005))
  names(lspcm_single_result) <- paste0("seed",(seed))
  result_n50_d2_005 <-  append(result_n50_d2_005, lspcm_single_result)
}
class(result_n50_d2_005) <- "LSPCM_asd"
# save(result_n50_d2_005, file="result_n50_d2_005.Rdata")

# Check diagnosis plot
for(seed in seed_number) {
  diagLSPM(result_n50_d2_005[[paste0("seed",(seed))]], n_dimen = calculate_mode(result_n50_d2_005[[paste0("seed",(seed))]]$iter_d))
  # cat(calculate_mode(result_n50_d2_005[[paste0("seed",(seed))]]$iter_d), " ")
  # cat(calculate_mode(result_n50_d2_005[[paste0("seed",(seed))]]$iter_G), " ")
}

# Cluster plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_005_cluster_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_G)))/ length(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_G)))
        , ylim=c(0,1), xlab = expression(G), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_005_dim_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_d)))/ length(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# 0.5 -------------------------------------------------------------------

result_n50_d2_5 <- list()
for(seed in seed_number[7:10]) {
  lspcm_single_result <- list(LSPCM_asd(sim_n50_d2[[paste0("seed",(seed))]]$network,
                                        n_dimen=4, G=10,adapt_param = c(5, 1e-5),
                                        iter=10e5, burnin=1e5, thin=3000,dim_threshold = c(0.8,0.9,5),
                                        step_size = c(3.3,1), mix_prior = 0.5))
  names(lspcm_single_result) <- paste0("seed",(seed))
  result_n50_d2_5 <-  append(result_n50_d2_5, lspcm_single_result)
}
class(result_n50_d2_5) <- "LSPCM_asd"
# save(result_n50_d2_5, file="result_n50_d2_5.Rdata")


# Check diagnosis plot
for(seed in seed_number) {
  diagLSPM(result_n50_d2_5[[paste0("seed",(seed))]], n_dimen = calculate_mode(result_n50_d2_5[[paste0("seed",(seed))]]$iter_d))
  # cat(calculate_mode(result_n50_d2_5[[paste0("seed",(seed))]]$iter_d), " ")
  # cat(calculate_mode(result_n50_d2_5[[paste0("seed",(seed))]]$iter_G), " ")
}

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_G)), c(0.025,0.975))
)

# Cluster plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_5_cluster_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n50_d2_5, function(x) x$iter_G)))/ length(do.call(rbind, lapply(result_n50_d2_5, function(x) x$iter_G)))
        , ylim=c(0,1), xlab = expression(G), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n50p2g3_5_dim_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_d)))/ length(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n50_d2_005, function(x) x$iter_d)), c(0.025,0.975))
)

# Procrustes correlation
n50p2g3_005_protest <- c()
for(seed in seed_number){
  # n50p2g3_mean = apply(result_n50_d2[[paste0("seed",(seed))]]$positions[,1:2,], c(1,2), mean, na.rm=T)
  n50p2g3_005_mean = apply(result_n50_d2_005[[paste0("seed",(seed))]]$positions[,1:2,result_n50_d2_005[[paste0("seed",(seed))]]$iter_d==2], c(1,2), mean, na.rm=T)
  n50p2g3_005_protest_single <- protest(n50p2g3_005_mean,
                                    sim_n50_d2[[paste0("seed",(seed))]]$positions)$scale
  n50p2g3_005_protest <- c(n50p2g3_005_protest, n50p2g3_005_protest_single)
}

mean(n50p2g3_005_protest)
quantile(n50p2g3_005_protest, c(0.025, 0.975))
sd(n50p2g3_005_protest)


# Procrustes correlation
n50p2g3_5_protest <- c()
for(seed in seed_number){
  # n50p2g3_mean = apply(result_n50_d2[[paste0("seed",(seed))]]$positions[,1:2,], c(1,2), mean, na.rm=T)
  n50p2g3_5_mean = apply(result_n50_d2_5[[paste0("seed",(seed))]]$positions[,1:2,result_n50_d2_5[[paste0("seed",(seed))]]$iter_d==2], c(1,2), mean, na.rm=T)
  n50p2g3_5_protest_single <- protest(n50p2g3_5_mean,
                                        sim_n50_d2[[paste0("seed",(seed))]]$positions)$scale
  n50p2g3_5_protest <- c(n50p2g3_5_protest, n50p2g3_5_protest_single)
}

mean(n50p2g3_5_protest)
quantile(n50p2g3_5_protest, c(0.025, 0.975))
sd(n50p2g3_5_protest)

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(result_n50_d2_5, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n50_d2_5, function(x) x$iter_d)), c(0.025,0.975))
)

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(result_n50_d2_5, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n50_d2_5, function(x) x$iter_G)), c(0.025,0.975))
)




# alternate label switching -----------------------------------------------
n50p2g3_5_relab_mbind_full <- c()
n50p2g3_5_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  n50p2g3_5_relab_mbind <- minbinder.ext(comp.psm(result_n50_d2_5[[paste0("seed",(seed))]]$cluster),
                                         result_n50_d2_5[[paste0("seed",(seed))]]$cluster,
                                       method="all", max.k = calculate_mode(result_n50_d2_5[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n50p2g3_5_relab_mbind_full <- append(n50p2g3_5_relab_mbind_full,list(n50p2g3_5_relab_mbind))
  n50p2g3_5_rand_mbind <- c(n50p2g3_5_rand_mbind, adjustedRandIndex(n50p2g3_5_relab_mbind$cl[1,], sim_n50_d2[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n50p2g3_rand_mbind$cl, edge.col="gray")
}
names(n50p2g3_5_relab_mbind_full) <- paste0("seed",(seed_number))

cat(paste0(round(mean(n50p2g3_5_rand_mbind),2), " (",
           round(quantile(n50p2g3_5_rand_mbind, 0.025), 2), ", ",
           round(quantile(n50p2g3_5_rand_mbind, 0.975), 2), ")" ))


# alternate label switching -----------------------------------------------
n50p2g3_005_relab_mbind_full <- c()
n50p2g3_005_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  n50p2g3_005_relab_mbind <- minbinder.ext(comp.psm(result_n50_d2_005[[paste0("seed",(seed))]]$cluster),
                                         result_n50_d2_005[[paste0("seed",(seed))]]$cluster,
                                         method="all", max.k = calculate_mode(result_n50_d2_5[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n50p2g3_005_relab_mbind_full <- append(n50p2g3_005_relab_mbind_full,list(n50p2g3_005_relab_mbind))
  n50p2g3_005_rand_mbind <- c(n50p2g3_005_rand_mbind, adjustedRandIndex(n50p2g3_005_relab_mbind$cl[1,], sim_n50_d2[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n50p2g3_rand_mbind$cl, edge.col="gray")
}
names(n50p2g3_005_relab_mbind_full) <- paste0("seed",(seed_number))

cat(paste0(round(mean(n50p2g3_005_rand_mbind),2), " (",
           round(quantile(n50p2g3_005_rand_mbind, 0.025), 2), ", ",
           round(quantile(n50p2g3_005_rand_mbind, 0.975), 2), ")" ))



# lpcm --------------------------------------------------------------------


system.time({ # seed871806
  sim_n50_d2_lpcm_1d1g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=1), seed = 111)
sim_n50_d2_lpcm_1d2g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=2), seed = 111)
sim_n50_d2_lpcm_1d3g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=3), seed = 111)
sim_n50_d2_lpcm_1d4g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=4), seed = 111)

sim_n50_d2_lpcm_2d1g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=1), seed = 111)
sim_n50_d2_lpcm_2d2g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=2), seed = 111)
sim_n50_d2_lpcm_2d3g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=3), seed = 111)
sim_n50_d2_lpcm_2d4g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=4), seed = 111)

sim_n50_d2_lpcm_3d1g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=1), seed = 111)
sim_n50_d2_lpcm_3d2g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=2), seed = 111)
sim_n50_d2_lpcm_3d3g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=3), seed = 111)
sim_n50_d2_lpcm_3d4g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=4), seed = 111)

sim_n50_d2_lpcm_4d1g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=1), seed = 111)
sim_n50_d2_lpcm_4d2g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=2), seed = 111)
sim_n50_d2_lpcm_4d3g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=3), seed = 111)
sim_n50_d2_lpcm_4d4g <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=4), seed = 111)
})
# user  system elapsed
# 680.310   0.207 682.023

system.time({ # seed761680
  sim_n50_d2_lpcm_1d1g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=1), seed = 111)
  sim_n50_d2_lpcm_1d2g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=2), seed = 111)
  sim_n50_d2_lpcm_1d3g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=3), seed = 111)
  sim_n50_d2_lpcm_1d4g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=4), seed = 111)

  sim_n50_d2_lpcm_2d1g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=1), seed = 111)
  sim_n50_d2_lpcm_2d2g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=2), seed = 111)
  sim_n50_d2_lpcm_2d3g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=3), seed = 111)
  sim_n50_d2_lpcm_2d4g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=4), seed = 111)

  sim_n50_d2_lpcm_3d1g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=1), seed = 111)
  sim_n50_d2_lpcm_3d2g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=2), seed = 111)
  sim_n50_d2_lpcm_3d3g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=3), seed = 111)
  sim_n50_d2_lpcm_3d4g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=4), seed = 111)

  sim_n50_d2_lpcm_4d1g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=1), seed = 111)
  sim_n50_d2_lpcm_4d2g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=2), seed = 111)
  sim_n50_d2_lpcm_4d3g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=3), seed = 111)
  sim_n50_d2_lpcm_4d4g_0 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=4), seed = 111)
})
# user  system elapsed
# 728.472   0.448 730.477

system.time({ # seed630678
  sim_n50_d2_lpcm_1d1g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=1), seed = 111)
  sim_n50_d2_lpcm_1d2g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=2), seed = 111)
  sim_n50_d2_lpcm_1d3g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=3), seed = 111)
  sim_n50_d2_lpcm_1d4g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=4), seed = 111)

  sim_n50_d2_lpcm_2d1g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=1), seed = 111)
  sim_n50_d2_lpcm_2d2g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=2), seed = 111)
  sim_n50_d2_lpcm_2d3g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=3), seed = 111)
  sim_n50_d2_lpcm_2d4g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=4), seed = 111)

  sim_n50_d2_lpcm_3d1g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=1), seed = 111)
  sim_n50_d2_lpcm_3d2g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=2), seed = 111)
  sim_n50_d2_lpcm_3d3g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=3), seed = 111)
  sim_n50_d2_lpcm_3d4g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=4), seed = 111)

  sim_n50_d2_lpcm_4d1g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=1), seed = 111)
  sim_n50_d2_lpcm_4d2g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=2), seed = 111)
  sim_n50_d2_lpcm_4d3g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=3), seed = 111)
  sim_n50_d2_lpcm_4d4g_1 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=4), seed = 111)
})
# user  system elapsed
# 789.976   2.439 794.107

system.time({ # seed304108
  sim_n50_d2_lpcm_1d1g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=1), seed = 111)
  sim_n50_d2_lpcm_1d2g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=2), seed = 111)
  sim_n50_d2_lpcm_1d3g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=3), seed = 111)
  sim_n50_d2_lpcm_1d4g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=4), seed = 111)

  sim_n50_d2_lpcm_2d1g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=1), seed = 111)
  sim_n50_d2_lpcm_2d2g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=2), seed = 111)
  sim_n50_d2_lpcm_2d3g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=3), seed = 111)
  sim_n50_d2_lpcm_2d4g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=4), seed = 111)

  sim_n50_d2_lpcm_3d1g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=1), seed = 111)
  sim_n50_d2_lpcm_3d2g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=2), seed = 111)
  sim_n50_d2_lpcm_3d3g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=3), seed = 111)
  sim_n50_d2_lpcm_3d4g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=4), seed = 111)

  sim_n50_d2_lpcm_4d1g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=1), seed = 111)
  sim_n50_d2_lpcm_4d2g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=2), seed = 111)
  sim_n50_d2_lpcm_4d3g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=3), seed = 111)
  sim_n50_d2_lpcm_4d4g_2 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=4), seed = 111)
})
# user  system elapsed
# 787.758   6.181 795.662

system.time({ # seed932745
  sim_n50_d2_lpcm_1d1g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=1), seed = 111)
  sim_n50_d2_lpcm_1d2g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=2), seed = 111)
  sim_n50_d2_lpcm_1d3g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=3), seed = 111)
  sim_n50_d2_lpcm_1d4g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=4), seed = 111)

  sim_n50_d2_lpcm_2d1g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=1), seed = 111)
  sim_n50_d2_lpcm_2d2g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=2), seed = 111)
  sim_n50_d2_lpcm_2d3g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=3), seed = 111)
  sim_n50_d2_lpcm_2d4g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=4), seed = 111)

  sim_n50_d2_lpcm_3d1g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=1), seed = 111)
  sim_n50_d2_lpcm_3d2g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=2), seed = 111)
  sim_n50_d2_lpcm_3d3g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=3), seed = 111)
  sim_n50_d2_lpcm_3d4g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=4), seed = 111)

  sim_n50_d2_lpcm_4d1g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=1), seed = 111)
  sim_n50_d2_lpcm_4d2g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=2), seed = 111)
  sim_n50_d2_lpcm_4d3g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=3), seed = 111)
  sim_n50_d2_lpcm_4d4g_3 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=4), seed = 111)
})
# user  system elapsed
# 632.685   2.728 636.857

system.time({ # seed295846
  sim_n50_d2_lpcm_1d1g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=1), seed = 111)
  sim_n50_d2_lpcm_1d2g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=2), seed = 111)
  sim_n50_d2_lpcm_1d3g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=3), seed = 111)
  sim_n50_d2_lpcm_1d4g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=4), seed = 111)

  sim_n50_d2_lpcm_2d1g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=1), seed = 111)
  sim_n50_d2_lpcm_2d2g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=2), seed = 111)
  sim_n50_d2_lpcm_2d3g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=3), seed = 111)
  sim_n50_d2_lpcm_2d4g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=4), seed = 111)

  sim_n50_d2_lpcm_3d1g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=1), seed = 111)
  sim_n50_d2_lpcm_3d2g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=2), seed = 111)
  sim_n50_d2_lpcm_3d3g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=3), seed = 111)
  sim_n50_d2_lpcm_3d4g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=4), seed = 111)

  sim_n50_d2_lpcm_4d1g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=1), seed = 111)
  sim_n50_d2_lpcm_4d2g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=2), seed = 111)
  sim_n50_d2_lpcm_4d3g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=3), seed = 111)
  sim_n50_d2_lpcm_4d4g_4 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=4), seed = 111)
})
# user  system elapsed
# 736.568   1.592 739.766

system.time({ # seed126055
  sim_n50_d2_lpcm_1d1g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=1), seed = 111)
  sim_n50_d2_lpcm_1d2g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=2), seed = 111)
  sim_n50_d2_lpcm_1d3g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=3), seed = 111)
  sim_n50_d2_lpcm_1d4g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=4), seed = 111)
  sim_n50_d2_lpcm_1d5g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=1, G=5), seed = 111)

  sim_n50_d2_lpcm_2d1g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=1), seed = 111)
  sim_n50_d2_lpcm_2d2g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=2), seed = 111)
  sim_n50_d2_lpcm_2d3g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=3), seed = 111)
  sim_n50_d2_lpcm_2d4g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=4), seed = 111)
  sim_n50_d2_lpcm_2d5g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=2, G=5), seed = 111)

  sim_n50_d2_lpcm_3d1g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=1), seed = 111)
  sim_n50_d2_lpcm_3d2g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=2), seed = 111)
  sim_n50_d2_lpcm_3d3g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=3), seed = 111)
  sim_n50_d2_lpcm_3d4g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=4), seed = 111)
  sim_n50_d2_lpcm_3d5g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=3, G=5), seed = 111)

  sim_n50_d2_lpcm_4d1g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=1), seed = 111)
  sim_n50_d2_lpcm_4d2g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=2), seed = 111)
  sim_n50_d2_lpcm_4d3g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=3), seed = 111)
  sim_n50_d2_lpcm_4d4g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=4), seed = 111)
  sim_n50_d2_lpcm_4d5g_5 <- ergmm(sim_n50_d2[[paste0("seed",(seed))]]$network ~ euclidean(d=4, G=5), seed = 111)
})


system.time(  lspcm_sim_n50_d2_time1 <- LSPCM(sim_n50_d2[[paste0("seed",(seed))]]$network,
                                              n_dimen=4, G=25,adapt_param = c(4, 1e-5),
                                              iter=10e4, burnin=1e4, thin=300,dim_threshold = c(0.8,0.9,5),
                                              step_size = c(3.3,1), mix_prior = 0.01))
# user   system  elapsed
# 4772.406   22.919 4805.002


system.time(  lspcm_sim_n50_d2_time10 <- LSPCM(sim_n50_d2[[paste0("seed",(seed))]]$network,
                                              n_dimen=4, G=25,adapt_param = c(4, 1e-5),
                                              iter=10e4, burnin=1e4, thin=300,dim_threshold = c(0.8,0.9,5),
                                              step_size = c(3.3,1), mix_prior = 0.1))

system.time(  lspcm_sim_n50_d2_time01 <- LSPCM(sim_n50_d2[[paste0("seed",(seed))]]$network,
                                              n_dimen=4, G=25,adapt_param = c(4, 1e-5),
                                              iter=10e4, burnin=1e4, thin=300,dim_threshold = c(0.8,0.9,5),
                                              step_size = c(3.3,1), mix_prior = 0.001))


system.time(  lspcm_single_result <- list(LSPCM(sim_n50_d2[[paste0("seed",(seed))]]$network,
                                                 n_dimen=5, G=25,adapt_param = c(4, 5e-4),
                                                 iter=5e5, burnin=5e4, thin=1000,dim_threshold = c(0.8,0.9,5),
                                                 step_size = c(3.3,2), mix_prior = 0.002)))

result_n50_d2_full <- list()
for(seed in seed_number[8:10]) {
  system.time(lspcm_single_result <- list(LSPCM(sim_n50_d2[[paste0("seed",(seed))]]$network,
                                                  n_dimen=5, G=25,adapt_param = c(4, 5e-4),
                                                  iter=5e5, burnin=5e4, thin=1000,dim_threshold = c(0.8,0.9,5),
                                                  step_size = c(3.3,3), mix_prior = 0.002)))
  names(lspcm_single_result) <- paste0("seed",(seed))
  result_n50_d2_full <-  append(result_n50_d2_full, lspcm_single_result)
}
class(result_n50_d2_full) <- "LSPCM"
plot(result_n50_d2_full$seed761680, parameter = "dimension")
diagLSPM(result_n50_d2_full$seed761680, n_dimen=2)
