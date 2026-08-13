# # setting seed number for run samples
# set.seed(1234)
# seed_number <- sample(1:1e6, 10)
#
# # generating 200 nodes network with 7 clusters
# sim_n200_d4 <- list()
# for(seed in seed_number) {
#   network <- list(networkMTGP_cluster(200,4,7,alpha=20,
#                                       cluster_mean = matrix(c(5,0,0,0,
#                                                               -5,5,0,0,
#                                                               0,-5,5,0,
#                                                               0,0,-5,5,
#                                                               2,0,2,-5,
#                                                               -2,2,-2,0,
#                                                               0,-2,0,0
#                                       ), ncol=4, byrow=T),
#                                       deltas =  matrix(rep(c(1,1.1,1.05,1.02), 7), nrow=7, byrow=TRUE)
#   )
#   )
#   names(network) <- paste0("seed",(seed))
#   sim_n200_d4 <- append(sim_n200_d4, network)
# }
# save(sim_n200_d4, file="sim_n200_d4.Rdata")
#
# # Checking the simulated networks
# for(seed in seed_number) {
#   # pairs(sim_n200_d4[[paste0("seed",(seed))]]$positions, col=sim_n200_d4[[paste0("seed",(seed))]]$cluster)
#   # plot(as.network(sim_n200_d4[[paste0("seed",(seed))]]$network), vertex.col=sim_n200_d4[[paste0("seed",(seed))]]$cluster, edge.col="gray")
#   cat(gden(sim_n200_d4[[paste0("seed",(seed))]]$network), " ")
#   # print(table(sim_n200_d4[[paste0("seed",(seed))]]$cluster))
# }
#
# result_n200_d4_full <- list()
# for(seed in seed_number[1:10]) {
#   lspcm_single_result <- list(LSPCM(sim_n200_d4[[paste0("seed",(seed))]]$network,
#                                         n_dimen=4, G=8,adapt_param = c(4, 5e-4),
#                                         iter=30e5, burnin=1e5, thin=4500,dim_threshold = c(0.8,0.9,5),
#                                         step_size = c(3.3,2), mix_prior = 0.05, dim2_prior = c(3,1)))
#   names(lspcm_single_result) <- paste0("seed",(seed))
#   result_n200_d4_full <-  append(result_n200_d4_full, lspcm_single_result)
# }
# class(result_n200_d4_full) <- "LSPCM"
# # save(result_n200_d4_full, file="result_n200_d4_full.Rdata")

# load("/home/scratch/15202688/lspm/inst/extdata/lspcm/sim_n200_d4.Rdata")
# load("/home/scratch/15202688/lspm/inst/extdata/lspcm/result_n200_d4_full.Rdata")

load("sim_n200_d4.Rdata")
load("result_n200_d4_full.Rdata")

# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 10)
library(mcclust)
library(mcclust.ext)
library(mclust)
library(network)
library(vegan)

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_G)), c(0.025,0.975))
)

# label switching ---------------------------------------------------------

n200p4g7_relab_full <- c()
n200p4g7_rand <- c()
for(seed in seed_number[1:10]) {
  k <- apply(result_n200_d4_full[[paste0("seed",(seed))]]$cluster,1, function(cl) length(table(cl))) # number of active cluster across iteration
  max.k <- as.numeric(names(table(k))[which.max(table(k))]) # posterior modal cluster
  n200p4g7_relab <- relabel(result_n200_d4_full[[paste0("seed",(seed))]]$cluster[k==max.k,]) # relabel condition on postterior modal cluster
  n200p4g7_relab_full <- append(n200p4g7_relab_full,list(n200p4g7_relab))
  n200p4g7_rand <- c(n200p4g7_rand, adjustedRandIndex(n200p4g7_relab$cl, sim_n200_d4[[paste0("seed",(seed))]]$cluster)) # saving data

  # plot(as.network(sim_n200_d4[[paste0("seed",(seed))]]$network), vertex.col=n200p4g7_relab$cl, edge.col="gray")
}
names(n200p4g7_relab_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index
mean(n200p4g7_rand)

# 95% adjusted rand index
quantile(n200p4g7_rand, c(0.025,0.975))

cat(paste0(round(mean(n200p4g7_rand),2), " (",
           round(quantile(n200p4g7_rand, 0.025), 2), ", ",
           round(quantile(n200p4g7_rand, 0.975), 2), ")" ))

# alternate label switching -----------------------------------------------

n200p4g7_relab_mbind_full <- c()
n200p4g7_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  n200p4g7_relab_mbind <- minbinder.ext(comp.psm(n200p4g7_relab_full[[paste0("seed",(seed))]]$cls),
                                    n200p4g7_relab_full[[paste0("seed",(seed))]]$cls,
                                   method="all", max.k = calculate_mode(result_n200_d4_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n200p4g7_relab_mbind_full <- append(n200p4g7_relab_mbind_full,list(n200p4g7_relab_mbind))
  n200p4g7_rand_mbind <- c(n200p4g7_rand_mbind, adjustedRandIndex(n200p4g7_relab_mbind$cl[1,], sim_n200_d4[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(sim_n200_d4[[paste0("seed",(seed))]]$network), vertex.col=n200p4g7_relab_mbind$cl, edge.col="gray")
}
names(n200p4g7_relab_mbind_full) <- paste0("seed",(seed_number))


# Mean adjusted rand index for mbind
mean(n200p4g7_rand_mbind)

# 95% adjusted rand index for mbind
quantile(n200p4g7_rand_mbind, c(0.025,0.975))

cat(paste0(round(mean(n200p4g7_rand_mbind),2), " (",
           round(quantile(n200p4g7_rand_mbind, 0.025), 2), ", ",
           round(quantile(n200p4g7_rand_mbind, 0.975), 2), ")" ))

n200p4g7_relab_mpear_full <- c()
n200p4g7_rand_mpear <- c()
for(seed in seed_number[1:10]) {
  n200p4g7_relab_mpear <- maxpear(comp.psm(n200p4g7_relab_full[[paste0("seed",(seed))]]$cls),
                                  n200p4g7_relab_full[[paste0("seed",(seed))]]$cls,
                                  method="all", max.k = calculate_mode(result_n200_d4_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n200p4g7_relab_mpear_full <- append(n200p4g7_relab_mpear_full,list(n200p4g7_relab_mpear))
  n200p4g7_rand_mpear <- c(n200p4g7_rand_mpear, adjustedRandIndex(n200p4g7_relab_mpear$cl[1,], sim_n200_d4[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n200p4g7_relab_mpear$cl, edge.col="gray")
}
names(n200p4g7_relab_mpear_full) <- paste0("seed",(seed_number))


# Mean adjusted rand index for mpear
mean(n200p4g7_rand_mpear)

# 95% adjusted rand index for mpear
quantile(n200p4g7_rand_mpear, c(0.025,0.975))

cat(paste0(round(mean(n200p4g7_rand_mpear),2), " (",
           round(quantile(n200p4g7_rand_mpear, 0.025), 2), ", ",
           round(quantile(n200p4g7_rand_mpear, 0.975), 2), ")" ))


n200p4g7_relab_VI_full <- c()
n200p4g7_rand_VI <- c()
for(seed in seed_number[1:10]) {
  n200p4g7_relab_VI <- minVI(comp.psm(n200p4g7_relab_full[[paste0("seed",(seed))]]$cls),
                                  n200p4g7_relab_full[[paste0("seed",(seed))]]$cls,
                                  method="all", max.k = calculate_mode(result_n200_d4_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n200p4g7_relab_VI_full <- append(n200p4g7_relab_VI_full,list(n200p4g7_relab_VI))
  n200p4g7_rand_VI <- c(n200p4g7_rand_VI, adjustedRandIndex(n200p4g7_relab_VI$cl[1,], sim_n200_d4[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n200p4g7_relab_mpear$cl, edge.col="gray")
}
names(n200p4g7_relab_VI_full) <- paste0("seed",(seed_number))


# Mean adjusted rand index for VI
mean(n200p4g7_rand_VI)

# 95% adjusted rand index for VI
quantile(n200p4g7_rand_VI, c(0.025,0.975))

cat(paste0(round(mean(n200p4g7_rand_VI),2), " (",
           round(quantile(n200p4g7_rand_VI, 0.025), 2), ", ",
           round(quantile(n200p4g7_rand_VI, 0.975), 2), ")" ))

# Cluster plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n200p4g7_cluster_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_G)))[2:4]/ length(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_G)))
        , ylim=c(0,1), xlab = expression(G), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n200p4g7_dim_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_d)))[2:3]/ length(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n200_d4_full, function(x) x$iter_d)), c(0.025,0.975))
)

# Procrustes correlation
n200p4g7_protest <- c()
for(seed in seed_number){
  n200p4g7_mean = apply(result_n200_d4_full[[paste0("seed",(seed))]]$positions, c(1,2), mean, na.rm=T)
  if(sum(is.na(n200p4g7_mean)) > 0){
    n200p4g7_protest_single <- protest(n200p4g7_mean[,1:3],
                                       sim_n200_d4[[paste0("seed",(seed))]]$positions[,1:3])$scale
  } else {
    n200p4g7_protest_single <- protest(n200p4g7_mean,
                                       sim_n200_d4[[paste0("seed",(seed))]]$positions)$scale
  }
  n200p4g7_protest <- c(n200p4g7_protest, n200p4g7_protest_single)
}

mean(n200p4g7_protest)
quantile(n200p4g7_protest, c(0.025, 0.975))


# Check diagnosis plot
for(seed in seed_number) {
  diagLSPM(result_n200_d4_full[[paste0("seed",(seed))]], n_dimen = calculate_mode(result_n200_d4_full[[paste0("seed",(seed))]]$iter_d))
}

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n200p4g7_pmd.pdf", width = 2.5, height = 3)

par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(result_n200_d4_full, parameter="deltas", n_dimen=4, true_values = c(1,1.1,1.05,1.02))
plot(result_n200_d4_full, parameter="deltas", n_dimen=3, true_values = c(1,1.1,1.05,1.02), add=T)
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n200p4g7_pmv.pdf", width = 2.5, height = 3)

par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(result_n200_d4_full, parameter="variances", n_dimen=4, true_values = 1/cumprod(c(1,1.1,1.05,1.02)))
plot(result_n200_d4_full, parameter="variances", n_dimen=3, true_values = 1/cumprod(c(1,1.1,1.05,1.02)), add=T)
legend("topright", legend=c('Posterior distribution',"True value"), pch=c(20,4), col=c("black","red"), cex=0.65)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/n200p4g7_alpha.pdf", width = 2.5, height = 3)

par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(result_n200_d4_full, parameter="alpha", n_dimen=4, true_values = 20)
plot(result_n200_d4_full, parameter="alpha", n_dimen=3, true_values = 20, add=T)
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()
plot(result_n200_d4_full, parameter="deltas", n_dimen=4, true_values = c(1,1.1,1.05,1.02))
plot(result_n200_d4_full, parameter="variances", n_dimen=4, true_values = 1/cumprod(c(1,1.1,1.05,1.02)))
plot(result_n200_d4_full, parameter="alpha", n_dimen=4, true_values = 20)


# position side by side ---------------------------------------------------
par(mfrow=c(2,2))
for(seed in seed_number[1:10]) {
  plot(sim_n200_d4[[paste0("seed",(seed))]]$positions, pch=20,
       col = sim_n200_d4[[paste0("seed",(seed))]]$cluster,
       xlab = "Dimension 1", ylab = "Dimension 2", main = paste0("True seed",(seed)))
  # plot(result_n200_d4_full[[paste0("seed",(seed))]],
  #      parameter="position",
  # n_dimen=result_n200_d4_full[[paste0("seed",(seed))]]$iter_d==calculate_mode(result_n200_d4_full[[paste0("seed",(seed))]]$iter_d),pch=20,
  #      col=n200p4g7_relab_mpear_full[[paste0("seed",(seed))]]$cl)

  n200p4g7_mean = apply(result_n200_d4_full[[paste0("seed",(seed))]]$positions[,1:2,result_n200_d4_full[[paste0("seed",(seed))]]$iter_d==calculate_mode(result_n200_d4_full[[paste0("seed",(seed))]]$iter_d)], c(1,2), mean, na.rm=T)
  n200p4g7_Yrot_single <- protest(sim_n200_d4[[paste0("seed",(seed))]]$positions,
                                  n200p4g7_mean)$Yrot

  plot(n200p4g7_Yrot_single, pch=20,
       col = n200p4g7_relab_mpear_full[[paste0("seed",(seed))]]$cl,
       xlab = "Dimension 1", ylab = "Dimension 2", main = paste0("LSPCM seed",(seed)))
}
par(mfrow=c(1,1))

# reingold side by side ---------------------------------------------------
par(mfrow=c(2,2))
for(seed in seed_number) {
  set.seed(111)
  plot(as.network(sim_n200_d4[[paste0("seed",(seed))]]$network), vertex.col = sim_n200_d4[[paste0("seed",(seed))]]$cluster, edge.col="gray", main="True")

  # k <- apply(result_n200_d4_full[[paste0("seed",(seed))]]$cluster,1, function(cl) length(table(cl))) # number of active cluster across iteration
  # max.k <- as.numeric(names(table(k))[which.max(table(k))]) # posterior modal cluster
  # n200p4g7_relab <- relabel(result_n200_d4_full[[paste0("seed",(seed))]]$cluster[k==max.k,]) # relabel condition on postterior modal cluster
  set.seed(111)
  plot(as.network(sim_n200_d4[[paste0("seed",(seed))]]$network), vertex.col = n200p4g7_relab_mpear_full[[paste0("seed",(seed))]]$cl, edge.col="gray", main="LSPCM")

}
par(mfrow=c(1,1))

# 0.005 --------------------------------------------------------------------

result_n200_d4_full_005 <- list()
for(seed in seed_number[9:10]) {
  lspcm_single_result <- list(LSPCM_asd(sim_n200_d4[[paste0("seed",(seed))]]$network,
                                    n_dimen=4, G=8,adapt_param = c(4, 5e-4),
                                    iter=30e5, burnin=1e5, thin=4500,dim_threshold = c(0.8,0.9,5),
                                    step_size = c(3.3,2), mix_prior = 0.005, dim2_prior = c(3,1)))
  names(lspcm_single_result) <- paste0("seed",(seed))
  result_n200_d4_full_005 <-  append(result_n200_d4_full_005, lspcm_single_result)
}
class(result_n200_d4_full_005) <- "LSPCM_asd"
# save(result_n200_d4_full_005, file="result_n200_d4_full_005.Rdata")

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/test.pdf", width = 10, height = 10)
# Check diagnosis plot
for(seed in seed_number) {
  # diagLSPM(result_n200_d4_full_005[[paste0("seed",(seed))]], n_dimen = calculate_mode(result_n200_d4_full_005[[paste0("seed",(seed))]]$iter_d))
  cat(calculate_mode(result_n200_d4_full_005[[paste0("seed",(seed))]]$iter_d), " ")
  cat(calculate_mode(result_n200_d4_full_005[[paste0("seed",(seed))]]$iter_G), " ")
}

dev.off()
# 0.5 --------------------------------------------------------------------

result_n200_d4_full_5 <- list()
for(seed in seed_number[8:10]) {
  lspcm_single_result <- list(LSPCM_asd(sim_n200_d4[[paste0("seed",(seed))]]$network,
                                        n_dimen=4, G=8,adapt_param = c(4, 5e-4),
                                        iter=30e5, burnin=1e5, thin=4500,dim_threshold = c(0.8,0.9,5),
                                        step_size = c(3.3,2), mix_prior = 0.5, dim2_prior = c(3,1)))
  names(lspcm_single_result) <- paste0("seed",(seed))
  result_n200_d4_full_5 <-  append(result_n200_d4_full_5, lspcm_single_result)
}
class(result_n200_d4_full_5) <- "LSPCM_asd"
# save(result_n200_d4_full_5, file="result_n200_d4_full_5.Rdata")

# Check diagnosis plot
for(seed in seed_number) {
  # diagLSPM(result_n200_d4_full_005[[paste0("seed",(seed))]], n_dimen = calculate_mode(result_n200_d4_full_005[[paste0("seed",(seed))]]$iter_d))
  cat(calculate_mode(result_n200_d4_full_5[[paste0("seed",(seed))]]$iter_d), " ")
  cat(calculate_mode(result_n200_d4_full_5[[paste0("seed",(seed))]]$iter_G), " ")
}


# 0.0005 --------------------------------------------------------------------

result_n200_d4_full_0005 <- list()
for(seed in seed_number[3:5]) {
  lspcm_single_result <- list(LSPCM_asd(sim_n200_d4[[paste0("seed",(seed))]]$network,
                                        n_dimen=4, G=8,adapt_param = c(4, 5e-4),
                                        iter=30e5, burnin=1e5, thin=4500,dim_threshold = c(0.8,0.9,5),
                                        step_size = c(3.3,2), mix_prior = 0.0005, dim2_prior = c(3,1)))
  names(lspcm_single_result) <- paste0("seed",(seed))
  result_n200_d4_full_0005 <-  append(result_n200_d4_full_0005, lspcm_single_result)
}
class(result_n200_d4_full_0005) <- "LSPCM_asd"
# save(result_n200_d4_full_0005, file="result_n200_d4_full_0005.Rdata")



# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(result_n200_d4_full_005, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n200_d4_full_005, function(x) x$iter_d)), c(0.025,0.975))
)

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(result_n200_d4_full_005, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n200_d4_full_005, function(x) x$iter_G)), c(0.025,0.975))
)
# Procrustes correlation
n200_p4g7_005_protest <- c()
for(seed in seed_number){
  # n200_p4g7_mean = apply(result_n50_d2[[paste0("seed",(seed))]]$positions[,1:2,], c(1,2), mean, na.rm=T)
  n200_p4g7_005_mean = apply(result_n200_d4_full_005[[paste0("seed",(seed))]]$positions[,1:3,result_n200_d4_full_005[[paste0("seed",(seed))]]$iter_d==3], c(1,2), mean, na.rm=T)
  n200_p4g7_005_protest_single <- protest(n200_p4g7_005_mean[,1:3],
                                        sim_n200_d4[[paste0("seed",(seed))]]$positions[,1:3])$scale
  n200_p4g7_005_protest <- c(n200_p4g7_005_protest, n200_p4g7_005_protest_single)
}

mean(n200_p4g7_005_protest)
quantile(n200_p4g7_005_protest, c(0.025, 0.975))
sd(n200_p4g7_005_protest)


# Procrustes correlation
n200_p4g7_5_protest <- c()
for(seed in seed_number){
  # n200_p4g7_mean = apply(result_n50_d2[[paste0("seed",(seed))]]$positions[,1:2,], c(1,2), mean, na.rm=T)
  n200_p4g7_5_mean = apply(result_n200_d4_full_5[[paste0("seed",(seed))]]$positions[,1:3,result_n200_d4_full_5[[paste0("seed",(seed))]]$iter_d==3], c(1,2), mean, na.rm=T)
  n200_p4g7_5_protest_single <- protest(n200_p4g7_5_mean,
                                      sim_n200_d4[[paste0("seed",(seed))]]$positions[,1:3])$scale
  n200_p4g7_5_protest <- c(n200_p4g7_5_protest, n200_p4g7_5_protest_single)
}

mean(n200_p4g7_5_protest)
quantile(n200_p4g7_5_protest, c(0.025, 0.975))
sd(n200_p4g7_5_protest)

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(result_n200_d4_full_5, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n200_d4_full_5, function(x) x$iter_d)), c(0.025,0.975))
)

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(result_n200_d4_full_5, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(result_n200_d4_full_5, function(x) x$iter_G)), c(0.025,0.975))
)




# alternate label switching -----------------------------------------------
n200_p4g7_5_relab_mbind_full <- c()
n200_p4g7_5_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  n200_p4g7_5_relab_mbind <- minbinder.ext(comp.psm(result_n200_d4_full_5[[paste0("seed",(seed))]]$cluster),
                                         result_n200_d4_full_5[[paste0("seed",(seed))]]$cluster,
                                         method="all", max.k = calculate_mode(result_n200_d4_full_5[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n200_p4g7_5_relab_mbind_full <- append(n200_p4g7_5_relab_mbind_full,list(n200_p4g7_5_relab_mbind))
  n200_p4g7_5_rand_mbind <- c(n200_p4g7_5_rand_mbind, adjustedRandIndex(n200_p4g7_5_relab_mbind$cl[1,], sim_n200_d4[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n200_p4g7_rand_mbind$cl, edge.col="gray")
}
names(n200_p4g7_5_relab_mbind_full) <- paste0("seed",(seed_number))

cat(paste0(round(mean(n200_p4g7_5_rand_mbind),2), " (",
           round(quantile(n200_p4g7_5_rand_mbind, 0.025), 2), ", ",
           round(quantile(n200_p4g7_5_rand_mbind, 0.975), 2), ")" ))


# alternate label switching -----------------------------------------------
n200_p4g7_005_relab_mbind_full <- c()
n200_p4g7_005_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  n200_p4g7_005_relab_mbind <- minbinder.ext(comp.psm(result_n200_d4_full_005[[paste0("seed",(seed))]]$cluster),
                                           result_n200_d4_full_005[[paste0("seed",(seed))]]$cluster,
                                           method="all", max.k = calculate_mode(result_n200_d4_full_5[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n200_p4g7_005_relab_mbind_full <- append(n200_p4g7_005_relab_mbind_full,list(n200_p4g7_005_relab_mbind))
  n200_p4g7_005_rand_mbind <- c(n200_p4g7_005_rand_mbind, adjustedRandIndex(n200_p4g7_005_relab_mbind$cl[1,], sim_n200_d4[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n200_p4g7_rand_mbind$cl, edge.col="gray")
}
names(n200_p4g7_005_relab_mbind_full) <- paste0("seed",(seed_number))

cat(paste0(round(mean(n200_p4g7_005_rand_mbind),2), " (",
           round(quantile(n200_p4g7_005_rand_mbind, 0.025), 2), ", ",
           round(quantile(n200_p4g7_005_rand_mbind, 0.975), 2), ")" ))



n200_p4g7_05_relab_mbind_full <- c()
n200_p4g7_05_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  n200_p4g7_05_relab_mbind <- minbinder.ext(comp.psm(result_n200_d4_full[[paste0("seed",(seed))]]$cluster),
                                           result_n200_d4_full[[paste0("seed",(seed))]]$cluster,
                                           method="all", max.k = calculate_mode(result_n200_d4_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  n200_p4g7_05_relab_mbind_full <- append(n200_p4g7_5_relab_mbind_full,list(n200_p4g7_05_relab_mbind))
  n200_p4g7_05_rand_mbind <- c(n200_p4g7_05_rand_mbind, adjustedRandIndex(n200_p4g7_05_relab_mbind$cl[1,], sim_n200_d4[[paste0("seed",(seed))]]$cluster)) # saving data
  # plot(as.network(football_trim2), vertex.col=n200_p4g7_rand_mbind$cl, edge.col="gray")
}
names(n200_p4g7_05_relab_mbind_full) <- paste0("seed",(seed_number))

cat(paste0(round(mean(n200_p4g7_05_rand_mbind),2), " (",
           round(quantile(n200_p4g7_05_rand_mbind, 0.025), 2), ", ",
           round(quantile(n200_p4g7_05_rand_mbind, 0.975), 2), ")" ))
