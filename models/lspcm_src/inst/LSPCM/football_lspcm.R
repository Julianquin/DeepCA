#
# # Football data -----------------------------------------------------------
#
# # Read in football data
# football.follows <- read.csv("~/lspm/inst/LSPCM/football-mentions.mtx", sep="")
# football_adj <- as.matrix.network.adjacency(as.network(football.follows))
#
# # Stoke players ID
# stoke <- c(216264820,44335177,336082762,637041761,52079636,91388229,282634845,136035018,48467334,304182081,482237272,343626364,287202982,51597791,41684671)
# spurs <- c(377037875,151584475,226347105,197001564,178915141,230013639,472185221,241065455,507489702,188417619,455237361,142702489,714421232,121402638,293559894,237313700,362523468,123080710,469753464,431429137,373555025,429247237,385463372)
# west_brom <- c(138846688,233899912,451693376,369758604,153406657,571021282,148680848,471271538,304953074,581281429,456021856,280665816,240869466,77001859,280412626,240002233,406971569)
#
# # c(571021282, 148680848, 471271538, 456021856, 240869466,240002233) # unknown west_brom
# # 148680848 outlier west_brom Romelu Lukaku only connect to  507489702 Janvertonghen
# # 431429137 misclassified from spurs to stoke Clint Dempsey prob. 0.43095238    0 0.06011905    0 0.5089286
# # clint 51597791  91388229 121402638 469753464 714421232  2stoke 3spurs mention by
# # clint 51597791  91388229 121402638 188417619 429247237 469753464 714421232 2stoke 5spurs mention to
#
# # Extract the club with the top 3 most players
# football_trim2 = football_adj[rownames(football_adj) %in% c(stoke, spurs, west_brom),
#                               colnames(football_adj) %in% c(stoke, spurs, west_brom)]
#
# # Create labels for the club
# football_group2 <- rep(0,dim(football_trim2)[1])
# football_group2 <- football_group2 + (colnames(football_trim2) %in% stoke) # 1 for stoke
# football_group2 <- football_group2 + (colnames(football_trim2) %in% spurs*2) # 2 for spurs
# football_group2 <- football_group2 + (colnames(football_trim2) %in% west_brom*3) # 3 for west-brom
#
# # Visualise the network with Fruchterman-Reingold
# plot(as.network(football_trim2), vertex.col=football_group2, edge.col="gray")
#
# # # Saving the network and labels
# # save(football_trim2, file="football_trim2.Rdata")
# # save(football_group2, file="football_group2.Rdata")
#
#
# # LSPCM results -----------------------------------------------------------
#
# # setting seed number for run samples
# set.seed(1234)
# seed_number <- sample(1:1e6, 10)
#
# # LSPCM
# football2_lspcm_full <- list()
# for(seed in seed_number[1:10]) {
#   lspcm_single_result <- list(LSPCM(football_trim2,
#                                         n_dimen=4, G=10,adapt_param = c(4, 1e-4),
#                                         iter=20e5, burnin=1e5, thin=4000,dim_threshold = c(0.8,0.9,5),
#                                         step_size = c(3.3,2), mix_prior = 0.05, dim2_prior = c(3,1)))
#   names(lspcm_single_result) <- paste0("seed",(seed))
#   football2_lspcm_full <-  append(football2_lspcm_full, lspcm_single_result)
# }
# class(football2_lspcm_full) <- "LSPCM"
# # save(football2_lspcm_full, file="football2_lspcm_full.Rdata")

load("football_group2.Rdata")
load("football_trim2.Rdata")
load("football2_lspcm_full.Rdata")

# setting seed number for run samples
set.seed(1234)
seed_number <- sample(1:1e6, 10)
library(mcclust)
library(mcclust.ext)
library(mclust)
library(network)
library(vegan)

# Cluster mode
rbind(calculate_mode(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_G)))
)

# Cluster 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_G)), c(0.025,0.975))
)

# label switching ---------------------------------------------------------
football2_relab_full <- c()
football2_rand <- c()
for(seed in seed_number[1:10]) {
  k <- apply(football2_lspcm_full[[paste0("seed",(seed))]]$cluster,1, function(cl) length(table(cl))) # number of active cluster across iteration
  max.k <- as.numeric(names(table(k))[which.max(table(k))]) # posterior modal cluster
  football2_relab <- relabel(football2_lspcm_full[[paste0("seed",(seed))]]$cluster[k==max.k,]) # relabel condition on postterior modal cluster
  football2_relab_full <- append(football2_relab_full,list(football2_relab))
  football2_rand <- c(football2_rand, adjustedRandIndex(football2_relab$cl, football_group2)) # saving data

  # plot(as.network(football_trim2), vertex.col=football2_relab$cl, edge.col="gray")
}
names(football2_relab_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index
mean(football2_rand)

# 95% adjusted rand index
quantile(football2_rand, c(0.025,0.975))

cat(paste0(round(mean(football2_rand),2), " (",
           round(quantile(football2_rand, 0.025), 2), ", ",
           round(quantile(football2_rand, 0.975), 2), ")" ))



# alternate label switching -----------------------------------------------

football2_relab_mbind_full <- c()
football2_rand_mbind <- c()
for(seed in seed_number[1:10]) {
  football2_relab_mbind <- minbinder.ext(comp.psm(football2_relab_full[[paste0("seed",(seed))]]$cls),
                                   football2_relab_full[[paste0("seed",(seed))]]$cls,
                                   method="all", max.k = calculate_mode(football2_lspcm_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mbind
  football2_relab_mbind_full <- append(football2_relab_mbind_full,list(football2_relab_mbind))
  football2_rand_mbind <- c(football2_rand_mbind, adjustedRandIndex(football2_relab_mbind$cl[1,], football_group2)) # saving data
  # plot(as.network(football_trim2), vertex.col=football2_relab_mbind$cl, edge.col="gray")
}
names(football2_relab_mbind_full) <- paste0("seed",(seed_number))


# Mean adjusted rand index for mbind
median(football2_rand_mbind)

# 95% adjusted rand index for mbind
quantile(football2_rand_mbind, c(0.025,0.975))

cat(paste0(round(mean(football2_rand_mbind),2), " (",
           round(quantile(football2_rand_mbind, 0.025), 2), ", ",
           round(quantile(football2_rand_mbind, 0.975), 2), ")" ))

football2_relab_mpear_full <- c()
football2_rand_mpear <- c()
for(seed in seed_number[1:10]) {
  football2_relab_mpear <- maxpear(comp.psm(football2_relab_full[[paste0("seed",(seed))]]$cls),
                                   football2_relab_full[[paste0("seed",(seed))]]$cls,
                                   method="all", max.k = calculate_mode(football2_lspcm_full[[paste0("seed",(seed))]]$iter_G)) # relabel via mpear
  football2_relab_mpear_full <- append(football2_relab_mpear_full,list(football2_relab_mpear))
  football2_rand_mpear <- c(football2_rand_mpear, adjustedRandIndex(football2_relab_mpear$cl[1,], football_group2)) # saving data
  # plot(as.network(football_trim2), vertex.col=football2_relab_mpear$cl, edge.col="gray")
}
names(football2_relab_mpear_full) <- paste0("seed",(seed_number))

# Mean adjusted rand index for mpear
mean(football2_rand_mpear)

# 95% adjusted rand index for mpear
quantile(football2_rand_mpear, c(0.025,0.975))

cat(paste0(round(mean(football2_rand_mpear),2), " (",
           round(quantile(football2_rand_mpear, 0.025), 2), ", ",
           round(quantile(football2_rand_mpear, 0.975), 2), ")" ))


football2_rand_VI_full <- c()
football2_rand_VI <- c()
for(seed in seed_number[1:10]) {
  football2_relab_VI <- minVI(comp.psm(football2_relab_full[[paste0("seed",(seed))]]$cls),
                                   football2_relab_full[[paste0("seed",(seed))]]$cls,
                                   method="all", max.k = calculate_mode(football2_lspcm_full[[paste0("seed",(seed))]]$iter_G)) # relabel via minVI
  football2_rand_VI_full <- append(football2_rand_VI_full,list(football2_relab_mpear))
  football2_rand_VI <- c(football2_rand_VI, adjustedRandIndex(football2_relab_VI$cl[1,], football_group2)) # saving data
  # plot(as.network(football_trim2), vertex.col=football2_relab_mpear$cl, edge.col="gray")
}
names(football2_rand_VI_full) <- paste0("seed",(seed_number))


# Mean adjusted rand index for VI
mean(football2_rand_VI)

# 95% adjusted rand index for VI
quantile(football2_rand_VI, c(0.025,0.975))

cat(paste0(round(mean(football2_rand_VI),2), " (",
           round(quantile(football2_rand_VI, 0.025), 2), ", ",
           round(quantile(football2_rand_VI, 0.975), 2), ")" ))


# Cluster plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_cluster_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_G)))/ length(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_G)))
        , ylim=c(0,1), xlab = expression(G), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension plot ----------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_dim_bar.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 1, 0.5))

barplot(table(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_d)))/ length(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

dev.off()


# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(football2_lspcm_full, function(x) x$iter_d)), c(0.025,0.975))
)





k = apply(do.call(rbind,lapply(football2_lspcm_full, function(x) x$cluster)), 1, function(cl) length(table(cl)))
max.k <- as.numeric(names(table(k))[which.max(table(k))])
football2_relab_combined <- relabel(do.call(rbind,lapply(football2_lspcm_full, function(x) x$cluster))[k==max.k,])
football2_mbind_combined <- minbinder.ext(comp.psm(do.call(rbind,lapply(football2_lspcm_full, function(x) x$cluster))))
football2_mbind_combined$cl[football2_mbind_combined$cl == 3] = 4
football2_mbind_combined$cl[football2_mbind_combined$cl == 2] = 3
football2_mbind_combined$cl[football2_mbind_combined$cl == 4] = 2


# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_true.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(0.1, 0.1, 0.1, 0.1))
custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(custom_palette)
# par(mfrow=c(1,2))
set.seed(111)
plot(as.network(football_trim2), vertex.col = football_group2, edge.col="gray", vertex.cex=1.5, vertex.border="black") # true cluster
legend("topleft", legend=c('Stoke', "Spurs", "West-brom"), pch=20, col=1:3, cex=1)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_lspcm.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(0.1, 0.1, 0.1, 0.1))
custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(custom_palette)
set.seed(111)
# plot(as.network(football_trim2), vertex.col = football2_relab_combined$cl/2, edge.col="gray", vertex.cex=1.5) # lspcm combined cluster
plot(as.network(football_trim2), vertex.col = football2_mbind_combined$cl, edge.col="gray", vertex.cex=1.5, vertex.border="black") # lspcm combined cluster
legend("topleft", legend=c("Cluster 1", "Cluster 2", "Cluster 3"), pch=20, col=1:3, cex=1)
dev.off()
par(mfrow=c(1,1))


cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_lspcm_pos.pdf", width = 2.5, height = 3)
custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette(custom_palette)
par(mfrow=c(1,1))
par(mar = c(4, 4.2, .8, 0.5))
# plot(apply(do.call(abind, lapply(football2_lspcm_full, function(x) x$positions)), c(1,2), mean, na.rm=T),
#      col=football2_relab_combined$cl/2, pch=20, cex=1.3 ,
#      xlim=c(-4.8,3), ylim=c(-3.2,3.5), xlab="Dimension 1", ylab="Dimension 2")
football_position_combined <- apply(do.call(abind, lapply(football2_lspcm_full, function(x) x$positions[,,x$iter_d == calculate_mode(x$iter_d)])), c(1,2), mean, na.rm=T)
football_position_combined[,1] <- football_position_combined[,1] * -1 # to match fruchterman
# plot(ie_position_combined,
#      col=football2_mbind_combined$cl, pch=20, cex=1.3 ,
#      xlim=c(-4.8,3), ylim=c(-3.2,3.5), xlab="Dimension 1", ylab="Dimension 2")
plot(football_position_combined,
     col=football2_mbind_combined$cl, pch=20, cex=1.3 ,
     xlim=c(-3,4.8), ylim=c(-3.2,3.5), xlab="Dimension 1", ylab="Dimension 2")
segments(football_position_combined[row(football_trim2)[which(football_trim2 != 0)],1],
         football_position_combined[row(football_trim2)[which(football_trim2 != 0)],2],
         football_position_combined[col(football_trim2)[which(football_trim2 != 0)],1],
         football_position_combined[col(football_trim2)[which(football_trim2 != 0)],2],
         lwd = 1, col = adjustcolor("grey50", 0.2))
points(football_position_combined[,c(1,2)], #ylim=c(-2,3), xlim=c(-2,3),
       col=football2_mbind_combined$cl, pch=20, cex=1.3)

legend("topright", legend=c("Cluster 1", "Cluster 2", "Cluster 3"), pch=20, col=1:3, cex=0.7)
dev.off()


# LPCM --------------------------------------------------------------------

football_lpcm_1d3g = ergmm(football_trim2 ~ euclidean(d=1, G=3), seed = 111)
football_lpcm_2d3g = ergmm(football_trim2 ~ euclidean(d=2, G=3), seed = 111)
football_lpcm_3d3g = ergmm(football_trim2 ~ euclidean(d=3, G=3), seed = 111)

summary(football_lpcm_1d3g)$bic$overall
summary(football_lpcm_2d3g)$bic$overall
summary(football_lpcm_3d3g)$bic$overall

football_lpcm_2d7g = ergmm(football_trim2 ~ euclidean(d=2, G=7), seed = 111)
football_lpcm_2d6g = ergmm(football_trim2 ~ euclidean(d=2, G=6), seed = 111)
football_lpcm_2d5g = ergmm(football_trim2 ~ euclidean(d=2, G=5), seed = 111)
football_lpcm_2d4g = ergmm(football_trim2 ~ euclidean(d=2, G=4), seed = 111)
football_lpcm_2d2g = ergmm(football_trim2 ~ euclidean(d=2, G=2), seed = 111)

summary(football_lpcm_2d7g)$bic$overall
summary(football_lpcm_2d6g)$bic$overall
summary(football_lpcm_2d5g)$bic$overall
summary(football_lpcm_2d4g)$bic$overall
summary(football_lpcm_2d3g)$bic$overall
summary(football_lpcm_2d2g)$bic$overall


# Procrustes correlation
football_protest <- c()
for(seed in seed_number){
  football_pos_mean = apply(football2_lspcm_full[[paste0("seed",(seed))]]$positions[,1:2,], c(1,2), mean, na.rm=T)
  football_protest_single <- protest(football_pos_mean,
                                    football_lpcm_2d3g$mcmc.pmode$Z)$scale
  football_protest <- c(football_protest, football_protest_single)
}

mean(football_protest)
quantile(football_protest, c(0.025, 0.975))
sd(football_protest)



# violin plots ------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_pmd.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(football2_lspcm_full, parameter="deltas", n_dimen=2)
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_pmv.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(football2_lspcm_full, parameter="variances", n_dimen=2)
legend("topleft", legend=c('Posterior distribution'), pch=c(20), col=c("black"), cex=0.75)
dev.off()

cairo_pdf("~/lspm/inst/extdata/lspcm/figure/football_alpha.pdf", width = 2.5, height = 3)
par(mfrow=c(1,1))
par(mar = c(3.3, 3.7, 1, 0.5))
plot(football2_lspcm_full, parameter="alpha", n_dimen=2)
# legend("topleft", legend=c('Posterior distribution', "Average posterior mean","True value"), pch=c(20,20,4), col=c("black","green3","red"), cex=0.75)
dev.off()



# LPCM --------------------------------------------------------------------

# Fitting LPCM
system.time({ football_lpcm_full <- list()
for(seed in seed_number) {
  lpcm_single_result <- list(ergmm(football_trim2 ~ euclidean(d=2, G=3), seed=seed))
  names(lpcm_single_result) <- paste0("seed",(seed))
  football_lpcm_full <-  append(football_lpcm_full, lpcm_single_result)
}
})
football_lpcm_ari <- do.call(c, lapply(football_lpcm_full, function(x) adjustedRandIndex(football_group2, x$mcmc.pmode$Z.K)))

median(football_lpcm_ari)
quantile(football_lpcm_ari, c(0.025, 0.975))
# user  system elapsed
# 397.889   1.763 400.619
