load(file = "~/lspm/inst/extdata/count/count2d0_result_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/count2d1_result_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/count2d5_result_thin.Rdata")

source("~/lspm/inst/section4_4network_c.R")

# R2 plot -----------------------------------------------------------------

library(vioplot)
# sapply(count2d0_result_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network))
# sapply(count2d1_result_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network))
# sapply(count2d5_result_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network))
collection_of_R2 = cbind(sapply(count2d0_result_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network)),
                         sapply(count2d1_result_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network)),
                         sapply(count2d5_result_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network)))
colnames(collection_of_R2) = c("Slight", "Moderate", "High")
vioplot(collection_of_R2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5, ylim=c(0,1),
        colMed = NA, border=NA, col="black")
# legend("bottomright", legend = c("LSPM3", "LSPM4", "LSPM8"), col=c(rgb(red=106/255, green = 13/255, blue = 173/255), "magenta3", "orange"), pch=20)
mtext(expression(paste("Deviance ", R^2)), side=2, line=2.5, cex=1.5)



# Individual curve (not used) ---------------------------------------------
#
# plot(density(as.matrix(dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col='red', main="3D", xlab="Pairwise distance", ylim=c(0,0.15), lwd=3)
# lines(density(as.matrix(dist(count2d0_result_thin$seed761680$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col=rgb(red=106/255, green = 13/255, blue = 173/255), lty=2, lwd=3)
# lines(density(as.matrix(dist(count2d1_result_thin$seed761680$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col='magenta3', lty=2, lwd=3)
# lines(density(as.matrix(dist(count2d5_result_thin$seed761680$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col='orange', lty=2, lwd=3)



# Pairwise estimated over true curve --------------------------------------

plot(density(as.vector(as.matrix((dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/(as.matrix(dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=0.03),
     type="n",col='red', main="", ylab="",xlab="Ratio of pairwise distance", ylim=c(0,15), lwd=1, xlim=c(0,4), cex.sub=2, cex.axis=1.5, cex.lab=1.5)
mtext("Density", side=2, line=2.5, cex=1.5)
for(seed in seed_number) {
  lines(density(as.vector((as.matrix(dist(count2d0_result_thin[[paste0("seed",(seed))]]$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/as.matrix((dist(count2d0_network[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=.03), col=rgb(red=106/255, green = 13/255, blue = 173/255), main="3D", xlab="Pairwise distance", lwd=.3)
  lines(density(as.vector((as.matrix(dist(count2d1_result_thin[[paste0("seed",(seed))]]$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/as.matrix((dist(count2d1_network[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=.03), col='magenta2', main="3D", xlab="Pairwise distance", lwd=.3)
  lines(density(as.vector((as.matrix(dist(count2d5_result_thin[[paste0("seed",(seed))]]$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/as.matrix((dist(count2d5_network[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=.03), col='orange', main="3D", xlab="Pairwise distance", lwd=.3)
}
legend("topright", legend = c("Slight", "Moderate", "High"),
       col=c(rgb(red=106/255, green = 13/255, blue = 173/255), "magenta3", "orange"),
       lty=1, lwd=3, cex=1.2)



# absolute difference curve -----------------------------------------------


abs_diff = function(est_network, obs_network) {
  return(sum(abs(as.numeric(est_network)-
                   as.numeric(obs_network))/
               (dim(obs_network)[1]*(dim(obs_network)[1]-1))))
}
# abs_diff(beta2orderlspmbi_c_thin$, beta2order$network)

n_pos=30
full_pred2d0 <- full_pred2d1 <- full_pred2d5 <- full_true4d <- c()
for(seed in seed_number) {
  node100results2d0_single_check = list(predcheck(n_pos, count2d0_result_thin[[paste0("seed",(seed))]]$alpha,
                                                  count2d0_result_thin[[paste0("seed",(seed))]]$positions,
                                                  type="count",
                                                  network = count2d0_result_thin[[paste0("seed",(seed))]]$initialisation$network, dist_power = 2, seed=1234))
  names(node100results2d0_single_check) <- paste0("seed",(seed))
  full_pred2d0 <-  append(full_pred2d0, node100results2d0_single_check)

  node100results2d1_single_check = list(predcheck(n_pos, count2d1_result_thin[[paste0("seed",(seed))]]$alpha,
                                                  count2d1_result_thin[[paste0("seed",(seed))]]$positions,
                                                  type="count",
                                                  network = count2d1_result_thin[[paste0("seed",(seed))]]$initialisation$network, dist_power = 2, seed=1234))
  names(node100results2d1_single_check) <- paste0("seed",(seed))
  full_pred2d1 <-  append(full_pred2d1, node100results2d1_single_check)

  node100results2d5_single_check = list(predcheck(n_pos, count2d5_result_thin[[paste0("seed",(seed))]]$alpha,
                                                  count2d5_result_thin[[paste0("seed",(seed))]]$positions,
                                                  type="count",
                                                  network = count2d5_result_thin[[paste0("seed",(seed))]]$initialisation$network, dist_power = 2, seed=1234))
  names(node100results2d5_single_check) <- paste0("seed",(seed))
  full_pred2d5 <-  append(full_pred2d5, node100results2d5_single_check)

  # true4d <- list(predcheck(n_pos, type="count", network = node100network4d_c[[paste0("seed",(seed))]]$network,networkMGP = node100network4d_c[[paste0("seed",(seed))]], dist_power=2, seed = 1234))
  # names(true4d) <- paste0("seed",(seed))
  # full_true4d <- append(full_true4d, true4d)

}

abs2d0 <- abs2d1 <- abs2d5 <- abstrue <- c()
for(seed in seed_number) {
  abs2d0 <- c(abs2d0, mean(apply(full_pred2d0[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, count2d0_network[[paste0("seed",(seed))]]$network))))
  abs2d1 <- c(abs2d1, mean(apply(full_pred2d1[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, count2d1_network[[paste0("seed",(seed))]]$network))))
  abs2d5 <- c(abs2d5, mean(apply(full_pred2d5[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, count2d5_network[[paste0("seed",(seed))]]$network))))
  # abstrue <- c(abstrue, mean(apply(full_true4d[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
}

bbb = cbind(abs2d0, abs2d1, abs2d5)
colnames(bbb) = c("Slight", "Moderate", "High")
vioplot(bbb, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, col="black")
mtext("Mean Absolute Difference", side=2, line=2.5, cex=1.35)

# mean(apply(count2d0_result_thin_check$Networks, 3, function(x) abs_diff(x, node100network4d_c$seed761680$network)))
#
# sapply(full_pred2d5, function(x) apply(x$Networks, 3, function(x) mean(abs_diff(x, x$initialisation$network))))
# apply(full_pred2d5$seed761680$Networks, 3, function(x) mean(abs_diff(x, x$initialisation$network)))

# legend("topright", legend = c("True", "LSPM3", "LSPM4", "LSPM8"), col="black", pch=20)



# Count difference plot ---------------------------------------------------

predBar2 <- function(posterior_results, obs_network, ...){
  # browser()
  posterior_counts = apply(posterior_results$Networks, 3, table)
  merged_counts <- merge(table(obs_network), posterior_counts[[1]], by=1, all=TRUE)
  for(i in 2:length(posterior_counts)) {
    suppressWarnings(merged_counts <- merge(merged_counts, posterior_counts[[i]], by=1,all=TRUE))
  }
  merged_counts= merged_counts[order(as.numeric(levels(merged_counts$obs_network))),]
  merged_counts = t(merged_counts)
  colnames(merged_counts) = merged_counts[1,]
  ifelse(dim(merged_counts)[2] > 20, merged_counts <- merged_counts[-1,1:10], merged_counts <- merged_counts[-1,])
  # merged_counts <- merged_counts[-1,1:10]
  merged_counts = log(apply(merged_counts, 2, as.numeric))
  vioplot(merged_counts[-1,], drawRect = FALSE, border = NA, ...,
          ylab = "", xlab = "",
          ylim=c(min(merged_counts, na.rm = TRUE),
                 max(merged_counts, na.rm = TRUE)+2))
  points(merged_counts[1,], col=rgb(red=1, green = 0, blue = 0, alpha=0.5))
  # legend("topright", legend = c("Posterior predictive networks", "Observed network"),
  #        col = c("grey", "red"), pch = c(20, 1))
}

# predBar(pred2d0, node100network4d_c$seed630678$network)
haiz = matrix(1, ncol=10)
colnames(haiz) = 0:9
vioplot(haiz,colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0),
        rectCol = NA, lineCol = NA,
        ylab = "log Frequency", xlab = "Counts",
        ylim=c(0,10))
for(seed in seed_number){
  predBar2(full_pred2d0[[paste0("seed",(seed))]], count2d0_network[[paste0("seed",(seed))]]$network, add=T, col=rgb(red=106/255, green = 13/255, blue = 173/255, alpha=0.1))
  predBar2(full_pred2d1[[paste0("seed",(seed))]], count2d1_network[[paste0("seed",(seed))]]$network, add=T, col=rgb(red=205/255, green = 11/255, blue = 188/255, alpha=0.1))
  predBar2(full_pred2d5[[paste0("seed",(seed))]], count2d5_network[[paste0("seed",(seed))]]$network, add=T, col=rgb(red=1, green = .647, blue = 0, alpha=0.1))

}
legend("topright", legend = c("Observed network", "Slight o.d. p.p.n", "Moderate o.d. p.p.n", "High o.d. p.p.n"), col=c("red", rgb(red=106/255, green = 13/255, blue = 173/255), "magenta3", "orange"), pch=c(1,20,20,20))
