load(file = "~/lspm/inst/extdata/count/node100results4d3_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node100results4d4_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node100results4d8_c_thin.Rdata")

source("~/lspm/inst/section4_2network_c.R")

# open the pdf file
cairo_pdf("~/lspm/inst/figures/devR4_2c.pdf", width = 4.4, height = 4)

par(mar = c(4.1, 4.1, 1.1, 1.1))
# R2 plot -----------------------------------------------------------------

library(vioplot)
# sapply(node100results4d3_c_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network))
# sapply(node100results4d4_c_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network))
# sapply(node100results4d8_c_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network))
collection_of_R2 = cbind(sapply(node100results4d3_c_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network)),
                         sapply(node100results4d4_c_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network)),
                         sapply(node100results4d8_c_thin, function(x) devR2(x$alpha_mean, x$z_mean, x$initialisation$network)))
colnames(collection_of_R2) = c("LSPM3", "LSPM4", "LSPM8")
vioplot(collection_of_R2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5, ylim=c(0.85,1),
        colMed = NA, border=NA, col="black")
# legend("bottomright", legend = c("LSPM3", "LSPM4", "LSPM8"), col=c(rgb(red=106/255, green = 13/255, blue = 173/255), "magenta3", "orange"), pch=20)
mtext(expression(paste("Deviance ", R^2)), side=2, line=2.5, cex=1.5)

# Close the pdf file
dev.off()

# Individual curve (not used) ---------------------------------------------
#
# plot(density(as.matrix(dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col='red', main="3D", xlab="Pairwise distance", ylim=c(0,0.15), lwd=3)
# lines(density(as.matrix(dist(node100results4d3_c_thin$seed761680$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col=rgb(red=106/255, green = 13/255, blue = 173/255), lty=2, lwd=3)
# lines(density(as.matrix(dist(node100results4d4_c_thin$seed761680$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col='magenta3', lty=2, lwd=3)
# lines(density(as.matrix(dist(node100results4d8_c_thin$seed761680$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2), col='orange', lty=2, lwd=3)



# Pairwise estimated over true curve --------------------------------------
# open the pdf file
cairo_pdf("~/lspm/inst/figures/pairwise4_2c.pdf", width = 4.4, height = 4)

par(mar = c(4.1, 4.1, 1.1, 1.1))

plot(density(as.vector(as.matrix((dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/(as.matrix(dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=0.03),
     type="n",col='red', main="", ylab="",xlab="Ratio of pairwise distance", ylim=c(0,10), lwd=1, xlim=c(0,2), cex.sub=2, cex.axis=1.5, cex.lab=1.5)
mtext("Density", side=2, line=2.5, cex=1.5)
for(seed in seed_number) {
  lines(density(as.vector((as.matrix(dist(node100results4d3_c_thin[[paste0("seed",(seed))]]$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/as.matrix((dist(node100network4d_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=.03), col=rgb(red=106/255, green = 13/255, blue = 173/255), main="3D", xlab="Pairwise distance", lwd=.3)
  lines(density(as.vector((as.matrix(dist(node100results4d4_c_thin[[paste0("seed",(seed))]]$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/as.matrix((dist(node100network4d_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=.03), col='magenta2', main="3D", xlab="Pairwise distance", lwd=.3)
  lines(density(as.vector((as.matrix(dist(node100results4d8_c_thin[[paste0("seed",(seed))]]$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/as.matrix((dist(node100network4d_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=.03), col='orange', main="3D", xlab="Pairwise distance", lwd=.3)
}
legend("topright", legend = c("LSPM3", "LSPM4", "LSPM8"),
       col=c(rgb(red=106/255, green = 13/255, blue = 173/255), "magenta3", "orange"),
       lty=1, lwd=3, cex=1.2)

# Close the pdf file
dev.off()


# absolute difference curve -----------------------------------------------


abs_diff = function(est_network, obs_network) {
  return(sum(abs(as.numeric(est_network)-
                   as.numeric(obs_network))/
               (dim(obs_network)[1]*(dim(obs_network)[1]-1))))
}
# abs_diff(beta2orderlspmbi_c_thin$, beta2order$network)

n_pos=30
full_pred4d3 <- full_pred4d4 <- full_pred4d8 <- full_true4d <- c()
for(seed in seed_number) {
  node100results4d3_single_check = list(predcheck(n_pos, node100results4d3_c_thin[[paste0("seed",(seed))]]$alpha,
                                                  node100results4d3_c_thin[[paste0("seed",(seed))]]$positions,
                                                  type="count",
                                                  network = node100results4d3_c_thin[[paste0("seed",(seed))]]$initialisation$network, dist_power = 2, seed=1234))
  names(node100results4d3_single_check) <- paste0("seed",(seed))
  full_pred4d3 <-  append(full_pred4d3, node100results4d3_single_check)

  node100results4d4_single_check = list(predcheck(n_pos, node100results4d4_c_thin[[paste0("seed",(seed))]]$alpha,
                                                  node100results4d4_c_thin[[paste0("seed",(seed))]]$positions,
                                                  type="count",
                                                  network = node100results4d4_c_thin[[paste0("seed",(seed))]]$initialisation$network, dist_power = 2, seed=1234))
  names(node100results4d4_single_check) <- paste0("seed",(seed))
  full_pred4d4 <-  append(full_pred4d4, node100results4d4_single_check)

  node100results4d8_single_check = list(predcheck(n_pos, node100results4d8_c_thin[[paste0("seed",(seed))]]$alpha,
                                                  node100results4d8_c_thin[[paste0("seed",(seed))]]$positions,
                                                  type="count",
                                                  network = node100results4d8_c_thin[[paste0("seed",(seed))]]$initialisation$network, dist_power = 2, seed=1234))
  names(node100results4d8_single_check) <- paste0("seed",(seed))
  full_pred4d8 <-  append(full_pred4d8, node100results4d8_single_check)

  # true4d <- list(predcheck(n_pos, type="count", network = node100network4d_c[[paste0("seed",(seed))]]$network,networkMGP = node100network4d_c[[paste0("seed",(seed))]], dist_power=2, seed = 1234))
  # names(true4d) <- paste0("seed",(seed))
  # full_true4d <- append(full_true4d, true4d)

}

abs4d3 <- abs4d4 <- abs4d8 <- abstrue <- c()
for(seed in seed_number) {
  abs4d3 <- c(abs4d3, mean(apply(full_pred4d3[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
  abs4d4 <- c(abs4d4, mean(apply(full_pred4d4[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
  abs4d8 <- c(abs4d8, mean(apply(full_pred4d8[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
  # abstrue <- c(abstrue, mean(apply(full_true4d[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
}

# open the pdf file
cairo_pdf("~/lspm/inst/figures/abs4_2c.pdf", width = 4.4, height = 4)

par(mar = c(4.1, 4.1, 1.1, 1.1))

bbb = cbind(abs4d3, abs4d4, abs4d8)
colnames(bbb) = c("LSPM3", "LSPM4", "LSPM8")
vioplot(bbb, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, col="black")
mtext("Mean absolute difference", side=2, line=2.5, cex=1.35)

# mean(apply(node100results4d3_c_thin_check$Networks, 3, function(x) abs_diff(x, node100network4d_c$seed761680$network)))
#
# sapply(full_pred4d8, function(x) apply(x$Networks, 3, function(x) mean(abs_diff(x, x$initialisation$network))))
# apply(full_pred4d8$seed761680$Networks, 3, function(x) mean(abs_diff(x, x$initialisation$network)))

# legend("topright", legend = c("True", "LSPM3", "LSPM4", "LSPM8"), col="black", pch=20)

# Close the pdf file
dev.off()

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

# open the pdf file
cairo_pdf("~/lspm/inst/figures/counts4_2c.pdf", width = 5, height = 4)
par(mar = c(4.1, 4.1, 1.1, 1.1))

# predBar(pred4d3, node100network4d_c$seed630678$network)
haiz = matrix(1, ncol=10)
colnames(haiz) = 0:9
vioplot(haiz,colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0),
        rectCol = NA, lineCol = NA,
        ylab = "Log frequency", xlab = "Observed counts",
        ylim=c(0,10))
for(seed in seed_number){
  predBar2(full_pred4d3[[paste0("seed",(seed))]], node100network4d_c[[paste0("seed",(seed))]]$network, add=T, col=rgb(red=106/255, green = 13/255, blue = 173/255, alpha=0.1))
  predBar2(full_pred4d4[[paste0("seed",(seed))]], node100network4d_c[[paste0("seed",(seed))]]$network, add=T, col=rgb(red=205/255, green = 11/255, blue = 188/255, alpha=0.1))
  predBar2(full_pred4d8[[paste0("seed",(seed))]], node100network4d_c[[paste0("seed",(seed))]]$network, add=T, col=rgb(red=1, green = .647, blue = 0, alpha=0.1))

}
legend("topright", legend = c("Observed network", "LSPM3 p.p.n", "LSPM4 p.p.n", "LSPM8 p.p.n"), col=c("red", rgb(red=106/255, green = 13/255, blue = 173/255), "magenta3", "orange"), pch=c(1,20,20,20))

par(mar = c(5.1, 4.1, 4.1, 2.1))
# Close the pdf file
dev.off()

