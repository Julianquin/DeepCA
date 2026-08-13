load(file = "~/lspm/inst/extdata/adapt/count2d0_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d1_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d5_result_adapt.Rdata")

source("~/lspm/inst/disp_network_c.R")

# Pairwise estimated over true curve --------------------------------------
# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/disp_ratio.pdf", width = 5, height = 4)
par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))
d=2
plot(density(as.vector(as.matrix((dist(count2d0_network$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/(as.matrix(dist(count2d0_network$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=0.03),
     type="n",col='red', main="", ylab="",xlab="Ratio of pairwise distance", ylim=c(0,10), lwd=1, xlim=c(0,3), cex.sub=2, cex.axis=1.5, cex.lab=1.5)
mtext("Density", side=2, line=2.5, cex=1.5)
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
for(seed in seed_number) {
  if(d %in% names(table(count2d0_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    count2d0_lspm_dist = (as.matrix(dist(apply(count2d0_result_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    count2d0_true_dist = as.matrix((dist(count2d0_network[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(count2d0_lspm_dist/count2d0_true_dist), na.rm = T, bw=.03), col=3, main="3D", xlab="Pairwise distance", lwd=.3)

  }

  if(d %in% names(table(count2d1_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    count2d1_lspm_dist = (as.matrix(dist(apply(count2d1_result_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    count2d1_true_dist = as.matrix((dist(count2d1_network[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(count2d1_lspm_dist/count2d1_true_dist), na.rm = T, bw=.03), col=4, main="3D", xlab="Pairwise distance", lwd=.3)

  }

  if(d %in% names(table(count2d5_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    count2d5_lspm_dist = (as.matrix(dist(apply(count2d5_result_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    count2d5_true_dist = as.matrix((dist(count2d5_network[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(count2d5_lspm_dist/count2d5_true_dist), na.rm = T, bw=.03), col=5, main="3D", xlab="Pairwise distance", lwd=.3)

  }

}


legend("topright", legend = c("Slight", "Moderate", "High"),
       col=3:5,
       lty=1, lwd=3, cex=1.2)


dev.off()
# absolute difference curve -----------------------------------------------


abs_diff = function(est_network, obs_network) {
  return(sum(abs(as.numeric(est_network)-
                   as.numeric(obs_network))/
               (dim(obs_network)[1]*(dim(obs_network)[1]-1))))
}
# abs_diff(beta2orderlspmbi_c_thin$, beta2order$network)

n_pos=30
full_pred2d0 <- full_pred2d1 <- full_pred2d5 <- c()
for(seed in seed_number) {
  if(d %in% names(table(count2d0_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    node100results2d0_single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = count2d0_result_adapt[[paste0("seed",(seed))]],
                                                    type="count", dist_power = 2, seed=1234))
    names(node100results2d0_single_check) <- paste0("seed",(seed))
    full_pred2d0 <-  append(full_pred2d0, node100results2d0_single_check)
  }

  if(d %in% names(table(count2d1_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    node100results2d1_single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = count2d1_result_adapt[[paste0("seed",(seed))]],
                                                    type="count", dist_power = 2, seed=1234))
    names(node100results2d1_single_check) <- paste0("seed",(seed))
    full_pred2d1 <-  append(full_pred2d1, node100results2d1_single_check)
  }

  if(d %in% names(table(count2d5_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    node100results2d5_single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = count2d5_result_adapt[[paste0("seed",(seed))]],
                                                    type="count", dist_power = 2, seed=1234))
    names(node100results2d5_single_check) <- paste0("seed",(seed))
    full_pred2d5 <-  append(full_pred2d5, node100results2d5_single_check)
  }

}

abs2d0 <- abs2d1 <- abs2d5 <- c()
for(seed in seed_number) {
  if(d %in% names(table(count2d0_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    abs2d0 <- c(abs2d0, mean(apply(full_pred2d0[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, count2d0_network[[paste0("seed",(seed))]]$network))))
  }

  if(d %in% names(table(count2d1_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    abs2d1 <- c(abs2d1, mean(apply(full_pred2d1[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, count2d1_network[[paste0("seed",(seed))]]$network))))
  }

  if(d %in% names(table(count2d5_result_adapt[[paste0("seed",(seed))]]$iter_d))) {
    abs2d5 <- c(abs2d5, mean(apply(full_pred2d5[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, count2d5_network[[paste0("seed",(seed))]]$network))))
  }

}

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/disp_abs.pdf", width = 5, height = 4)
par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))
# bbb = cbind(abs2d0, abs2d1, abs2d5)
bbb = cbind(-10,-10,-10)
colnames(bbb) = c("Slight", "Moderate", "High")
vioplot(bbb, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, col="white", ylim=c(0,2.5))
vioplot(abs2d0, add=T, at = 1, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, ylim=c(0,1))
vioplot(abs2d1, add=T, at = 2, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, ylim=c(0,1))
vioplot(abs2d5, add=T, at = 3, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, ylim=c(0,1))

mtext("Mean absolute difference", side=2, line=2.5, cex=1.35)
mtext("Overdispersion", side=1, line=2.5, cex=1.35)

dev.off()

# log count ---------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/disp_logcount.pdf", width = 10, height = 4)


par(mfrow=c(1,3))
layout.matrix <- matrix(1:3, nrow = 1, ncol = 3)
layout(mat = layout.matrix, widths = c(2.4,2,2.2))
par(mar = c(5.6, 4.1, 4.1, 0))

vioplot(do.call(rbind,lapply(full_pred2d0, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:6], drawRect = FALSE, border = NA,
        colMed = NA, rectCol = NA, lineCol = NA, yaxt="n",
        ylim=c(0,10), main="Low overdispersion")
vioplot(do.call(rbind,lapply(count2d0_network, function(x) log(tabulate(x$network+1, 10))))[,1:6], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:6, las=1, labels=0:5, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("log frequency")), line=2.2, cex.lab=1.5)
axis(2, at=0:10, las=1, labels=0:10, cex.axis=1.5)
legend("topright", legend = c("Posterior predictive network", "Observed network"), col=c("gray","red"), pch=c(20,20))


par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0

vioplot(do.call(rbind,lapply(full_pred2d1, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:6], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n", main="Moderate overdispersion")
vioplot(do.call(rbind,lapply(count2d1_network, function(x) log(tabulate(x$network+1, 10))))[,1:6], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:6, las=1, labels=0:5, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)

par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

vioplot(do.call(rbind,lapply(full_pred2d5, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:6], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n", main="High overdispersion")
vioplot(do.call(rbind,lapply(count2d5_network, function(x) log(tabulate(x$network+1, 10))))[,1:6], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:6, las=1, labels=0:5, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)


dev.off()
