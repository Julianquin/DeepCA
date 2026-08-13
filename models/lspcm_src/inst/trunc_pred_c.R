load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_low_d_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_same_d_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_high_d_c.Rdata")

source("~/lspm/inst/trunc_network_c.R")


# Pairwise estimated over true curve --------------------------------------
# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/trunc_ratio.pdf", width = 5, height = 3)

par(mfrow=c(1,1))
par(mar = c(4.1, 4.5, 0.5, 0.5))
d=4
plot(density(as.vector(as.matrix((dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/(as.matrix(dist(node100network4d_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=0.03),
     type="n",col='red', main="", ylab="",xlab="Ratio of pairwise distance", ylim=c(0,10), lwd=1, xlim=c(0,2), cex.sub=2, cex.axis=1.5, cex.lab=1.5)
mtext("Density", side=2, line=2.5, cex=1.5)
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
for(seed in seed_number) {
  node4d_true_dist = as.matrix((dist(node100network4d_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)

  if(d %in% names(table(node100results4d3_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    node4d_adapt_lspm_dist = (as.matrix(dist(apply(node100results4d3_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node4d_adapt_lspm_dist/node4d_true_dist), na.rm = T, bw=.03), col=3, main="3D", xlab="Pairwise distance", lwd=.3)
  }

  if(d %in% names(table(node100results4d3_adapt_low_d_c[[paste0("seed",(seed))]]$iter_d))) {
    node4d_low_d_lspm_dist = (as.matrix(dist(apply(node100results4d3_adapt_low_d_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node4d_low_d_lspm_dist/node4d_true_dist), na.rm = T, bw=.03), col=4, main="3D", xlab="Pairwise distance", lwd=.3)
  }

  if(d %in% names(table(node100results4d3_adapt_same_d_c[[paste0("seed",(seed))]]$iter_d))) {
    node4d_same_d_lspm_dist = (as.matrix(dist(apply(node100results4d3_adapt_same_d_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node4d_same_d_lspm_dist/node4d_true_dist), na.rm = T, bw=.03), col=5, main="3D", xlab="Pairwise distance", lwd=.3)
  }

  if(d %in% names(table(node100results4d3_adapt_high_d_c[[paste0("seed",(seed))]]$iter_d))) {
    node4d_high_d_lspm_dist = (as.matrix(dist(apply(node100results4d3_adapt_high_d_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node4d_high_d_lspm_dist/node4d_true_dist), na.rm = T, bw=.03), col=6, main="3D", xlab="Pairwise distance", lwd=.3)
  }

}

legend("topright", legend = c("auto","2","4","10"), title=expression(p[0]),
       col=3:6,
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
full_pred_adapt_c <- full_pred_adapt_low_d_c <- full_pred_adapt_same_d_c <- full_pred_adapt_high_d_c <- c()
for(seed in seed_number) {
  if(d %in% names(table(node100results4d3_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt_c[[paste0("seed",(seed))]],
                                  type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred_adapt_c <-  append(full_pred_adapt_c, single_check)
  }

  if(d %in% names(table(node100results4d3_adapt_low_d_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt_low_d_c[[paste0("seed",(seed))]],
                                  type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred_adapt_low_d_c <-  append(full_pred_adapt_low_d_c, single_check)
  }

  if(d %in% names(table(node100results4d3_adapt_same_d_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt_same_d_c[[paste0("seed",(seed))]],
                                  type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred_adapt_same_d_c <-  append(full_pred_adapt_same_d_c, single_check)
  }


  if(d %in% names(table(node100results4d3_adapt_high_d_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt_high_d_c[[paste0("seed",(seed))]],
                                  type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred_adapt_high_d_c <-  append(full_pred_adapt_high_d_c, single_check)
  }

}

abs_adapt_c <- abs_adapt_low_d_c <- abs_adapt_same_d_c <- abs_adapt_high_d_c <- c()
for(seed in seed_number) {
  if(d %in% names(table(node100results4d3_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    abs_adapt_c <- c(abs_adapt_c, mean(apply(full_pred_adapt_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
  }

  if(d %in% names(table(node100results4d3_adapt_low_d_c[[paste0("seed",(seed))]]$iter_d))) {
    abs_adapt_low_d_c <- c(abs_adapt_low_d_c, mean(apply(full_pred_adapt_low_d_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
  }

  if(d %in% names(table(node100results4d3_adapt_same_d_c[[paste0("seed",(seed))]]$iter_d))) {
    abs_adapt_same_d_c <- c(abs_adapt_same_d_c, mean(apply(full_pred_adapt_same_d_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
  }

  if(d %in% names(table(node100results4d3_adapt_high_d_c[[paste0("seed",(seed))]]$iter_d))) {
    abs_adapt_high_d_c <- c(abs_adapt_high_d_c, mean(apply(full_pred_adapt_high_d_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network4d_c[[paste0("seed",(seed))]]$network))))
  }
}

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/trunc_abs.pdf", width = 5, height = 3)
par(mfrow=c(1,1))
par(mar = c(3.6, 4.5, 0.5, 1.0))

# bbb = cbind(abs_adapt_c, abs_adapt_low_d_c, abs_adapt_same_d_c)
bbb = cbind(-10,-10,-10,-10)
colnames(bbb) = c("auto","2","4","10")
vioplot(bbb, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.2,
        colMed = NA, border=NA, col="white", ylim=c(0,8))
vioplot(abs_adapt_c, add=T, at = 1, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.2,
        colMed = NA, border=NA)
vioplot(abs_adapt_low_d_c, add=T, at = 2, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.2,
        colMed = NA, border=NA)
vioplot(abs_adapt_same_d_c, add=T, at = 3, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.2,
        colMed = NA, border=NA)
vioplot(abs_adapt_high_d_c, add=T, at = 4, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.2,
        colMed = NA, border=NA)

mtext("Mean absolute difference", side=2, line=2.5, cex=1.2)
mtext(expression(p[0]), side=1, line=2.5, cex=1.2)

dev.off()
# log count ---------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/trunc_logcount.pdf", width = 10, height = 3)

par(mfrow=c(1,4))
layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)
layout(mat = layout.matrix, widths = c(2.4,2,2,2.2))
par(mar = c(4.1, 4.1, 4.1, 0))

vioplot(do.call(rbind,lapply(full_pred_adapt_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:6], drawRect = FALSE, border = NA,
        colMed = NA, rectCol = NA, lineCol = NA, yaxt="n",
        ylim=c(0,10))
title(main=expression(paste(p[0]," automatically chosen")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = auto)")), line=1, cex.main=1.5)
vioplot(do.call(rbind,lapply(node100network4d_c, function(x) log(tabulate(x$network+1, 10))))[,1:6], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:6, las=1, labels=0:5, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("log frequency")), line=2.2, cex.lab=1.5)
axis(2, at=0:10, las=1, labels=0:10, cex.axis=1.5)
legend("topright", legend = c("Posterior predictive network", "Observed network"), col=c("gray","red"), pch=c(20,20), cex=1.15)


par(mar = c(4.1, 0, 4.1, 0)) # left margin set as 0

vioplot(do.call(rbind,lapply(full_pred_adapt_low_d_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:6], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n")
title(main=expression(paste(p[0]," lower than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)
vioplot(do.call(rbind,lapply(node100network4d_c, function(x) log(tabulate(x$network+1, 10))))[,1:6], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:6, las=1, labels=0:5, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)

vioplot(do.call(rbind,lapply(full_pred_adapt_same_d_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:6], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n")
title(main=expression(paste(p[0]," same as the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)
vioplot(do.call(rbind,lapply(node100network4d_c, function(x) log(tabulate(x$network+1, 10))))[,1:6], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:6, las=1, labels=0:5, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)

par(mar = c(4.1, 0, 4.1, 0.25)) # right margin set as 0.1

vioplot(do.call(rbind,lapply(full_pred_adapt_high_d_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:6], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n")
title(main=expression(paste(p[0]," higher than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)
vioplot(do.call(rbind,lapply(node100network4d_c, function(x) log(tabulate(x$network+1, 10))))[,1:6], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:6, las=1, labels=0:5, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)

dev.off()

