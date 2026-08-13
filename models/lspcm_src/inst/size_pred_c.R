load(file = "~/lspm/inst/extdata/adapt/node20results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node200results_adapt_c.Rdata")

source("~/lspm/inst/size_network_c.R")


# Pairwise estimated over true curve --------------------------------------
# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/size_ratio.pdf", width = 5, height = 4)
par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))
d=2
plot(density(as.vector(as.matrix((dist(node20network_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)/(as.matrix(dist(node20network_c$seed761680$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)), na.rm = T, bw=0.03),
     type="n",col='red', main="", ylab="",xlab="Ratio of pairwise distance", ylim=c(0,10), lwd=1, xlim=c(0,2), cex.sub=2, cex.axis=1.5, cex.lab=1.5)
mtext("Density", side=2, line=2.5, cex=1.5)
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
for(seed in seed_number) {
  if(d %in% names(table(node20results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    node20_lspm_dist = (as.matrix(dist(apply(node20results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    node20_true_dist = as.matrix((dist(node20network_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node20_lspm_dist/node20_true_dist), na.rm = T, bw=.03), col=3, main="3D", xlab="Pairwise distance", lwd=.3)

  }

  if(d %in% names(table(node50results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    node50_lspm_dist = (as.matrix(dist(apply(node50results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    node50_true_dist = as.matrix((dist(node50network_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node50_lspm_dist/node50_true_dist), na.rm = T, bw=.03), col=4, main="3D", xlab="Pairwise distance", lwd=.3)

  }

  if(d %in% names(table(node100results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    node100_lspm_dist = (as.matrix(dist(apply(node100results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    node100_true_dist = as.matrix((dist(node100network_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node100_lspm_dist/node100_true_dist), na.rm = T, bw=.03), col=5, main="3D", xlab="Pairwise distance", lwd=.3)

  }

  if(seed %in% seed_number){
  if(d %in% names(table(node200results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    node200_lspm_dist = (as.matrix(dist(apply(node200results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions,c(1,2),mean, na.rm=TRUE), diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    node200_true_dist = as.matrix((dist(node200network_c[[paste0("seed",(seed))]]$positions, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
    lines(density(as.vector(node200_lspm_dist/node200_true_dist), na.rm = T, bw=.03), col=6, main="3D", xlab="Pairwise distance", lwd=.3)

  }
  }

}


legend("topright", legend = c("20","50","100","200"), title="n",
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
full_pred20_c <- full_pred50_c <- full_pred100_c <- full_pred200_c <- c()
for(seed in seed_number) {
  if(d %in% names(table(node20results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node20results_adapt_c[[paste0("seed",(seed))]],
                                                    type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred20_c <-  append(full_pred20_c, single_check)
  }

  if(d %in% names(table(node50results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node50results_adapt_c[[paste0("seed",(seed))]],
                                                    type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred50_c <-  append(full_pred50_c, single_check)
  }

  if(d %in% names(table(node100results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node100results_adapt_c[[paste0("seed",(seed))]],
                                                   type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred100_c <-  append(full_pred100_c, single_check)
  }

  if(seed %in% seed_number){
  if(d %in% names(table(node200results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    single_check = list(predcheck(n_pos, n_dimen = d, LSPM_object = node200results_adapt_c[[paste0("seed",(seed))]],
                                                    type="count", dist_power = 2, seed=1234))
    names(single_check) <- paste0("seed",(seed))
    full_pred200_c <-  append(full_pred200_c, single_check)
  }
}
}

abs20_c <- abs50_c <- abs100_c <- abs200_c <- c()
for(seed in seed_number) {
  if(d %in% names(table(node20results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    abs20_c <- c(abs20_c, mean(apply(full_pred20_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node20network_c[[paste0("seed",(seed))]]$network))))
  }

  if(d %in% names(table(node50results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    abs50_c <- c(abs50_c, mean(apply(full_pred50_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node50network_c[[paste0("seed",(seed))]]$network))))
  }

  if(d %in% names(table(node100results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    abs100_c <- c(abs100_c, mean(apply(full_pred100_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node100network_c[[paste0("seed",(seed))]]$network))))
  }

  if(seed %in% seed_number){
  if(d %in% names(table(node200results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
    abs200_c <- c(abs200_c, mean(apply(full_pred200_c[[paste0("seed",(seed))]]$Networks, 3, function(x) abs_diff(x, node200network_c[[paste0("seed",(seed))]]$network))))
  }
  }
}


# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/size_abs.pdf", width = 5, height = 4)
par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))

# bbb = cbind(abs20_c, abs50_c, abs100_c)
bbb = cbind(-10,-10,-10,-10)
colnames(bbb) = c("20","50","100","200")
vioplot(bbb, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA, col="white", ylim=c(0,1.5))
vioplot(abs20_c, add=T, at = 1, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA)
vioplot(abs50_c, add=T, at = 2, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA)
vioplot(abs100_c, add=T, at = 3, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA)
vioplot(abs200_c, add=T, at = 4, cex=0.2, rectCol = NA, lineCol = NA, cex.sub=2, cex.axis=1.5,
        colMed = NA, border=NA)

mtext("Mean absolute difference", side=2, line=2.5, cex=1.35)
mtext("n", side=1, line=2.5, cex=1.35)

dev.off()

# log count ---------------------------------------------------------------

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/size_logcount.pdf", width = 10, height = 4)

par(mfrow=c(1,4))
layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)
layout(mat = layout.matrix, widths = c(2.4,2,2,2.2))
par(mar = c(5.6, 4.1, 4.1, 0))

vioplot(do.call(rbind,lapply(full_pred20_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:5], drawRect = FALSE, border = NA,
        colMed = NA, rectCol = NA, lineCol = NA, yaxt="n",
        ylim=c(0,10), main="n = 20")
vioplot(do.call(rbind,lapply(node20network_c, function(x) log(tabulate(x$network+1, 10))))[,1:5], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:5, las=1, labels=0:4, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("log frequency")), line=2.2, cex.lab=1.5)
axis(2, at=0:10, las=1, labels=0:10, cex.axis=1.5)
legend("topright", legend = c("Posterior predictive network", "Observed network"), col=c("gray","red"), pch=c(20,20))


par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0

vioplot(do.call(rbind,lapply(full_pred50_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:5], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n", main="n = 50")
vioplot(do.call(rbind,lapply(node50network_c, function(x) log(tabulate(x$network+1, 10))))[,1:5], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:5, las=1, labels=0:4, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)

vioplot(do.call(rbind,lapply(full_pred100_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:5], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n", main="n = 100")
vioplot(do.call(rbind,lapply(node100network_c, function(x) log(tabulate(x$network+1, 10))))[,1:5], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:5, las=1, labels=0:4, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)

par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

vioplot(do.call(rbind,lapply(full_pred200_c, function(x) t(log(apply(x$Networks+1, 3, tabulate, 10)))))[,1:5], drawRect = FALSE, border = NA, ylim=c(0,10),
        yaxt="n", main="n = 200")
vioplot(do.call(rbind,lapply(node200network_c, function(x) log(tabulate(x$network+1, 10))))[,1:5], drawRect = FALSE, border = NA,
        add=T, col=rgb(red=255/255, green=0, blue=0, alpha=0.5))
axis(1, at=1:5, las=1, labels=0:4, cex.axis=1.5)
title(xlab=expression(paste("Count")), cex.lab=1.5, line=2.5)
dev.off()


