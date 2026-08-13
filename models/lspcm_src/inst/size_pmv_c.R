load(file = "~/lspm/inst/extdata/adapt/node20results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node200results_adapt_c.Rdata")

source("~/lspm/inst/size_network_c.R")

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/size_pmv_c.pdf", width = 10, height = 4)

# Setup the plotting area
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,4))
layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)
layout(mat = layout.matrix, widths = c(2.6,2,2,2.2))
par(mar = c(5.6, 4.5, 4.1, 0))


# 20 nodes ----------------------------------------------------------------


# Plot LSPM 5D results for Variances, 20 nodes.
vioplot(cbind(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances[complete.cases(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances),,drop=F]),
        ylim=c(0,12), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "n = 20", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(node20results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                      colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
title(xlab=expression(paste("Dimension \u2113")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.2, cex.lab=1.75)
# for(seed in seed_number[2:15]) {
#   vioplot(node100results4d3_adapt2[[paste0("seed",(seed))]]$mcmc_chain$"4D"$variances, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
#           colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03))
# }
points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node20results_thin,function(x) apply(x$variances,2,mean)))),2,mean),pch=20, cex=2, col="green")

legend("topright", legend=c('Posterior distribution', "True value"), pch=c(20,4), col=c("black","red"), cex=1.2)


# 50 nodes ----------------------------------------------------------------

par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0

vioplot(cbind(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances[complete.cases(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances),,drop=F]),
        ylim=c(0,12), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "n = 50", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
for(seed in seed_number) {
  lapply(node50results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                      colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
axis(1, at=1:4, las=1, labels=1:4, cex.axis=1.5)

title(xlab=expression(paste("Dimension \u2113")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.2)
points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node50results_thin,function(x) apply(x$variances,2,mean)))),2,mean),pch=20, cex=2, col="green")


# 100 nodes ---------------------------------------------------------------


vioplot(cbind(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances[complete.cases(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances),,drop=F]),
        ylim=c(0,12), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "n = 100", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
for(seed in seed_number) {
  lapply(node100results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                       colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
title(xlab=expression(paste("Dimension \u2113")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.2)
axis(1, at=1:4, las=1, labels=1:4, cex.axis=1.5)

points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node100results_thin,function(x) apply(x$variances,2,mean)))),2,mean),pch=20, cex=2, col="green")


# 200 nodes ---------------------------------------------------------------
par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

vioplot(cbind(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances[complete.cases(node200results_adapt_c[[paste0("seed",(seed_number[1]))]]$mcmc_chain$"4D"$variances),,drop=F]),
        ylim=c(0,12), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "n = 200", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
for(seed in seed_number) {
  lapply(node200results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$variances[complete.cases(x$variances),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                       colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}

title(xlab=expression(paste("Dimension \u2113")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance ", omega["\u2113"]^{-1})), line=2.2)
axis(1, at=1:4, las=1, labels=1:4, cex.axis=1.5)
points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node200results_thin,function(x) apply(x$variances,2,mean)))),2,mean),pch=20, cex=2, col="green")


par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

