load(file = "~/lspm/inst/extdata/adapt/count2d0_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d1_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d5_result_adapt.Rdata")

source("~/lspm/inst/disp_network_c.R")

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/disp_pmd.pdf", width = 10, height = 4)

# Setup the plotting area
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,3))
layout.matrix <- matrix(1:3, nrow = 1, ncol = 3)
layout(mat = layout.matrix, widths = c(2.4,2, 2.2))
par(mar = c(5.6, 4.1, 4.1, 0))


# Low overdispersion ----------------------------------------------------------------

vioplot(cbind(count2d5_result_adapt[[paste0("seed",(seed_number[2]))]]$mcmc_chain$"4D"$deltas[complete.cases(count2d5_result_adapt[[paste0("seed",(seed_number[2]))]]$mcmc_chain$"4D"$deltas),,drop=F]),
        ylim=c(0,60), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.0),
        main= "low overdispersion", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
for(seed in seed_number) {
  lapply(count2d0_result_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                      colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
axis(1, at=1:4, las=1, labels=1:4, cex.axis=1.5)

title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2, cex.lab=1.5)
points(c(1.5,1.5), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node50results_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")


# Moderate overdispersion ---------------------------------------------------------------
par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0

vioplot(cbind(count2d5_result_adapt[[paste0("seed",(seed_number[2]))]]$mcmc_chain$"4D"$deltas[complete.cases(count2d5_result_adapt[[paste0("seed",(seed_number[2]))]]$mcmc_chain$"4D"$deltas),,drop=F]),
        ylim=c(0,60), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "moderate overdispersion", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
for(seed in seed_number) {
  lapply(count2d1_result_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                       colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}
title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2)
axis(1, at=1:4, las=1, labels=1:4, cex.axis=1.5)

points(c(0.5,1.5), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node100results_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")


# High overdispersion ---------------------------------------------------------------
par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

vioplot(cbind(count2d5_result_adapt[[paste0("seed",(seed_number[2]))]]$mcmc_chain$"4D"$deltas[complete.cases(count2d5_result_adapt[[paste0("seed",(seed_number[2]))]]$mcmc_chain$"4D"$deltas),,drop=F]),
        ylim=c(0,60), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0),
        main= "high overdispersion", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
for(seed in seed_number) {
  lapply(count2d5_result_adapt[[paste0("seed",(seed))]]$mcmc_chain, function(x) vioplot(x$deltas[complete.cases(x$deltas),,drop=F], add=T, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
                                                                                       colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.03)))
}

title(xlab=expression(paste("Dimension h")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Shrinkage strength ", delta[h])), line=2.2)
axis(1, at=1:4, las=1, labels=1:4, cex.axis=1.5)
points(c(0.1,1.5), col='red', pch=4, cex=2, lwd=3) # true
# points(apply(t(as.data.frame(lapply(node200results_thin,function(x) apply(x$deltas,2,mean)))),2,mean),pch=20, cex=2, col="green")


par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

