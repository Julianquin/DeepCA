load(file = "~/lspm/inst/extdata/binary/node20results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node100results_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node200results_thin.Rdata")

source("~/lspm/inst/section4_1network.R")

# open the pdf file
cairo_pdf("~/lspm/inst/figures/pmv4_1.pdf", width = 10, height = 4)


# Setup the plotting area
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,4))
layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)
layout(mat = layout.matrix, widths = c(2.4,2,2,2, 2.2))
par(mar = c(5.6, 4.1, 4.1, 0))


# 20 nodes ----------------------------------------------------------------

# Plot LSPM 5D results for variances, 20 nodes.
vioplot(cbind(1/node20results_thin[[paste0("seed",(seed_number[1]))]]$omegas),
        ylim=c(0,8), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "n = 20", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), cex.lab=1.75, line=2)
for(seed in seed_number) {
  vioplot(1/node20results_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node20results_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=2, col="green")

legend("topright", legend=c('Posterior distribution', "True value", 'Average posterior mean'), pch=c(20,4,20), col=c("black","red", "green"), cex=1.2)


# 50 nodes ----------------------------------------------------------------


par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0


vioplot(1/node50results_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,8), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "n = 50", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
axis(1, at=1:5, las=1, labels=1:5, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node50results_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node50results_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=2, col="green")



# 100 nodes ---------------------------------------------------------------


vioplot(1/node100results_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,8), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "n = 100", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node100results_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node100results_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=2, col="green")


# 200 nodes ---------------------------------------------------------------
par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

vioplot(1/node200results_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,8), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= "n = 200", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node200results_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node200results_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=2, col="green")

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()
