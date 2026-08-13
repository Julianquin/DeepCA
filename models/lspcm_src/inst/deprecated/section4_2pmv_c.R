load(file = "~/lspm/inst/extdata/count/node100results4d3_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node100results4d4_c_thin.Rdata")
load(file = "~/lspm/inst/extdata/count/node100results4d8_c_thin.Rdata")

source("~/lspm/inst/section4_2network_c.R")

# open the pdf file
cairo_pdf("~/lspm/inst/figures/pmv4_2c.pdf", width = 10, height = 4)

# Setup the plotting area
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,3))
layout.matrix <- matrix(1:3, nrow = 1, ncol = 3)
layout(mat = layout.matrix, widths = c(2.3,2,2,2.2))
par(mar = c(5.6, 4.1, 4.1, 0))

# Plot LSPM 3D results for variances
vioplot(cbind(1/node100results4d3_c_thin[[paste0("seed",(seed_number[1]))]]$omegas, rep(0,nrow(node100results4d3_c_thin[[paste0("seed",(seed))]]$omegas))),
        ylim=c(0,6), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red=0, green = 0, blue = 0, alpha=0.1),
        main= "p = 3", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2, cex.lab=1.6)
for(seed in seed_number) {
  vioplot(1/node100results4d3_c_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red=0, green = 0, blue = 0, alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05,1.15)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node100results4d3_c_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=2, col="green")

legend("topright", legend=c('Posterior distribution', "True value", 'Average posterior mean'), pch=c(20,4,20), col=c("black","red", "green"), cex=1.4)

par(mar = c(5.6, 0, 4.1, 0))

# Plot LSPM 4D results for variances
vioplot(1/node100results4d4_c_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,6), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red=0, green = 0, blue = 0, alpha=0.1),
        main= "p = 4", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2, cex=1.5)
axis(1, at=1:5, las=1, labels=1:5, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node100results4d4_c_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red=0, green = 0, blue = 0, alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05,1.15)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node100results4d4_c_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=2, col="green")

# legend("topright", legend=c('Posterior', "distribution"), pch=c(15, NA), col=c("magenta3"), cex=1.4)

par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1

# Plot LSPM 8D results for variances
vioplot(1/node100results4d8_c_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,6), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red=0, green = 0, blue = 0, alpha=0.1),
        main= "p = 8", #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node100results4d8_c_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red=0, green = 0, blue = 0, alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05,1.15)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node100results4d8_c_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=2, col="green")

# legend("topright", legend=c('Posterior', "distribution"), pch=c(15, NA), col=c("orange"), cex=1.4)

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

