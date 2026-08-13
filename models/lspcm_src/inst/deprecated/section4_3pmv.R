load(file = "~/lspm/inst/extdata/binary/node50results3d5_0_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_1_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_5_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_10_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_20_thin.Rdata")
load(file = "~/lspm/inst/extdata/binary/node50results3d5_30_thin.Rdata")

source("~/lspm/inst/section4_3network.R")

# open the pdf file
cairo_pdf("~/lspm/inst/figures/pmv4_3.pdf", width = 10, height = 4)

# Setup the plotting area
old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,6))
layout.matrix <- matrix(1:6, nrow = 1, ncol = 6)
layout(mat = layout.matrix, widths = c(2.6,2,2,2,2,2.2))
par(mar = c(5.6, 4.1, 4.1, 0))


# alpha = 0 ----------------------------------------------------------------

# Plot LSPM 5D results for variances, 20 nodes, alpha = 0
vioplot(cbind(1/node50results3d5_0_thin[[paste0("seed",(seed_number[1]))]]$omegas),
        ylim=c(0,20), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= expression(paste(alpha, " = 0")), #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2, cex.lab=1.5)
title(main = ("(2-5% e.d.)"),  line = 0.5, cex.main = 1.3, cex.lab=1.5, font.main=1)
for(seed in seed_number) {
  vioplot(1/node50results3d5_0_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node50results3d5_0_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=1.5, col="green")

legend("topright", legend=c('Posterior distribution', "True value", 'Average posterior mean'), pch=c(20,4,20), col=c("black","red", "green"), cex=1)


# alpha = 1 ----------------------------------------------------------------


par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0


vioplot(1/node50results3d5_1_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,20), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= expression(paste(alpha, " = 1")), #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
title(main = ("(4-8% e.d.)"),  line = 0.5, cex.main = 1.3, font.main=1)
axis(1, at=1:5, las=1, labels=1:5, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node50results3d5_1_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node50results3d5_1_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=1.5, col="green")



# alpha = 5 ---------------------------------------------------------------


vioplot(1/node50results3d5_5_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,20), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= expression(paste(alpha, " = 5")), #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
title(main = ("(20-35% e.d.)"),  line = 0.5, cex.main = 1.3, font.main=1)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node50results3d5_5_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node50results3d5_5_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=1.5, col="green")


# alpha = 10 ---------------------------------------------------------------


vioplot(1/node50results3d5_10_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,20), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= expression(paste(alpha, " = 10")), #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
title(main = ("(49-65% e.d.)"),  line = 0.5, cex.main = 1.3, font.main=1)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node50results3d5_10_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node50results3d5_10_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=1.5, col="green")

# alpha = 20 ---------------------------------------------------------------

vioplot(1/node50results3d5_20_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,20), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= expression(paste(alpha, " = 20")), #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2)
title(main = ("(79-94% e.d.)"),  line = 0.5, cex.main = 1.3, font.main=1)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node50results3d5_20_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node50results3d5_20_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=1.5, col="green")

# alpha = 30 ---------------------------------------------------------------
par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1


vioplot(1/node50results3d5_30_thin[[paste0("seed",(seed_number[1]))]]$omegas,
        ylim=c(0,20), cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, cex.main=1.5, cex.axis=1.5,
        colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1),
        main= expression(paste(alpha, " = 30")), #main='Posterior Mean Variance vs Dimension',
        xlab= "", ylab="", yaxt="n")
title(xlab=expression(paste("Dimension")), cex.lab=1.5, line=2.5)
title(ylab=expression(paste("Variance")), line=2.2,)
title(main = ("(90-99% e.d.)"),  line = 0.5, cex.main = 1.3, cex.lab=1.5, font.main=1)
axis(1, at=1:8, las=1, labels=1:8, cex.axis=1.5)
for(seed in seed_number) {
  vioplot(1/node50results3d5_30_thin[[paste0("seed",(seed))]]$omegas, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
          colMed = NA, border=NA, col=rgb(red = 0, green=0, blue=0,alpha=0.1))
}
points(1/cumprod(c(0.5,1.1,1.05)), col='red', pch=4, cex=2, lwd=4) # true
points(apply(t(as.data.frame(lapply(node50results3d5_30_thin,function(x) apply(1/x$omegas,2,mean)))),2,mean),pch=20, cex=1.5, col="green")

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()

