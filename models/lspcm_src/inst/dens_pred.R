load(file = "~/lspm/inst/extdata/adapt/node50results3d5_0_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_1_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_5_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_10_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_20_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_30_adapt.Rdata")

source("~/lspm/inst/dens_network.R")

# Posterior Predictive Checking
n_pos=30
d = 3
full_pred3d5_0 <- full_pred3d5_1 <- full_pred3d5_5 <- full_pred3d5_10 <- full_pred3d5_20 <- full_pred3d5_30 <- list()

for(seed in seed_number) {
  if(d %in% names(table(node50results3d5_0_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred3d5_0 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object =  node50results3d5_0_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred3d5_0[[paste0("seed",(seed))]] = pred3d5_0
  }

  if(d %in% names(table(node50results3d5_1_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred3d5_1 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node50results3d5_1_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred3d5_1[[paste0("seed",(seed))]] = pred3d5_1
  }

  if(d %in% names(table(node50results3d5_5_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred3d5_5 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node50results3d5_5_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred3d5_5[[paste0("seed",(seed))]] = pred3d5_5
  }

  if(d %in% names(table(node50results3d5_10_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred3d5_10 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node50results3d5_10_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred3d5_10[[paste0("seed",(seed))]] = pred3d5_10
  }

  if(d %in% names(table(node50results3d5_20_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred3d5_20 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node50results3d5_20_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred3d5_20[[paste0("seed",(seed))]] = pred3d5_20
  }

  if(d %in% names(table(node50results3d5_30_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred3d5_30 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node50results3d5_30_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred3d5_30[[paste0("seed",(seed))]] = pred3d5_30
  }
}

# Predictive Check Plots
old.par <- par(no.readonly = TRUE) # save current layout setting

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/dens_pred.pdf", width = 10, height = 4)


mycol=adjustcolor(palette("default"), alpha.f = 0.03)
palette(mycol)
plot_names <- c("Network Density", "Transitivity", "Accuracy", "F1 Score", "Hamming Distance")

obs_d_3d5_0 <- lapply(node50network3d5_0, function(x) gden(x$network))
obs_d_3d5_1 <- lapply(node50network3d5_1, function(x) gden(x$network))
obs_d_3d5_5 <- lapply(node50network3d5_5, function(x) gden(x$network))
obs_d_3d5_10 <- lapply(node50network3d5_10, function(x) gden(x$network))
obs_d_3d5_20 <- lapply(node50network3d5_20, function(x) gden(x$network))
obs_d_3d5_30 <- lapply(node50network3d5_30, function(x) gden(x$network))

obs_t_3d5_0 <- lapply(node50network3d5_0, function(x) gtrans(x$network))
obs_t_3d5_1 <- lapply(node50network3d5_1, function(x) gtrans(x$network))
obs_t_3d5_5 <- lapply(node50network3d5_5, function(x) gtrans(x$network))
obs_t_3d5_10 <- lapply(node50network3d5_10, function(x) gtrans(x$network))
obs_t_3d5_20 <- lapply(node50network3d5_20, function(x) gtrans(x$network))
obs_t_3d5_30 <- lapply(node50network3d5_30, function(x) gtrans(x$network))

par(mfrow=c(1,5))
layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
layout(mat = layout.matrix, widths = c(2.6,1.6,1.6,1.6,1.7))
par(mar = c(5.6, 9.3, 2.1, 0.2))

density3d5_0 = Map('-', lapply(full_pred3d5_0, function(x) x$Density), obs_d_3d5_0[names(full_pred3d5_0)])
density3d5_1 = Map('-', lapply(full_pred3d5_1, function(x) x$Density), obs_d_3d5_1[names(full_pred3d5_1)])
density3d5_5 = Map('-', lapply(full_pred3d5_5, function(x) x$Density), obs_d_3d5_5[names(full_pred3d5_5)])
density3d5_10 = Map('-', lapply(full_pred3d5_10, function(x) x$Density), obs_d_3d5_10[names(full_pred3d5_10)])
density3d5_20 = Map('-', lapply(full_pred3d5_20, function(x) x$Density), obs_d_3d5_20[names(full_pred3d5_20)])
density3d5_30 = Map('-', lapply(full_pred3d5_30, function(x) x$Density), obs_d_3d5_30[names(full_pred3d5_30)])
all_density = Map(cbind, density3d5_0, density3d5_1, density3d5_5, density3d5_10, density3d5_20, density3d5_30)

vioplot(do.call(rbind,all_density), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_density))-0.01,max(unlist(all_density)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(density3d5_30, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density3d5_20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density3d5_10, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density3d5_5, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density3d5_1, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density3d5_0, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=6,
          colMed = NA, border=NA, col=1, horizontal = TRUE))


# lapply(all_density, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
#           colMed = NA, border=NA, col=c(1,1,1,1,4), horizontal = TRUE))
axis(2,1:6, las=2,labels=c(expression(paste(alpha, "=30, ", p[a],"=3")),
                           expression(paste(alpha, "=20, ", p[a],"=3")),
                           expression(paste(alpha, "=10, ", p[a],"=3")),
                           expression(paste(alpha, "=5, ", p[a],"=3")),
                           expression(paste(alpha, "=1, ", p[a],"=3")),
                           expression(paste(alpha, "=0, ", p[a],"=3"))), cex.axis=1.5)

# axis(2,1:6, las=2,labels=c(expression(atop(NA, atop("LSPM", (paste(alpha, "=30, ", p[a],"=3"))))),
#                            expression(atop(NA, atop("LSPM", (paste(alpha, "=20, ", p[a],"=3"))))),
#                            expression(atop(NA, atop("LSPM", (paste(alpha, "=10, ", p[a],"=3"))))),
#                            expression(atop(NA, atop("LSPM", (paste(alpha, "=5, ", p[a],"=3"))))),
#                            expression(atop(NA, atop("LSPM", (paste(alpha, "=1, ", p[a],"=3"))))),
#                            expression(atop(NA, atop("LSPM", (paste(alpha, "=0, ", p[a],"=3")))))), cex.axis=2)

title(xlab=plot_names[1], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)

par(mar = c(5.6, 0, 2.1, 0.2))

transitivity3d5_0 = Map('-', lapply(full_pred3d5_0, function(x) x$Transitivity), obs_t_3d5_0[names(full_pred3d5_0)])
transitivity3d5_1 = Map('-', lapply(full_pred3d5_1, function(x) x$Transitivity), obs_t_3d5_1[names(full_pred3d5_1)])
transitivity3d5_5 = Map('-', lapply(full_pred3d5_5, function(x) x$Transitivity), obs_t_3d5_5[names(full_pred3d5_5)])
transitivity3d5_10 = Map('-', lapply(full_pred3d5_10, function(x) x$Transitivity), obs_t_3d5_10[names(full_pred3d5_10)])
transitivity3d5_20 = Map('-', lapply(full_pred3d5_20, function(x) x$Transitivity), obs_t_3d5_20[names(full_pred3d5_20)])
transitivity3d5_30 = Map('-', lapply(full_pred3d5_30, function(x) x$Transitivity), obs_t_3d5_30[names(full_pred3d5_30)])
all_transitivity = Map(cbind, transitivity3d5_0, transitivity3d5_1, transitivity3d5_5, transitivity3d5_10,
                       transitivity3d5_20, transitivity3d5_30)

vioplot(do.call(rbind,all_transitivity), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_transitivity))-0.01,max(unlist(all_transitivity)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(transitivity3d5_30, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity3d5_20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity3d5_10, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity3d5_5, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity3d5_1, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity3d5_0, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=6,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[2], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)


accuracy3d5_0 = lapply(full_pred3d5_0, function(x) x$Accuracy)
accuracy3d5_1 = lapply(full_pred3d5_1, function(x) x$Accuracy)
accuracy3d5_5 = lapply(full_pred3d5_5, function(x) x$Accuracy)
accuracy3d5_10 = lapply(full_pred3d5_10, function(x) x$Accuracy)
accuracy3d5_20 = lapply(full_pred3d5_20, function(x) x$Accuracy)
accuracy3d5_30 = lapply(full_pred3d5_30, function(x) x$Accuracy)
all_accuracy = Map(cbind, accuracy3d5_0, accuracy3d5_1, accuracy3d5_5, accuracy3d5_10,
                   accuracy3d5_20, accuracy3d5_30)

vioplot(do.call(rbind,all_accuracy), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_accuracy))-0.01,max(unlist(all_accuracy)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(accuracy3d5_30, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy3d5_20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy3d5_10, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy3d5_5, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy3d5_1, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy3d5_0, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=6,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[3], line=2.75, cex.lab=1.6)


F1score3d5_0 = lapply(full_pred3d5_0, function(x) x$F1score)
F1score3d5_1 = lapply(full_pred3d5_1, function(x) x$F1score)
F1score3d5_5 = lapply(full_pred3d5_5, function(x) x$F1score)
F1score3d5_10 = lapply(full_pred3d5_10, function(x) x$F1score)
F1score3d5_20 = lapply(full_pred3d5_20, function(x) x$F1score)
F1score3d5_30 = lapply(full_pred3d5_30, function(x) x$F1score)
all_F1score = Map(cbind, F1score3d5_0, F1score3d5_1, F1score3d5_5,
                  F1score3d5_10, F1score3d5_20, F1score3d5_30)

vioplot(do.call(rbind,all_F1score), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_F1score))-0.01,max(unlist(all_F1score)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(F1score3d5_30, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score3d5_20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score3d5_10, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score3d5_5, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score3d5_1, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score3d5_0, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=6,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[4], line=2.75, cex.lab=1.6)

par(mar = c(5.6, 0, 2.1, 1)) # right margin set as 0.1

Hamming3d5_0 = lapply(full_pred3d5_0, function(x) x$Hamming)
Hamming3d5_1 = lapply(full_pred3d5_1, function(x) x$Hamming)
Hamming3d5_5 = lapply(full_pred3d5_5, function(x) x$Hamming)
Hamming3d5_10 = lapply(full_pred3d5_10, function(x) x$Hamming)
Hamming3d5_20 = lapply(full_pred3d5_20, function(x) x$Hamming)
Hamming3d5_30 = lapply(full_pred3d5_30, function(x) x$Hamming)
all_Hamming = Map(cbind, Hamming3d5_0, Hamming3d5_1, Hamming3d5_5,
                  Hamming3d5_10, Hamming3d5_20, Hamming3d5_30)

vioplot(do.call(rbind,all_Hamming), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_Hamming))-0.01,max(unlist(all_Hamming)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(Hamming3d5_30, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming3d5_20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming3d5_10, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming3d5_5, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming3d5_1, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming3d5_0, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=6,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[5], line=2.75, cex.lab=1.6)

par(old.par) # restore previous plot layout setting

# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()
