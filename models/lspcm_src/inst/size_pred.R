load(file = "~/lspm/inst/extdata/adapt/node20results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node200results_adapt.Rdata")

source("~/lspm/inst/size_network.R")

# Posterior Predictive Checking
n_pos=30
d = 2
full_pred20 <- full_pred50 <- full_pred100 <- full_pred200 <- list()

for(seed in seed_number) {
  if(d %in% names(table(node20results_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred20 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node20results_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred20[[paste0("seed",(seed))]] = pred20
  }

  if(d %in% names(table(node50results_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred50 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node50results_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred50[[paste0("seed",(seed))]] = pred50
  }

  if(d %in% names(table(node100results_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred100 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node100results_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred100[[paste0("seed",(seed))]] = pred100
  }

  if(d %in% names(table(node200results_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred200 <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node200results_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred200[[paste0("seed",(seed))]] = pred200
  }
}

# Predictive Check Plots
old.par <- par(no.readonly = TRUE) # save current layout setting

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/size_pred.pdf", width = 10, height = 4)


mycol=adjustcolor(palette("default"), alpha.f = 0.03)
palette(mycol)
plot_names <- c("Network Density", "Transitivity", "Accuracy", "F1 Score", "Hamming Distance")
obs_d20 <- lapply(node20network, function(x) gden(x$network))
obs_d50 <- lapply(node50network, function(x) gden(x$network))
obs_d100 <- lapply(node100network, function(x) gden(x$network))
obs_d200 <- lapply(node200network, function(x) gden(x$network))

obs_t20 <- lapply(node20network, function(x) gtrans(x$network))
obs_t50 <- lapply(node50network, function(x) gtrans(x$network))
obs_t100 <- lapply(node100network, function(x) gtrans(x$network))
obs_t200 <- lapply(node200network, function(x) gtrans(x$network))

par(mfrow=c(1,5))
layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
layout(mat = layout.matrix, widths = c(2.6,1.6,1.6,1.6,1.7))
par(mar = c(5.6, 9.3, 2.1, 0.2))

density20 = Map('-', lapply(full_pred20, function(x) x$Density), obs_d20[names(full_pred20)])
density50 = Map('-', lapply(full_pred50, function(x) x$Density), obs_d50[names(full_pred50)])
density100 = Map('-', lapply(full_pred100, function(x) x$Density), obs_d100[names(full_pred100)])
density200 = Map('-', lapply(full_pred200, function(x) x$Density), obs_d200[names(full_pred200)])
all_density = Map(cbind, density20, density50, density100, density200)

vioplot(do.call(rbind,all_density), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_density))-0.01,max(unlist(all_density)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(density200, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density100, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density50, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))


# lapply(all_density, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
#           colMed = NA, border=NA, col=c(1,1,1,1,4), horizontal = TRUE))
axis(2,1:4, las=2,labels=c(expression(paste("n=200, ", p,"=2")),
                           expression(paste("n=100, ", p,"=2")),
                           expression(paste("n=50, ", p,"=2")),
                           expression(paste("n=20, ", p,"=2"))), cex.axis=1.5)

# axis(2,1:4, las=2,labels=c(expression(atop(NA, atop("LSPM", (paste("n=200, ", p,"=2"))))),
#                            expression(atop(NA, atop("LSPM", (paste("n=100, ", p,"=2"))))),
#                            expression(atop(NA, atop("LSPM", (paste("n=50, ", p,"=2"))))),
#                            expression(atop(NA, atop("LSPM", (paste("n=20, ", p,"=2")))))), cex.axis=2)

title(xlab=plot_names[1], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)

par(mar = c(5.6, 0, 2.1, 0.2))

transitivity20 = Map('-', lapply(full_pred20, function(x) x$Transitivity), obs_t20[names(full_pred20)])
transitivity50 = Map('-', lapply(full_pred50, function(x) x$Transitivity), obs_t50[names(full_pred50)])
transitivity100 = Map('-', lapply(full_pred100, function(x) x$Transitivity), obs_t100[names(full_pred100)])
transitivity200 = Map('-', lapply(full_pred200, function(x) x$Transitivity), obs_t200[names(full_pred200)])
all_transitivity = Map(cbind, transitivity20, transitivity50,
                       transitivity100, transitivity200)

vioplot(do.call(rbind,all_transitivity), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_transitivity))-0.01,max(unlist(all_transitivity)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(transitivity200, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity100, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity50, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[2], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)


accuracy20 = lapply(full_pred20, function(x) x$Accuracy)
accuracy50 = lapply(full_pred50, function(x) x$Accuracy)
accuracy100 = lapply(full_pred100, function(x) x$Accuracy)
accuracy200 = lapply(full_pred200, function(x) x$Accuracy)
all_accuracy = Map(cbind, accuracy20, accuracy50,
                   accuracy100, accuracy200)

vioplot(do.call(rbind,all_accuracy), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_accuracy))-0.01,max(unlist(all_accuracy)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(accuracy200, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy100, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy50, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[3], line=2.75, cex.lab=1.6)


F1score20 = lapply(full_pred20, function(x) x$F1score)
F1score50 = lapply(full_pred50, function(x) x$F1score)
F1score100 = lapply(full_pred100, function(x) x$F1score)
F1score200 = lapply(full_pred200, function(x) x$F1score)
all_F1score = Map(cbind, F1score20, F1score50,
                  F1score100, F1score200)

vioplot(do.call(rbind,all_F1score), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_F1score))-0.01,max(unlist(all_F1score)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(F1score200, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score100, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score50, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[4], line=2.75, cex.lab=1.6)

par(mar = c(5.6, 0, 2.1, 1)) # right margin set as 0.1


Hamming20 = lapply(full_pred20, function(x) x$Hamming)
Hamming50 = lapply(full_pred50, function(x) x$Hamming)
Hamming100 = lapply(full_pred100, function(x) x$Hamming)
Hamming200 = lapply(full_pred200, function(x) x$Hamming)
all_Hamming = Map(cbind, Hamming20, Hamming50,
                  Hamming100, Hamming200)

vioplot(do.call(rbind,all_Hamming), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_Hamming))-0.01,max(unlist(all_Hamming)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(Hamming200, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming100, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming50, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming20, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))

title(xlab=plot_names[5], line=2.75, cex.lab=1.6)

par(old.par) # restore previous plot layout setting

# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()
