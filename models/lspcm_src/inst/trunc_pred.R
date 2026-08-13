load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_low_d.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_same_d.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_high_d.Rdata")

source("~/lspm/inst/trunc_network.R")

load(file = "~/lspm/inst/extdata/binary/node100results4d4net.Rdata")

# # Fitting LPM 4D
# node100results4d4net <- list()
# for(seed in seed_number) {
#
#   lspm_single_result <- list(ergmm(node100network4d[[paste0("seed",(seed))]]$network ~ euclidean(d=4), seed=115))
#   names(lspm_single_result) <- paste0("seed",(seed))
#   node100results4d4net <-  append(node100results4d4net, lspm_single_result)
# }

# save(node100results4d4net, file = "node100results4d4net.Rdata")


# Posterior Predictive Checking
n_pos=30
d = 4
full_pred4d_adapt_high_d <- full_pred4d_adapt_same_d <- full_pred4d_adapt_low_d <- full_pred4d_adapt <- full_true4d <- full_pred4d4_latent <- list()

for(seed in seed_number) {
  if(d %in% names(table(node100results4d3_adapt_high_d[[paste0("seed",(seed))]]$iter_d))) {
    pred4d_adapt_high_d <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt_high_d[[paste0("seed",(seed))]], seed = 1234))
    full_pred4d_adapt_high_d[[paste0("seed",(seed))]] = pred4d_adapt_high_d
  }

  if(d %in% names(table(node100results4d3_adapt_same_d[[paste0("seed",(seed))]]$iter_d))) {
    pred4d_adapt_same_d <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt_same_d[[paste0("seed",(seed))]], seed = 1234))
    full_pred4d_adapt_same_d[[paste0("seed",(seed))]] = pred4d_adapt_same_d
  }

  if(d %in% names(table(node100results4d3_adapt_low_d[[paste0("seed",(seed))]]$iter_d))) {
    pred4d_adapt_low_d <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt_low_d[[paste0("seed",(seed))]], seed = 1234))
    full_pred4d_adapt_low_d[[paste0("seed",(seed))]] = pred4d_adapt_low_d
  }

  if(d %in% names(table(node100results4d3_adapt[[paste0("seed",(seed))]]$iter_d))) {
    pred4d_adapt <- as.data.frame(predcheck(n_pos, n_dimen = d, LSPM_object = node100results4d3_adapt[[paste0("seed",(seed))]], seed = 1234))
    full_pred4d_adapt[[paste0("seed",(seed))]] = pred4d_adapt
  }

  # Predictives check of LPM results
  fit_latent <- as.data.frame(predcheck(n_pos,
                                        alpha_chain = node100results4d4net[[paste0("seed",(seed))]]$sample$beta,
                                        latent_pos_chain = aperm(node100results4d4net[[paste0("seed",(seed))]]$sample$Z, c(2,3,1)),
                                        network = node100network4d[[paste0("seed",(seed))]]$network, dist_power=1, seed = 1234))
  full_pred4d4_latent[[paste0("seed",(seed))]] = fit_latent
}

# Predictive Check Plots
old.par <- par(no.readonly = TRUE) # save current layout setting

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/trunc_pred.pdf", width = 10, height = 4)

palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
mycol=adjustcolor(palette("default"), alpha.f = 0.03)
palette(mycol)
plot_names <- c("Network Density", "Transitivity", "Accuracy", "F1 Score", "Hamming Distance")
obs_d <- lapply(node100network4d, function(x) gden(x$network))
obs_t <- lapply(node100network4d, function(x) gtrans(x$network))

par(mfrow=c(1,5))
layout.matrix <- matrix(1:5, nrow = 1, ncol = 5)
layout(mat = layout.matrix, widths = c(2.6,1.6,1.6,1.6,1.7))
par(mar = c(5.6, 9.3, 2.1, 0.2))

density4d_high_d = Map('-', lapply(full_pred4d_adapt_high_d, function(x) x$Density), obs_d[names(full_pred4d_adapt_high_d)])
density4d_same_d = Map('-', lapply(full_pred4d_adapt_same_d, function(x) x$Density), obs_d[names(full_pred4d_adapt_same_d)])
density4d_low_d = Map('-', lapply(full_pred4d_adapt_low_d, function(x) x$Density), obs_d[names(full_pred4d_adapt_low_d)])
density4d = Map('-', lapply(full_pred4d_adapt, function(x) x$Density), obs_d[names(full_pred4d_adapt)])
density4d4lpm = Map('-', lapply(full_pred4d4_latent, function(x) x$Density), obs_d)
all_density = Map(cbind, density4d_high_d, density4d_same_d, density4d_low_d, density4d, density4d4lpm)

vioplot(do.call(rbind,all_density), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_density))-0.01,max(unlist(all_density)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(density4d_high_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density4d_same_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density4d_low_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(density4d4lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=4, horizontal = TRUE))


# lapply(all_density, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
#           colMed = NA, border=NA, col=c(1,1,1,1,4), horizontal = TRUE))
axis(2,1:5, las=2,labels=c(expression(paste(p[0],"=10, ", p,"=4")),
                           expression(paste(p[0],"=4, ", p,"=4")),
                           expression(paste(p[0],"=2, ", p,"=4")),
                           expression(paste(p[0],"=auto, ", p,"=4")),
                           expression("p=4")), cex.axis=1.5)

# axis(2,1:5, las=2,labels=c(expression(atop(NA, atop("LSPM", (paste(p[0],"=10, ", p,"=4"))))),
#                            expression(atop(NA, atop("LSPM", (paste(p[0],"=4, ", p,"=4"))))),
#                            expression(atop(NA, atop("LSPM", (paste(p[0],"=2, ", p,"=4"))))),
#                            expression(atop(NA, atop("LSPM", (paste(p[0],"=auto, ", p,"=4"))))),
#                            expression(atop(NA, atop("LPM","(p=4)")))), cex.axis=2)

title(xlab=plot_names[1], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)

par(mar = c(5.6, 0, 2.1, 0.2))

transitivity4d_high_d = Map('-', lapply(full_pred4d_adapt_high_d, function(x) x$Transitivity), obs_t[names(full_pred4d_adapt_high_d)])
transitivity4d_same_d = Map('-', lapply(full_pred4d_adapt_same_d, function(x) x$Transitivity), obs_t[names(full_pred4d_adapt_same_d)])
transitivity4d_low_d = Map('-', lapply(full_pred4d_adapt_low_d, function(x) x$Transitivity), obs_t[names(full_pred4d_adapt_low_d)])
transitivity4d = Map('-', lapply(full_pred4d_adapt, function(x) x$Transitivity), obs_t[names(full_pred4d_adapt)])
transitivity4d4lpm = Map('-', lapply(full_pred4d4_latent, function(x) x$Transitivity), obs_t)
all_transitivity = Map(cbind, transitivity4d_high_d, transitivity4d_same_d,
                       transitivity4d_low_d, transitivity4d, transitivity4d4lpm)

vioplot(do.call(rbind,all_transitivity), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_transitivity))-0.01,max(unlist(all_transitivity)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(transitivity4d_high_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity4d_same_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity4d_low_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(transitivity4d4lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=4, horizontal = TRUE))


# transitivity4d3 = Map('-', lapply(full_pred4d3, function(x) x$Transitivity), obs_t[names(full_pred4d_adapt_high_d)])
# transitivity4d4 = Map('-', lapply(full_pred4d4, function(x) x$Transitivity), obs_t[names(full_pred4d_adapt_same_d)])
# transitivity4d8 = Map('-', lapply(full_pred4d8, function(x) x$Transitivity), obs_t[names(full_pred4d_adapt_low_d)])
# transitivity4d4lpm = Map('-', lapply(full_pred4d4_latent, function(x) x$Transitivity), obs_t)
# all_transitivity = Map(cbind, transitivity4d8, transitivity4d4, transitivity4d3, transitivity4d4lpm)
#
# vioplot(do.call(rbind,all_transitivity), cex=0.2, rectCol = NA, lineCol = NA,
#         xaxt="n", cex.names=1.4, horizontal = TRUE,
#         # ylim=c(0,1),
#         ylim=c(min(unlist(all_transitivity))-0.02,max(unlist(all_transitivity)+0.02)),
#         colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
# lapply(all_transitivity, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
#           colMed = NA, border=NA, col=c(1,1,1,4), horizontal = TRUE))
title(xlab=plot_names[2], line=2.75, cex.lab=1.6)
abline(v=0, col ="red", lty=2, lwd=2)


accuracy4d_high_d = lapply(full_pred4d_adapt_high_d, function(x) x$Accuracy)
accuracy4d_same_d = lapply(full_pred4d_adapt_same_d, function(x) x$Accuracy)
accuracy4d_low_d = lapply(full_pred4d_adapt_low_d, function(x) x$Accuracy)
accuracy4d = lapply(full_pred4d_adapt, function(x) x$Accuracy)
accuracy4d4lpm = lapply(full_pred4d4_latent, function(x) x$Accuracy)
all_accuracy = Map(cbind, accuracy4d_high_d, accuracy4d_same_d,
                   accuracy4d_low_d, accuracy4d, accuracy4d4lpm)

vioplot(do.call(rbind,all_accuracy), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_accuracy))-0.01,max(unlist(all_accuracy)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(accuracy4d_high_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy4d_same_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy4d_low_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(accuracy4d4lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=4, horizontal = TRUE))

# accuracy4d3 = lapply(full_pred4d3, function(x) x$Accuracy)
# accuracy4d4 = lapply(full_pred4d4, function(x) x$Accuracy)
# accuracy4d8 = lapply(full_pred4d8, function(x) x$Accuracy)
# accuracy4d4lpm = lapply(full_pred4d4_latent, function(x) x$Accuracy)
# all_accuracy = Map(cbind, accuracy4d8, accuracy4d4, accuracy4d3, accuracy4d4lpm)
#
# vioplot(do.call(rbind,all_accuracy), cex=0.2, rectCol = NA, lineCol = NA,
#         xaxt="n", cex.names=1.4, horizontal = TRUE,
#         # ylim=c(0,1),
#         ylim=c(min(unlist(all_accuracy))-0.02,max(unlist(all_accuracy)+0.02)),
#         colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
# lapply(all_accuracy, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
#           colMed = NA, border=NA, col=c(1,1,1,4), horizontal = TRUE))
title(xlab=plot_names[3], line=2.75, cex.lab=1.6)


F1score4d_high_d = lapply(full_pred4d_adapt_high_d, function(x) x$F1score)
F1score4d_same_d = lapply(full_pred4d_adapt_same_d, function(x) x$F1score)
F1score4d_low_d = lapply(full_pred4d_adapt_low_d, function(x) x$F1score)
F1score4d = lapply(full_pred4d_adapt, function(x) x$F1score)
F1score4d4lpm = lapply(full_pred4d4_latent, function(x) x$F1score)
all_F1score = Map(cbind, F1score4d_high_d, F1score4d_same_d,
                  F1score4d_low_d, F1score4d, F1score4d4lpm)

vioplot(do.call(rbind,all_F1score), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_F1score))-0.01,max(unlist(all_F1score)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(F1score4d_high_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score4d_same_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score4d_low_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(F1score4d4lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=4, horizontal = TRUE))


# F1score4d3 = lapply(full_pred4d3, function(x) x$F1score)
# F1score4d4 = lapply(full_pred4d4, function(x) x$F1score)
# F1score4d8 = lapply(full_pred4d8, function(x) x$F1score)
# F1score4d4lpm = lapply(full_pred4d4_latent, function(x) x$F1score)
# all_F1score = Map(cbind, F1score4d8, F1score4d4, F1score4d3, F1score4d4lpm)
#
# vioplot(do.call(rbind,all_F1score), cex=0.2, rectCol = NA, lineCol = NA,
#         xaxt="n", cex.names=1.4, horizontal = TRUE,
#         # ylim=c(0,1),
#         ylim=c(min(unlist(all_F1score))-0.02,max(unlist(all_F1score)+0.02)),
#         colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
# lapply(all_F1score, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
#           colMed = NA, border=NA, col=c(1,1,1,4), horizontal = TRUE))
title(xlab=plot_names[4], line=2.75, cex.lab=1.6)

par(mar = c(5.6, 0, 2.1, 1)) # right margin set as 0.1


Hamming4d_high_d = lapply(full_pred4d_adapt_high_d, function(x) x$Hamming)
Hamming4d_same_d = lapply(full_pred4d_adapt_same_d, function(x) x$Hamming)
Hamming4d_low_d = lapply(full_pred4d_adapt_low_d, function(x) x$Hamming)
Hamming4d = lapply(full_pred4d_adapt, function(x) x$Hamming)
Hamming4d4lpm = lapply(full_pred4d4_latent, function(x) x$Hamming)
all_Hamming = Map(cbind, Hamming4d_high_d, Hamming4d_same_d,
                  Hamming4d_low_d, Hamming4d, Hamming4d4lpm)

vioplot(do.call(rbind,all_Hamming), cex=0.2, rectCol = NA, lineCol = NA,
        xaxt="n", cex.names=1.4, horizontal = TRUE,
        # ylim=c(0,1),
        ylim=c(min(unlist(all_Hamming))-0.01,max(unlist(all_Hamming)+0.01)),
        colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))

lapply(Hamming4d_high_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=1,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming4d_same_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=2,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming4d_low_d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=3,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming4d, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=4,
          colMed = NA, border=NA, col=1, horizontal = TRUE))
lapply(Hamming4d4lpm, function(x)
  vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5, at=5,
          colMed = NA, border=NA, col=4, horizontal = TRUE))


# Hamming4d3 = lapply(full_pred4d3, function(x) x$Hamming)
# Hamming4d4 = lapply(full_pred4d4, function(x) x$Hamming)
# Hamming4d8 = lapply(full_pred4d8, function(x) x$Hamming)
# Hamming4d4lpm = lapply(full_pred4d4_latent, function(x) x$Hamming)
# all_Hamming = Map(cbind, Hamming4d8, Hamming4d4, Hamming4d3, Hamming4d4lpm)
#
# vioplot(do.call(rbind,all_Hamming), cex=0.2, rectCol = NA, lineCol = NA,
#         xaxt="n", cex.names=1.4, horizontal = TRUE,
#         # ylim=c(0,1),
#         ylim=c(min(unlist(all_Hamming))-0.02,max(unlist(all_Hamming)+0.02)),
#         colMed = NA, border=NA, col=rgb(red = 1, green=0.8, blue=1,alpha=0))
# lapply(all_Hamming, function(x)
#   vioplot(x, add=TRUE, cex=0.2, rectCol = NA, lineCol = NA, cex.lab=1.5,
#           colMed = NA, border=NA, col=c(1,1,1,4), horizontal = TRUE))
title(xlab=plot_names[5], line=2.75, cex.lab=1.6)

par(old.par) # restore previous plot layout setting

# palette("R3") # default palette
palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# Close the pdf file
dev.off()

