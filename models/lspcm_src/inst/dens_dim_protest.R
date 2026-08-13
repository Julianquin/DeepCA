load(file = "~/lspm/inst/extdata/adapt/node50results3d5_0_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_1_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_5_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_10_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_20_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results3d5_30_adapt.Rdata")

source("~/lspm/inst/dens_network.R")

library(vegan)

rbind(
  calculate_mode(do.call(rbind, lapply(node50results3d5_0_adapt, function(x) x$iter_d))),
  calculate_mode(do.call(rbind, lapply(node50results3d5_1_adapt, function(x) x$iter_d))),
  calculate_mode(do.call(rbind, lapply(node50results3d5_5_adapt, function(x) x$iter_d))),
  calculate_mode(do.call(rbind, lapply(node50results3d5_10_adapt, function(x) x$iter_d))),
  calculate_mode(do.call(rbind, lapply(node50results3d5_20_adapt, function(x) x$iter_d))),
  calculate_mode(do.call(rbind, lapply(node50results3d5_30_adapt, function(x) x$iter_d)))
)

rbind(
  quantile(do.call(rbind, lapply(node50results3d5_0_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node50results3d5_1_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node50results3d5_5_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node50results3d5_10_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node50results3d5_20_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node50results3d5_30_adapt, function(x) x$iter_d)), c(0.025,0.975))
)




table(do.call(rbind, lapply(node50results3d5_0_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_0_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node50results3d5_1_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_1_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node50results3d5_5_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_5_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node50results3d5_10_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_10_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node50results3d5_20_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_20_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node50results3d5_30_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_30_adapt, function(x) x$iter_d)))

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/dens_dim_bar.pdf", width = 13, height = 4)

par(mfrow=c(1,6))
par(mar = c(5.6, 4.5, 4.1, 0))
barplot(table(do.call(rbind, lapply(node50results3d5_0_adapt, function(x) x$iter_d)))[1:6]/ length(do.call(rbind, lapply(node50results3d5_0_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
# title(main="n = 20", line=1.3, cex.main=1.5)
title(main=expression(paste(alpha, " = 0")), line=1.3, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node50results3d5_1_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_1_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(alpha, " = 1")), line=1.3, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node50results3d5_5_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_5_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(alpha, " = 5")), line=1.3, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node50results3d5_10_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_10_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(alpha, " = 10")), line=1.3, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node50results3d5_20_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_20_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(alpha, " = 20")), line=1.3, cex.main=1.5)

par(mar = c(5.6, 4.5, 4.1, 0.25)) # right margin set as 0.1

barplot(table(do.call(rbind, lapply(node50results3d5_30_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results3d5_30_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
# title(main="n = 200", line=1.3, cex.main=1.5)
title(main=expression(paste(alpha, " = 30")), line=1.3, cex.main=1.5)
dev.off()

# # Proportion across 30 networks for dim. prop. (not used) -----------------
# # open the pdf file
# pdf("~/lspm/inst/extdata/adapt/figure/dens_dim_box.pdf", width = 15, height = 4)
#
# par(mfrow=c(1,6))
# layout.matrix <- matrix(1:6, nrow = 1, ncol = 6)
# layout(mat = layout.matrix, widths = c(2.6,1.8,1.8,1.8,1.8,1.9))
# par(mar = c(5.6, 6.5, 4.1, 0))
#
# boxplot(do.call(rbind,lapply(node50results3d5_0_adapt, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4)
# # title(main=expression(paste(p[0]," automatically chosen")), line=2.3, cex.main=1.5)
# title(main=expression(paste(alpha, " = 0")), line=1.3, cex.main=1.5)
#
# par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0
#
# boxplot(do.call(rbind,lapply(node50results3d5_1_adapt, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," lower than truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste(alpha, " = 1")), line=1.3, cex.main=1.5)
#
# boxplot(do.call(rbind,lapply(node50results3d5_5_adapt, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," same as truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste(alpha, " = 5")), line=1.3, cex.main=1.5)
#
# boxplot(do.call(rbind,lapply(node50results3d5_10_adapt, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," same as truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste(alpha, " = 10")), line=1.3, cex.main=1.5)
#
# boxplot(do.call(rbind,lapply(node50results3d5_20_adapt, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," same as truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste(alpha, " = 20")), line=1.3, cex.main=1.5)
#
#
# par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1
#
# boxplot(do.call(rbind,lapply(node50results3d5_30_adapt, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," higher than truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste(alpha, " = 30")), line=1.3, cex.main=1.5)
#
# dev.off()


##############



# Procrustes Correlation --------------------------------------------------
d=3
full_z_protest3d5_0 <- full_z_protest3d5_1 <- full_z_protest3d5_5 <- full_z_protest3d5_10 <- full_z_protest3d5_20 <- full_z_protest3d5_30 <- list()
for(seed in seed_number) {
d=2
  # if(d %in% names(table(node50results3d5_0_adapt[[paste0("seed",(seed))]]$iter_d))) {
  if(names(which.max(table(node50results3d5_0_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node50results3d5_0_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network3d5_0[[paste0("seed",(seed))]]$positions[,1:d])
    full_z_protest3d5_0 <- append(full_z_protest3d5_0, list(temp=z_protest))
    names(full_z_protest3d5_0)[names(full_z_protest3d5_0)=="temp"] <- paste0("seed",(seed))
  }
d=2
  if(names(which.max(table(node50results3d5_1_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node50results3d5_1_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network3d5_1[[paste0("seed",(seed))]]$positions[,1:d])
    full_z_protest3d5_1 <- append(full_z_protest3d5_1, list(temp=z_protest))
    names(full_z_protest3d5_1)[names(full_z_protest3d5_1)=="temp"] <- paste0("seed",(seed))
  }
d=3
  if(names(which.max(table(node50results3d5_5_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node50results3d5_5_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network3d5_5[[paste0("seed",(seed))]]$positions)
    full_z_protest3d5_5 <- append(full_z_protest3d5_5, list(temp=z_protest))
    names(full_z_protest3d5_5)[names(full_z_protest3d5_5)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(node50results3d5_10_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node50results3d5_10_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network3d5_10[[paste0("seed",(seed))]]$positions)
    full_z_protest3d5_10 <- append(full_z_protest3d5_10, list(temp=z_protest))
    names(full_z_protest3d5_10)[names(full_z_protest3d5_10)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(node50results3d5_20_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node50results3d5_20_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network3d5_20[[paste0("seed",(seed))]]$positions)
    full_z_protest3d5_20 <- append(full_z_protest3d5_20, list(temp=z_protest))
    names(full_z_protest3d5_20)[names(full_z_protest3d5_20)=="temp"] <- paste0("seed",(seed))
  }
d=2
  if(names(which.max(table(node50results3d5_30_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node50results3d5_30_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network3d5_30[[paste0("seed",(seed))]]$positions[,1:d])
    full_z_protest3d5_30 <- append(full_z_protest3d5_30, list(temp=z_protest))
    names(full_z_protest3d5_30)[names(full_z_protest3d5_30)=="temp"] <- paste0("seed",(seed))
  }
}
# Proportion of posterior mode dimension equal truth
sum(do.call(c,lapply(node50results3d5_0_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results3d5_0_adapt)
sum(do.call(c,lapply(node50results3d5_1_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results3d5_1_adapt)
sum(do.call(c,lapply(node50results3d5_5_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results3d5_5_adapt)
sum(do.call(c,lapply(node50results3d5_10_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results3d5_10_adapt)
sum(do.call(c,lapply(node50results3d5_20_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results3d5_20_adapt)
sum(do.call(c,lapply(node50results3d5_30_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results3d5_30_adapt)



# Procrustes correlation median
round(rbind(
  median(do.call(rbind, lapply(full_z_protest3d5_0, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest3d5_1, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest3d5_5, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest3d5_10, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest3d5_20, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest3d5_30, function(x) x$scale)))
),3)

# Procrustes correlation 95% credible interval
round(rbind(
  quantile((do.call(rbind, lapply(full_z_protest3d5_0, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest3d5_1, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest3d5_5, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest3d5_10, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest3d5_20, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest3d5_30, function(x) x$scale))), c(0.025, 0.975))
), 3)

# Procrustes correlation plot ----------------------------------
# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/dens_protest.pdf", width = 5, height = 4)
par(mar = c(4.6, 4.5, 3.6, 0.5)) # right margin set as 0.1

tempprotest=cbind(-1,-1,-1,-1,-1,-1)
colnames(tempprotest) <- c("0", "1", "5", "10", "20", "30")
boxplot(tempprotest, xlab=expression(paste("True ", alpha)), ylab="Procrustes correlation", ylim=c(0,1), cex.lab=1, cex.axis=1)
boxplot(do.call(rbind, lapply(full_z_protest3d5_0, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=1)
boxplot(do.call(rbind, lapply(full_z_protest3d5_1, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=2)
boxplot(do.call(rbind, lapply(full_z_protest3d5_5, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=3)
boxplot(do.call(rbind, lapply(full_z_protest3d5_10, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=4)
boxplot(do.call(rbind, lapply(full_z_protest3d5_20, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=5)
boxplot(do.call(rbind, lapply(full_z_protest3d5_30, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=6)

axis(3,1:6,labels=c("2-5%", "4-8%", "20-35%", "49-65%", "79-94%", "90-99%"), cex.axis=0.75)
title(main=expression(paste("Empirical network density")), line=2.6, cex.main=1.1)
# title(xlab="(b)", line=2)

par(old.par) # restore previous plot layout setting

# Close the pdf file
dev.off()
