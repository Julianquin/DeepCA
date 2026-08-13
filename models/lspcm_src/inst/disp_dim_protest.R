load(file = "~/lspm/inst/extdata/adapt/count2d0_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d1_result_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/count2d5_result_adapt.Rdata")

source("~/lspm/inst/disp_network_c.R")

library(vegan)

# # Proportion posterior mode (not used) ------------------------------------
#
# dim_plot_data = t(rbind(
#   tabulate(apply(do.call(rbind, lapply(count2d0_result_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(count2d1_result_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(count2d5_result_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30))
# dim_plot_data = dim_plot_data[2:6,]
# colnames(dim_plot_data) = c("0.5", "1.5", "5")
# rownames(dim_plot_data) = c("p = 2", "p = 3", "p = 4", "p = 5", "p = 6")
#
# barplot(dim_plot_data, ylab="Proportion", xlab=expression(paste("True ", alpha)), legend.text = T,args.legend = list(x="bottomright"))
#
# barplot(cbind(dim_plot_data, NA), ylab="Proportion", xlab=expression(paste("True ", alpha)), legend.text = T,args.legend = list(x="topright", title="Posterior mode"))

# # Prop. of dim. conditioned on active dim. (not used) ---------------------
# plot(count2d0_result_adapt, d=2)
# plot(count2d1_result_adapt, d=2)
# plot(count2d5_result_adapt, d=2)

# Dim quantiles for table -------------------------------------------------

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(count2d0_result_adapt, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(count2d1_result_adapt, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(count2d5_result_adapt, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(count2d0_result_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(count2d1_result_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(count2d5_result_adapt, function(x) x$iter_d)), c(0.025,0.975))
)

# posterior proportion of dimension (combined 30 chains) ------------------

table(do.call(rbind, lapply(count2d0_result_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(count2d0_result_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(count2d1_result_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(count2d1_result_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(count2d5_result_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(count2d5_result_adapt, function(x) x$iter_d)))

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/disp_dim_bar.pdf", width = 10, height = 4)

par(mfrow=c(1,3))
par(mar = c(5.6, 4.5, 4.1, 0))
barplot(table(do.call(rbind, lapply(count2d0_result_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(count2d0_result_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="low overdispersion", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(count2d1_result_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(count2d1_result_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="moderate overdispersion", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)

par(mar = c(5.6, 4.5, 4.1, 0.25)) # right margin set as 0.1

barplot(table(do.call(rbind, lapply(count2d5_result_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(count2d5_result_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="high overdispersion", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)
dev.off()


# # Proportion across 30 networks for dim. prop. (not used) -----------------
#
# # open the pdf file
# pdf("~/lspm/inst/extdata/adapt/disp_dim_box.pdf", width = 10, height = 4)
#
# par(mfrow=c(1,3))
# layout.matrix <- matrix(1:3, nrow = 1, ncol = 3)
# layout(mat = layout.matrix, widths = c(2.6,2,2.2))
# par(mar = c(5.6, 6.5, 4.1, 0))
# boxplot(do.call(rbind,lapply(count2d0_result_adapt, function(x) tabulate(x$iter_d, 10)/450)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4)
# # title(main=expression(paste(p[0]," lower than truth")), line=2.3, cex.main=1.5)
# title(main="low overdispersion", line=1.3, cex.main=1.5)
#
# par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0
#
# boxplot(do.call(rbind,lapply(count2d1_result_adapt, function(x) tabulate(x$iter_d, 10)/450)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," same as truth")), line=2.3, cex.main=1.5)
# title(main="moderate overdispersion", line=1.3, cex.main=1.5)
#
#
# par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1
#
# boxplot(do.call(rbind,lapply(count2d5_result_adapt, function(x) tabulate(x$iter_d, 10)/450)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," higher than truth")), line=2.3, cex.main=1.5)
# title(main="high overdispersion", line=1.3, cex.main=1.5)
#
# dev.off()


# Procrustes Correlation --------------------------------------------------
library(vegan)
d=2
full_z_protest2d0 <- full_z_protest2d1 <- full_z_protest2d5 <- list()
for(seed in seed_number) {
d=2
  if(names(which.max(table(count2d0_result_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(count2d0_result_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), count2d0_network[[paste0("seed",(seed))]]$positions)
    full_z_protest2d0 <- append(full_z_protest2d0, list(temp=z_protest))
    names(full_z_protest2d0)[names(full_z_protest2d0)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(count2d1_result_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(count2d1_result_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), count2d1_network[[paste0("seed",(seed))]]$positions)
    full_z_protest2d1 <- append(full_z_protest2d1, list(temp=z_protest))
    names(full_z_protest2d1)[names(full_z_protest2d1)=="temp"] <- paste0("seed",(seed))
  }
d=3
  if(names(which.max(table(count2d5_result_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(count2d5_result_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), count2d5_network[[paste0("seed",(seed))]]$positions)
    full_z_protest2d5 <- append(full_z_protest2d5, list(temp=z_protest))
    names(full_z_protest2d5)[names(full_z_protest2d5)=="temp"] <- paste0("seed",(seed))
  }
}


# Proportion of posterior mode dimension equal truth
sum(do.call(c,lapply(count2d0_result_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(count2d0_result_adapt)
sum(do.call(c,lapply(count2d1_result_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(count2d1_result_adapt)
sum(do.call(c,lapply(count2d5_result_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(count2d5_result_adapt)


# Procrustes correlation median
round(rbind(
  median(do.call(rbind, lapply(full_z_protest2d0, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest2d1, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest2d5, function(x) x$scale)))
), 3)


# Procrustes correlation 95% credible interval
round(rbind(
  quantile((do.call(rbind, lapply(full_z_protest2d0, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest2d1, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest2d5, function(x) x$scale))), c(0.025, 0.975))
), 3)

# # Procrustes correlation plot (not used) ----------------------------------
# # open the pdf file
# cairo_pdf("~/lspm/inst/extdata/adapt/size_protest.pdf", width = 5, height = 4)
#
# tempprotest=cbind(-1,-1,-1)
# colnames(tempprotest) <- c("0.5", "1.5", "5")
# boxplot(tempprotest, xlab="n", ylab="Procrustes correlation", ylim=c(0,1), cex.lab=1.5, cex.axis=1.5)
# boxplot(do.call(rbind, lapply(full_z_protest2d0, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=1)
# boxplot(do.call(rbind, lapply(full_z_protest2d1, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=2)
# boxplot(do.call(rbind, lapply(full_z_protest2d5, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=3)
#
# # title(xlab="(b)", line=2)
#
# par(old.par) # restore previous plot layout setting
#
# # Close the pdf file
# dev.off()
