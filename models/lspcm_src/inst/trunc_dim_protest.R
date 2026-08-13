load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_low_d.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_same_d.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_high_d.Rdata")

source("~/lspm/inst/trunc_network.R")

# # Proportion posterior mode (not used) ------------------------------------
#
# dim_plot_data = t(rbind(
#   tabulate(apply(do.call(rbind, lapply(node100results4d3_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30))
# dim_plot_data = dim_plot_data[2:6,]
# colnames(dim_plot_data) = c("auto", "2", "4", "10")
# rownames(dim_plot_data) = c("p = 2", "p = 3", "p = 4", "p = 5", "p = 6")
#
# barplot(dim_plot_data, ylab="Proportion", xlab="Initial p", legend.text = T,args.legend = list(x="bottomright"))
#
# barplot(cbind(dim_plot_data, NA, NA), ylab="Proportion", xlab="Initial p", legend.text = T,args.legend = list(x="topright", title="Posterior mode"))
#
#
#
#
# # Prop. of dim. conditioned on active dim. (not used) ---------------------
#
# #A function to add arrows on the chart
# error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
#   arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
# }
#
# plot(node100results4d3_adapt, d=4)
# plot(node100results4d3_adapt_low_d, d=4)
# plot(node100results4d3_adapt_same_d, d=4)
# plot(node100results4d3_adapt_high_d, d=4)



# Dim quantiles for table -------------------------------------------------

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(node100results4d3_adapt, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(quantile(do.call(rbind, lapply(node100results4d3_adapt, function(x) x$iter_d)), c(0.025,0.975)),
quantile(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(x) x$iter_d)), c(0.025,0.975)),
quantile(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(x) x$iter_d)), c(0.025,0.975)),
quantile(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(x) x$iter_d)), c(0.025,0.975))
)


# posterior proportion of dimension (combined 30 chains) ------------------

table(do.call(rbind, lapply(node100results4d3_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(x) x$iter_d)))

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/trunc_dim_bar.pdf", width = 10, height = 2.5)

par(mfrow=c(1,4))
par(mar = c(4.6, 4.5, 3.6, 0))
barplot(table(do.call(rbind, lapply(node100results4d3_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(p[0]," automatically chosen")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = auto)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_low_d, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(p[0]," lower than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(x) x$iter_d)))[1:6]/ length(do.call(rbind, lapply(node100results4d3_adapt_same_d, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(p[0]," same as the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)

par(mar = c(4.6, 4.5, 3.6, 0.25)) # right margin set as 0.1

barplot(table(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(x) x$iter_d)))[1:7]/ length(do.call(rbind, lapply(node100results4d3_adapt_high_d, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

title(main=expression(paste(p[0]," higher than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)
dev.off()



# # Proportion across 30 networks for dim. prop. (not used) -----------------
#
# # open the pdf file
# pdf("~/lspm/inst/extdata/adapt/trunc_dim_box.pdf", width = 12, height = 4)
#
# par(mfrow=c(1,4))
# layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)
# layout(mat = layout.matrix, widths = c(2.6,1.8,1.8,1.9))
# par(mar = c(5.6, 6.5, 4.1, 0))
#
# boxplot(do.call(rbind,lapply(node100results4d3_adapt, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4)
# title(main=expression(paste(p[0]," automatically chosen")), line=2.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = auto)")), line=1, cex.main=1.5)
#
# par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0
#
# boxplot(do.call(rbind,lapply(node100results4d3_adapt_low_d, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# title(main=expression(paste(p[0]," lower than truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)
#
# boxplot(do.call(rbind,lapply(node100results4d3_adapt_same_d, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# title(main=expression(paste(p[0]," same as truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)
#
#
# par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1
#
# boxplot(do.call(rbind,lapply(node100results4d3_adapt_high_d, function(x) tabulate(x$iter_d, 10)/600)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# title(main=expression(paste(p[0]," higher than truth")), line=2.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)
#
# dev.off()



# Procrustes Correlation --------------------------------------------------
library(vegan)
d=4

# Posterior predictive checking conditioned on the posterior mode equal to d
full_z_protest_adapt <- full_z_protest_adapt_low_d <- full_z_protest_adapt_same_d <- full_z_protest_adapt_high_d <- list()
for(seed in seed_number) {
d=4
  if(names(which.max(table(node100results4d3_adapt[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d[[paste0("seed",(seed))]]$positions)
    full_z_protest_adapt <- append(full_z_protest_adapt, list(temp=z_protest))
    names(full_z_protest_adapt)[names(full_z_protest_adapt)=="temp"] <- paste0("seed",(seed))
  }
d=3
  if(names(which.max(table(node100results4d3_adapt_low_d[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt_low_d[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d[[paste0("seed",(seed))]]$positions[,1:3])
    full_z_protest_adapt_low_d <- append(full_z_protest_adapt_low_d, list(temp=z_protest))
    names(full_z_protest_adapt_low_d)[names(full_z_protest_adapt_low_d)=="temp"] <- paste0("seed",(seed))
  }
d=4
  if(names(which.max(table(node100results4d3_adapt_same_d[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt_same_d[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d[[paste0("seed",(seed))]]$positions)
    full_z_protest_adapt_same_d <- append(full_z_protest_adapt_same_d, list(temp=z_protest))
    names(full_z_protest_adapt_same_d)[names(full_z_protest_adapt_same_d)=="temp"] <- paste0("seed",(seed))
  }
d=4
  if(names(which.max(table(node100results4d3_adapt_high_d[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt_high_d[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d[[paste0("seed",(seed))]]$positions)
    full_z_protest_adapt_high_d <- append(full_z_protest_adapt_high_d, list(temp=z_protest))
    names(full_z_protest_adapt_high_d)[names(full_z_protest_adapt_high_d)=="temp"] <- paste0("seed",(seed))
  }
}

# Proportion of posterior mode dimension equal truth
sum(do.call(c,lapply(node100results4d3_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt)
sum(do.call(c,lapply(node100results4d3_adapt_low_d, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt_low_d)
sum(do.call(c,lapply(node100results4d3_adapt_same_d, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt_same_d)
sum(do.call(c,lapply(node100results4d3_adapt_high_d, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt_high_d)


# Procrustes correlation median
round(rbind(median(do.call(rbind, lapply(full_z_protest_adapt, function(x) x$scale))),
median(do.call(rbind, lapply(full_z_protest_adapt_low_d, function(x) x$scale))),
median(do.call(rbind, lapply(full_z_protest_adapt_same_d, function(x) x$scale))),
median(do.call(rbind, lapply(full_z_protest_adapt_high_d, function(x) x$scale)))
),3)

# Procrustes correlation 95% credible interval
round(rbind(quantile((do.call(rbind, lapply(full_z_protest_adapt, function(x) x$scale))), c(0.025, 0.975)),
quantile((do.call(rbind, lapply(full_z_protest_adapt_low_d, function(x) x$scale))), c(0.025, 0.975)),
quantile((do.call(rbind, lapply(full_z_protest_adapt_same_d, function(x) x$scale))), c(0.025, 0.975)),
quantile((do.call(rbind, lapply(full_z_protest_adapt_high_d, function(x) x$scale))), c(0.025, 0.975))
),3)


# # Procrustes correlation plot (not used) ----------------------------------
#
# # open the pdf file
# cairo_pdf("~/lspm/inst/extdata/adapt/trunc_protest.pdf", width = 5, height = 4)
#
# tempprotest=cbind(-1,-1,-1,-1)
# colnames(tempprotest) <- c("auto", "2", "4", "10")
# boxplot(tempprotest, xlab=expression(p[0]), ylab="Procrustes correlation", ylim=c(0,1), cex.lab=1.5, cex.axis=1.5)
# boxplot(do.call(rbind, lapply(full_z_protest_adapt, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=1)
# boxplot(do.call(rbind, lapply(full_z_protest_adapt_low_d, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=2)
# boxplot(do.call(rbind, lapply(full_z_protest_adapt_same_d, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=3)
# boxplot(do.call(rbind, lapply(full_z_protest_adapt_high_d, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=4)
#
# # title(xlab="(b)", line=2)
#
# par(old.par) # restore previous plot layout setting
#
# # Close the pdf file
# dev.off()




