load(file = "~/lspm/inst/extdata/adapt/node20results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node200results_adapt.Rdata")

source("~/lspm/inst/size_network.R")

# # Proportion posterior mode (not used) ------------------------------------
#
# dim_plot_data = t(rbind(
#   tabulate(apply(do.call(rbind, lapply(node20results_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(node50results_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(node100results_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30,
#   tabulate(apply(do.call(rbind, lapply(node200results_adapt, function(x) tabulate(x$iter_d, 10))), 1, which.max), 6)/30))
# dim_plot_data = dim_plot_data[2:6,]
# colnames(dim_plot_data) = c("auto", "2", "4", "10")
# rownames(dim_plot_data) = c("p = 2", "p = 3", "p = 4", "p = 5", "p = 6")
#
# barplot(dim_plot_data, ylab="Proportion", xlab="Initial p", legend.text = T,args.legend = list(x="bottomright"))
#
# barplot(cbind(dim_plot_data, NA, NA), ylab="Proportion", xlab="Initial p", legend.text = T,args.legend = list(x="topright", title="Posterior mode"))

# # Prop. of dim. conditioned on active dim. (not used) ---------------------
# plot(node20results_adapt, d=2)
# plot(node50results_adapt, d=2)
# plot(node100results_adapt, d=2)
# plot(node200results_adapt, d=2)

# Dim quantiles for table -------------------------------------------------

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(node20results_adapt, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node50results_adapt, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results_adapt, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node200results_adapt, function(x) x$iter_d)))
      )

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(node20results_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node50results_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node100results_adapt, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node200results_adapt, function(x) x$iter_d)), c(0.025,0.975))
)

# posterior proportion of dimension (combined 30 chains) ------------------

table(do.call(rbind, lapply(node20results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node20results_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node50results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results_adapt, function(x) x$iter_d)))
table(do.call(rbind, lapply(node200results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node200results_adapt, function(x) x$iter_d)))

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/size_dim_bar.pdf", width = 10, height = 4)

par(mfrow=c(1,4))
par(mar = c(5.6, 4.5, 4.1, 0))
barplot(table(do.call(rbind, lapply(node20results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node20results_adapt, function(x) x$iter_d)))
, ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 20", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = auto)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node50results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 50", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node100results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 100", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)

par(mar = c(5.6, 4.5, 4.1, 0.25)) # right margin set as 0.1

barplot(table(do.call(rbind, lapply(node200results_adapt, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node200results_adapt, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 200", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)
dev.off()


# # Proportion across 30 networks for dim. prop. (not used) -----------------
#
# # open the pdf file
# pdf("~/lspm/inst/extdata/adapt/size_dim_box.pdf", width = 12, height = 4)
#
# par(mfrow=c(1,4))
# layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)
# layout(mat = layout.matrix, widths = c(2.6,1.8,1.8,1.9))
# par(mar = c(5.6, 6.5, 4.1, 0))
#
# boxplot(do.call(rbind,lapply(node20results_adapt, function(x) tabulate(x$iter_d, 10)/300)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4)
# # title(main=expression(paste(p[0]," automatically chosen")), line=2.3, cex.main=1.5)
# title(main="n = 20", line=1.3, cex.main=1.5)
#
# par(mar = c(5.6, 0, 4.1, 0)) # left margin set as 0
#
# boxplot(do.call(rbind,lapply(node50results_adapt, function(x) tabulate(x$iter_d, 10)/300)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," lower than truth")), line=2.3, cex.main=1.5)
# title(main="n = 50", line=1.3, cex.main=1.5)
#
# boxplot(do.call(rbind,lapply(node100results_adapt, function(x) tabulate(x$iter_d, 10)/300)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," same as truth")), line=2.3, cex.main=1.5)
# title(main="n = 100", line=1.3, cex.main=1.5)
#
#
# par(mar = c(5.6, 0, 4.1, 0.25)) # right margin set as 0.1
#
# boxplot(do.call(rbind,lapply(node200results_adapt, function(x) tabulate(x$iter_d, 10)/300)),
#         ylim=c(0,1), xlab = expression(p), ylab="Proportion across networks of\n posterior proportion", cex.lab=1.5, cex.axis=1.4, yaxt="n")
# # title(main=expression(paste(p[0]," higher than truth")), line=2.3, cex.main=1.5)
# title(main="n = 200", line=1.3, cex.main=1.5)
#
# dev.off()


# Procrustes Correlation --------------------------------------------------
library(vegan)
d=2
full_z_protest20 <- full_z_protest50 <- full_z_protest100 <- full_z_protest200 <- list()
for(seed in seed_number) {

  # if(d %in% names(table(node20results_adapt[[paste0("seed",(seed))]]$iter_d))) {
  if(names(which.max(table(node20results_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
  z_protest <- protest(apply(node20results_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node20network[[paste0("seed",(seed))]]$positions)
  full_z_protest20 <- append(full_z_protest20, list(temp=z_protest))
  names(full_z_protest20)[names(full_z_protest20)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(node50results_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
  z_protest <- protest(apply(node50results_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network[[paste0("seed",(seed))]]$positions)
  full_z_protest50 <- append(full_z_protest50, list(temp=z_protest))
  names(full_z_protest50)[names(full_z_protest50)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(node100results_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
  z_protest <- protest(apply(node100results_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network[[paste0("seed",(seed))]]$positions)
  full_z_protest100 <- append(full_z_protest100, list(temp=z_protest))
  names(full_z_protest100)[names(full_z_protest100)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(node200results_adapt[[paste0("seed",(seed))]]$iter_d))) == d){
  z_protest <- protest(apply(node200results_adapt[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node200network[[paste0("seed",(seed))]]$positions)
  full_z_protest200 <- append(full_z_protest200, list(temp=z_protest))
  names(full_z_protest200)[names(full_z_protest200)=="temp"] <- paste0("seed",(seed))
  }
}


# Proportion of posterior mode dimension equal truth
sum(do.call(c,lapply(node20results_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node20results_adapt)
sum(do.call(c,lapply(node50results_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results_adapt)
sum(do.call(c,lapply(node100results_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results_adapt)
sum(do.call(c,lapply(node200results_adapt, function(x) names(which.max(table(x$iter_d))) == d))) / length(node200results_adapt)


# Procrustes correlation median
round(rbind(
  median(do.call(rbind, lapply(full_z_protest20, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest50, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest100, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest200, function(x) x$scale)))
), 3)


# Procrustes correlation 95% credible interval
round(rbind(
  quantile((do.call(rbind, lapply(full_z_protest20, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest50, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest100, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest200, function(x) x$scale))), c(0.025, 0.975))
), 3)

# # Procrustes correlation plot (not used) ----------------------------------
# # open the pdf file
# cairo_pdf("~/lspm/inst/extdata/adapt/size_protest.pdf", width = 5, height = 4)
#
# tempprotest=cbind(-1,-1,-1,-1)
# colnames(tempprotest) <- c("20", "50", "100", "200")
# boxplot(tempprotest, xlab="n", ylab="Procrustes correlation", ylim=c(0,1), cex.lab=1.5, cex.axis=1.5)
# boxplot(do.call(rbind, lapply(full_z_protest20, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=1)
# boxplot(do.call(rbind, lapply(full_z_protest50, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=2)
# boxplot(do.call(rbind, lapply(full_z_protest100, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=3)
# boxplot(do.call(rbind, lapply(full_z_protest200, function(x) x$scale)), xlab="n", ylab="n", ylim=c(0,1), yaxt="n", add=T, at=4)
#
# # title(xlab="(b)", line=2)
#
# par(old.par) # restore previous plot layout setting
#
# # Close the pdf file
# dev.off()
