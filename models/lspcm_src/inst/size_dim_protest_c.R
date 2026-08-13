load(file = "~/lspm/inst/extdata/adapt/node20results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node200results_adapt_c.Rdata")

source("~/lspm/inst/size_network_c.R")


# Dim quantiles for table -------------------------------------------------

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(node20results_adapt_c, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node50results_adapt_c, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results_adapt_c, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node200results_adapt_c, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(
  quantile(do.call(rbind, lapply(node20results_adapt_c, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node50results_adapt_c, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node100results_adapt_c, function(x) x$iter_d)), c(0.025,0.975)),
  quantile(do.call(rbind, lapply(node200results_adapt_c, function(x) x$iter_d)), c(0.025,0.975))
)

# posterior proportion of dimension (combined 30 chains) ------------------

table(do.call(rbind, lapply(node20results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node20results_adapt_c, function(x) x$iter_d)))
table(do.call(rbind, lapply(node50results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results_adapt_c, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results_adapt_c, function(x) x$iter_d)))
table(do.call(rbind, lapply(node200results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node200results_adapt_c, function(x) x$iter_d)))

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/size_dim_bar_c.pdf", width = 10, height = 4)

par(mfrow=c(1,4))
par(mar = c(5.6, 4.5, 4.1, 0))
barplot(table(do.call(rbind, lapply(node20results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node20results_adapt_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 20", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = auto)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node50results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node50results_adapt_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 50", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node100results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results_adapt_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 100", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)

par(mar = c(5.6, 4.5, 4.1, 0.25)) # right margin set as 0.1

barplot(table(do.call(rbind, lapply(node200results_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node200results_adapt_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main="n = 200", line=1.3, cex.main=1.5)
# title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)
dev.off()


# Procrustes Correlation --------------------------------------------------
library(vegan)
d=2
full_z_protest20_c <- full_z_protest50_c <- full_z_protest100_c <- full_z_protest200_c <- list()
for(seed in seed_number) {

  # if(d %in% names(table(node20results_adapt_c[[paste0("seed",(seed))]]$iter_d))) {
  if(names(which.max(table(node20results_adapt_c[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node20results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node20network_c[[paste0("seed",(seed))]]$positions)
    full_z_protest20_c <- append(full_z_protest20_c, list(temp=z_protest))
    names(full_z_protest20_c)[names(full_z_protest20_c)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(node50results_adapt_c[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node50results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node50network_c[[paste0("seed",(seed))]]$positions)
    full_z_protest50_c <- append(full_z_protest50_c, list(temp=z_protest))
    names(full_z_protest50_c)[names(full_z_protest50_c)=="temp"] <- paste0("seed",(seed))
  }

  if(names(which.max(table(node100results_adapt_c[[paste0("seed",(seed))]]$iter_d))) == d){
    z_protest <- protest(apply(node100results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network_c[[paste0("seed",(seed))]]$positions)
    full_z_protest100_c <- append(full_z_protest100_c, list(temp=z_protest))
    names(full_z_protest100_c)[names(full_z_protest100_c)=="temp"] <- paste0("seed",(seed))
  }

  if(seed %in% seed_number){
    if(names(which.max(table(node200results_adapt_c[[paste0("seed",(seed))]]$iter_d))) == d){
      z_protest <- protest(apply(node200results_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node200network_c[[paste0("seed",(seed))]]$positions)
      full_z_protest200_c <- append(full_z_protest200_c, list(temp=z_protest))
      names(full_z_protest200_c)[names(full_z_protest200_c)=="temp"] <- paste0("seed",(seed))
    }
  }
}


# Proportion of posterior mode dimension equal truth
sum(do.call(c,lapply(node20results_adapt_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node20results_adapt)
sum(do.call(c,lapply(node50results_adapt_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node50results_adapt_c)
sum(do.call(c,lapply(node100results_adapt_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results_adapt_c)
sum(do.call(c,lapply(node200results_adapt_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node200results_adapt_c)


# Procrustes correlation median
round(rbind(
  median(do.call(rbind, lapply(full_z_protest20_c, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest50_c, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest100_c, function(x) x$scale))),
  median(do.call(rbind, lapply(full_z_protest200_c, function(x) x$scale)))
), 3)


# Procrustes correlation 95% credible interval
round(rbind(
  quantile((do.call(rbind, lapply(full_z_protest20_c, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest50_c, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest100_c, function(x) x$scale))), c(0.025, 0.975)),
  quantile((do.call(rbind, lapply(full_z_protest200_c, function(x) x$scale))), c(0.025, 0.975))
), 3)

