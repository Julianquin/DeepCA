load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_low_d_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_same_d_c.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results4d3_adapt_high_d_c.Rdata")

source("~/lspm/inst/trunc_network_c.R")

# Dim quantiles for table -------------------------------------------------

# Dimension mode
rbind(calculate_mode(do.call(rbind, lapply(node100results4d3_adapt_c, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results4d3_adapt_low_d_c, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results4d3_adapt_same_d_c, function(x) x$iter_d))),
      calculate_mode(do.call(rbind, lapply(node100results4d3_adapt_high_d_c, function(x) x$iter_d)))
)

# Dimension 95% credible interval
rbind(quantile(do.call(rbind, lapply(node100results4d3_adapt_c, function(x) x$iter_d)), c(0.025,0.975)),
      quantile(do.call(rbind, lapply(node100results4d3_adapt_low_d_c, function(x) x$iter_d)), c(0.025,0.975)),
      quantile(do.call(rbind, lapply(node100results4d3_adapt_same_d_c, function(x) x$iter_d)), c(0.025,0.975)),
      quantile(do.call(rbind, lapply(node100results4d3_adapt_high_d_c, function(x) x$iter_d)), c(0.025,0.975))
)


# posterior proportion of dimension (combined 30 chains) ------------------

table(do.call(rbind, lapply(node100results4d3_adapt_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_c, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results4d3_adapt_low_d_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_low_d_c, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results4d3_adapt_same_d_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_same_d_c, function(x) x$iter_d)))
table(do.call(rbind, lapply(node100results4d3_adapt_high_d_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_high_d_c, function(x) x$iter_d)))

# open the pdf file
pdf("~/lspm/inst/extdata/adapt/figure/trunc_dim_bar_c.pdf", width = 10, height = 2.5)

par(mfrow=c(1,4))
par(mar = c(4.6, 4.5, 3.6, 0))
barplot(table(do.call(rbind, lapply(node100results4d3_adapt_c, function(x) x$iter_d)))[1:6]/ length(do.call(rbind, lapply(node100results4d3_adapt_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(p[0]," automatically chosen")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = auto)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node100results4d3_adapt_low_d_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_low_d_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(p[0]," lower than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 2)")), line=1, cex.main=1.5)

barplot(table(do.call(rbind, lapply(node100results4d3_adapt_same_d_c, function(x) x$iter_d)))[1:6]/ length(do.call(rbind, lapply(node100results4d3_adapt_same_d_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)
title(main=expression(paste(p[0]," same as the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 4)")), line=1, cex.main=1.5)

par(mar = c(4.6, 4.5, 3.6, 0.25)) # right margin set as 0.1

barplot(table(do.call(rbind, lapply(node100results4d3_adapt_high_d_c, function(x) x$iter_d)))/ length(do.call(rbind, lapply(node100results4d3_adapt_high_d_c, function(x) x$iter_d)))
        , ylim=c(0,1), xlab = expression(p), ylab="Posterior proportion", cex.lab=1.5, cex.axis=1.5, cex.names=1.5)

title(main=expression(paste(p[0]," higher than the truth")), line=2.3, cex.main=1.5)
title(main=expression(paste("(", p[0], " = 10)")), line=1, cex.main=1.5)
dev.off()


# Procrustes Correlation --------------------------------------------------
library(vegan)
d=4

# Posterior predictive checking conditioned on the posterior mode equal to d
full_z_protest_adapt_c <- full_z_protest_adapt_low_d_c <- full_z_protest_adapt_same_d_c <- full_z_protest_adapt_high_d_c <- list()
for(seed in seed_number) {
d=4
  if(names(which.max(table(node100results4d3_adapt_c[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d_c[[paste0("seed",(seed))]]$positions)
    full_z_protest_adapt_c <- append(full_z_protest_adapt_c, list(temp=z_protest))
    names(full_z_protest_adapt_c)[names(full_z_protest_adapt_c)=="temp"] <- paste0("seed",(seed))
  }
d=3
  if(names(which.max(table(node100results4d3_adapt_low_d_c[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt_low_d_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d_c[[paste0("seed",(seed))]]$positions[,1:3])
    full_z_protest_adapt_low_d_c <- append(full_z_protest_adapt_low_d_c, list(temp=z_protest))
    names(full_z_protest_adapt_low_d_c)[names(full_z_protest_adapt_low_d_c)=="temp"] <- paste0("seed",(seed))
  }
d=4
  if(names(which.max(table(node100results4d3_adapt_same_d_c[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt_same_d_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d_c[[paste0("seed",(seed))]]$positions)
    full_z_protest_adapt_same_d_c <- append(full_z_protest_adapt_same_d_c, list(temp=z_protest))
    names(full_z_protest_adapt_same_d_c)[names(full_z_protest_adapt_same_d_c)=="temp"] <- paste0("seed",(seed))
  }
d=4
  if(names(which.max(table(node100results4d3_adapt_high_d_c[[paste0("seed",(seed))]]$iter_d))) == d) {
    z_protest <- protest(apply(node100results4d3_adapt_high_d_c[[paste0("seed",(seed))]]$mcmc_chain[[paste0(d, "D")]]$positions, c(1,2), mean, na.rm=T), node100network4d_c[[paste0("seed",(seed))]]$positions)
    full_z_protest_adapt_high_d_c <- append(full_z_protest_adapt_high_d_c, list(temp=z_protest))
    names(full_z_protest_adapt_high_d_c)[names(full_z_protest_adapt_high_d_c)=="temp"] <- paste0("seed",(seed))
  }
}

# Proportion of posterior mode dimension equal truth
sum(do.call(c,lapply(node100results4d3_adapt_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt_c)
sum(do.call(c,lapply(node100results4d3_adapt_low_d_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt_low_d_c)
sum(do.call(c,lapply(node100results4d3_adapt_same_d_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt_same_d_c)
sum(do.call(c,lapply(node100results4d3_adapt_high_d_c, function(x) names(which.max(table(x$iter_d))) == d))) / length(node100results4d3_adapt_high_d_c)


# Procrustes correlation median
round(rbind(median(do.call(rbind, lapply(full_z_protest_adapt_c, function(x) x$scale))),
            median(do.call(rbind, lapply(full_z_protest_adapt_low_d_c, function(x) x$scale))),
            median(do.call(rbind, lapply(full_z_protest_adapt_same_d_c, function(x) x$scale))),
            median(do.call(rbind, lapply(full_z_protest_adapt_high_d_c, function(x) x$scale)))
),3)

# Procrustes correlation 95% credible interval
round(rbind(quantile((do.call(rbind, lapply(full_z_protest_adapt_c, function(x) x$scale))), c(0.025, 0.975)),
            quantile((do.call(rbind, lapply(full_z_protest_adapt_low_d_c, function(x) x$scale))), c(0.025, 0.975)),
            quantile((do.call(rbind, lapply(full_z_protest_adapt_same_d_c, function(x) x$scale))), c(0.025, 0.975)),
            quantile((do.call(rbind, lapply(full_z_protest_adapt_high_d_c, function(x) x$scale))), c(0.025, 0.975))
),3)

