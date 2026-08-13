load(file = "~/lspm/inst/extdata/adapt/node20results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node50results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node100results_adapt.Rdata")
load(file = "~/lspm/inst/extdata/adapt/node200results_adapt.Rdata")

source("~/lspm/inst/size_network.R")

# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/size_alpha.pdf", width = 5, height = 4)

# alpha comparison --------------------------------------------------------

old.par <- par(no.readonly = TRUE) # save current layout setting
par(mfrow=c(1,1))
par(mar = c(4.6, 4.5, 1.5, 0.5))

# cbf_2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# palette(cbf_2)
# custom_palette <- c("black", "red", "#f1a2a9", "#e49444", "#6a9f58", "#56B4E9")
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
palette(custom_palette)
mycol=adjustcolor(palette(), alpha.f = 0.2)
palette(mycol)

# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

# d = 4
alpha_thin_mean <- c()
# tempalpha <- cbind(alphaaa2,c(result4d4_100_thin_alpha_mean,rep(NA,13)),alphaaa)
tempalpha=cbind(0,0,0,0,0)
# colnames(tempalpha) <- c("Auto", "2", "4", "10", "")
boxplot(tempalpha, xlab=expression(n) ,ylab=expression(paste("", alpha)), ylim=c(0.5,11), boxwex=0.4, xaxt="n")
axis(1, at=1:4, las=1, labels=c("20", "50", "100", "200"), cex.axis=1)

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node20results_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node20results_adapt[[paste0("seed",(seed))]]$alpha[node20results_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=1,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node50results_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node50results_adapt[[paste0("seed",(seed))]]$alpha[node50results_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=2,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node100results_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node100results_adapt[[paste0("seed",(seed))]]$alpha[node100results_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=3,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}

for(d in 2:4){
  for(seed in seed_number) {
    if(names(which.max(table(node200results_adapt[[paste0("seed",(seed))]]$iter_d))) != d){
      next
    } else{
      alpha_data = node200results_adapt[[paste0("seed",(seed))]]$alpha[node200results_adapt[[paste0("seed",(seed))]]$iter_d == d]
      vioplot(alpha_data, at=4,
              add=TRUE, cex=0.2, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, col=d+1)
    }
  }
}


abline(h=3, col='red', lwd=3) # true
# points(apply(alpha_thin_mean,2,mean),pch=20, cex=1.5, col="green")
# legend("topleft", legend=c('Estimated value', "True value", 'Thinned chain'), lty=c(1,1,1), col=c("orange","red", rgb(red = 1, green=0.5, blue=1)), lwd=3)
# title(xlab="(a)", line=2)
# palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette
# palette(cbf_2)
# custom_palette <- c("black", "red", "#f1a2a9", "#e49444", "#6a9f58", "#56B4E9")
custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
palette(custom_palette)
legend("topright", legend=c("True", expression(paste(p, " = 2")), expression(paste(p, " = 3")), expression(paste(p, " = 4"))),
       pch=c(NA, rep(20,4)), lty=c(1,rep(NA,4)), col=c("red",3:5), cex=1)

# Close the pdf file
dev.off()

