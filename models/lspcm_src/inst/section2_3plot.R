library(expint)
# expected squared distance WITHIN dimension
expected_zpair_distance <-  function(a1,b1,a2,b2,c, l) {
  return(2*(b1/(a1-1))*((b2*gammainc(a2-1,c))/gammainc(a2,c))^(l-1))
}

# expected squared distance ACROSS dimension
expected_z_distance <-  function(a1,b1,a2,b2,c, pstar) {
  frac <- ((b2*gammainc(a2-1,c))/gammainc(a2,c))
  return(2*(b1/(a1-1))*((1-frac^pstar)/(1-frac)))
}

# within dimension --------------------------------------------------------
# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/dist2_3a.pdf", width = 5, height = 4)

custom_palette <- c("black", "red", "#984EA3", "#e49444", "#6a9f58", "#56B4E9")
palette(custom_palette)
par(mar=c(4.1, 4.6, 1.2, 1.1))
plot(NA, type='n',ylim=c(0,1.3), xlim=c(2,8), xlab="",ylab="Expected pairwise squared distance", cex.lab=1.25, cex.axis=1.2)
col=3
for(a2 in 1:4) {
  for(dim in 1:8) {
    points(x=dim, y=expected_zpair_distance(2,1,a2,1,1,dim), pch=a2+14, col=col, cex=2)
  }
  col=col+1
}
legend("topright", legend=seq(1,4,1), title=expression(a[2]),
       pch=14+1:4, col=3:7, cex=1.3)
title(xlab="Dimension \u2113", line=2.5, cex.lab=1.25)
dev.off()

# across dimension --------------------------------------------------------
# par(mar = c(5.6, 5.1, 4.1, 2.1))
# open the pdf file
cairo_pdf("~/lspm/inst/extdata/adapt/figure/dist2_3b.pdf", width = 5, height = 4)
par(mar=c(4.1, 5.6, 1.2, 1.1))
plot(NA, type='n', xlab="", ylab="", #log="y",
     col=1, ylim=c(2,7), xlim=c(0,8),  cex.lab=1.25, cex.axis=1.2)
col=3
for(pstar in c(2,3,5,20)) {
  lines(seq(0,12,.01),expected_z_distance(2,1,seq(0,12,.01),1,1,pstar), col=col, lwd=3)
  col=col+1
}
legend("topright", legend=c(2:3, 5,20), title="p",
       lty=1, col=3:7, lwd=3, cex=1.2)
title(ylab="Expected pairwise squared distance", line=3.7, cex.lab=1.25)
title(ylab="across all p dimensions", line=2.5, cex.lab=1.25)
title(xlab=expression(a[2]), line=2.5, cex.lab=1.25)
dev.off()
# par(mar=c(5.1, 4.1, 4.1, 2.1))


palette(c("black"  , "red"  ,   "green3"  ,"blue"   , "cyan"  ,  "magenta" ,"yellow"  ,"gray"  )) # default palette

