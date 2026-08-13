loglike <- function(y, alpha, dist_btwn_z, family) {
  # log likelihood calculation

  eta <- alpha-dist_btwn_z

  if(family == "logit") {
    # 2nd and 3rd term code here is faster breakdown calculation for 2nd term in paper
    sum(as.matrix(eta)*y) - sum(log(1+exp(eta)))*2 # - (dim(y)[1] * log(2)) # 2nd term multiple 2 for upper+lower triangle, last term calculates the diagonal contribution
  } else if(family == "poisson") {
    sum(as.matrix(eta)*y) - sum(exp(eta))*2 - # dim(y)[1] -
      sum(lfactorial(y))# 2nd term multiple 2 for upper+lower triangle, 3rd term calculates the diagonal contribution
  }
}

# A function to add arrows on the chart
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# A function to calculate the mode
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


dst=function(Y,infd=F) {
  # node distance calculation ## currently not used

  g=dim(Y)[1] ;  Dst=Yr=Y;  Dst=Y*(Y==1) + g*(Y==0)
  for(r in 2:(g-1)) { Yr=Yr%*%Y ; Dst=Dst+(r-g)*( Yr>0 & Dst==g ) }
  if(infd==T){for(i in 1:g){ for(j in 1:g) { if( Dst[i,j]==g ){ Dst[i,j]=Inf} }}}
  diag(Dst)=0
  Dst  }

proc.crr=function(Z0,Z,k=2){
  #Procrustes transform; gives rotation,reflection,translation
  #of Z closest to Z0

  for( i in 1:k) { Z[,i]=Z[,i]-mean(Z[,i]) + mean(Z0[,i]) } #translation

  A=t(Z)%*%(  Z0%*%t(Z0)  )%*%Z
  eA=eigen(A,symmetric=T)
  Ahalf=eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]))%*%t(eA$vec[,1:k])

  t(t(Z0)%*%Z%*%solve(Ahalf)%*%t(Z)) }

#' @importFrom stats dnorm
latent_priors <- function(latent_pos, omegas) {
  # Calculation for latent priors

  value <- c()
  for(l in 1:dim(latent_pos)[2]) {
    value <- sum(value, dnorm(latent_pos[,l], mean = 0, sd = 1/sqrt((omegas[l])), log=TRUE))
  }
  return(value)
}

latent_hastings <- function(positions_from, positions_to, sd) {
  value <- c()
  for(l in 1:dim(positions_from)[2]) {
    value <- sum(value, dnorm(positions_to[,l], mean = positions_from[,l], sd = sd[l], log=TRUE))
  }
  return(value)
}

#' @importFrom sna gvectorize stackcount
#' @importFrom stats glm.fit poisson
netpois <- function (y, x, intercept = TRUE, mode = "digraph", diag = FALSE)
{
  gfit <- function(glist, mode, diag) {
    y <- gvectorize(glist[[1]], mode = mode, diag = diag,
                          censor.as.na = TRUE)
    x <- vector()
    for (i in 2:length(glist)) x <- cbind(x, gvectorize(glist[[i]],
                                                              mode = mode, diag = diag, censor.as.na = TRUE))
    if (!is.matrix(x))
      x <- matrix(x, ncol = 1)
    mis <- is.na(y) | apply(is.na(x), 1, any)
    glm.fit(x[!mis, ], y[!mis], family = poisson(), intercept = FALSE)
  }
  if (is.list(y) || ((length(dim(y)) > 2) && (dim(y)[1] >
                                              1)))
    stop("y must be a single graph in netlogit.")
  if (length(dim(y)) > 2)
    y <- y[1, , ]
  if (is.list(x) || (dim(x)[2] != dim(y)[2]))
    stop("Homogeneous graph orders required in netlogit.")
  nx <- stackcount(x) + intercept
  n <- dim(y)[2]
  g <- list(y)
  if (intercept)
    g[[2]] <- matrix(1, n, n)
  if (nx - intercept == 1)
    g[[2 + intercept]] <- x
  else for (i in 1:(nx - intercept)) g[[i + 1 + intercept]] <- x[i,
                                                                 , ]
  if (any(sapply(lapply(g, is.na), any)))
    warning("Missing data supplied to netlogit; this may pose problems for certain null hypotheses.  Hope you know what you're doing....")
  fit.base <- gfit(g, mode = mode, diag = diag)
  fit <- list()
  fit.base$coefficients
}

