#' Diagnostic plots of LSPM
#'
#' Produces trace plots and autocorrelation plots for the parameters of an LSPM
#' object.
#'
#' @param LSPM_object results from running LSPM
#' @param n_dimen The number of dimension to conditioned on the chain
#' @param what Specifying the quantity to be plotted
#' @param backward Used for pre-adaptive LSPM results
#' and autocorrelation plot
#' \describe{
#'  \item{\strong{alpha} or \strong{likelihood}: }{Produces trace plots and
#'  autocorrelation plots for both the posterior alpha samples and posterior
#'  loglikelihood values. The loglikelihood plotted is based on after the
#'  acceptance of the latent positions in the MCMC chain. These parameters used
#'  Metropolis-Hasting algorithm and thus have an acceptance ratio given.}
#'  \item{\strong{deltas}: }{Produces trace plots and autocorrelation plots for
#'  the posterior shrinkage strengths samples. These parameters used Gibbs
#'  Sampling and thus are accepted in every sampling iterations.}
#' }
#'
#' @export
#'
#' @importFrom stats acf
#' @importFrom graphics plot par lines legend
#' @importFrom vioplot vioplot
#'
#' @family functions
#'
#' @examples diagLSPM(lspm:::LSPM_sample, n_dimen = 2)
diagLSPM <- function(LSPM_object, n_dimen, what = c("alpha", "likelihood",
                                                 "deltas"), backward=FALSE) {
  available_d = unique(LSPM_object$iter_d)
  if(!(n_dimen %in% available_d)){
    cat("Available dimensions are", paste(available_d), "\n")
    return(invisible())
  }
  what <- match.arg(what)
  aca <- c(LSPM_object$aca)
  acz <- c(LSPM_object$acz)
  iter <- c(LSPM_object$initialisation$iter)
  burnin <- c(LSPM_object$initialisation$burnin)
  thin <- c(LSPM_object$initialisation$thin)
  iter_thin = ceiling((iter - burnin) / thin)

  if(backward) {
    acz_prop = round(acz/iter, 4)
    aca_prop = round(aca/iter, 4)
  } else if(backward==FALSE) {
    acz_prop = round(sum(acz)/iter_thin,4)
    aca_prop = round(sum(aca)/iter_thin,4)
  }

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))


  if(what == "alpha" || what == "likelihood") {
    par(mfrow=c(2,2))
    plot(LSPM_object$alpha[LSPM_object$iter_d==n_dimen], type='l',
         ylab="Posterior Alpha Samples", xlab = "Iteration",
         ylim=c(min(LSPM_object$alpha[LSPM_object$iter_d==n_dimen]), max(LSPM_object$alpha[LSPM_object$iter_d==n_dimen])))

    plot(LSPM_object$like[LSPM_object$iter_d==n_dimen], type='l',
         ylab="Posterior loglikelihood Samples", xlab = "Iteration")

    acf(LSPM_object$alpha[LSPM_object$iter_d==n_dimen], main="Alpha ACF")
    legend("topright", legend=aca_prop, lty=1,
           cex=0.7, title="Acceptance ratio")
    acf(LSPM_object$like[LSPM_object$iter_d==n_dimen], main="Likelihood ACF")
    legend("topright", legend=acz_prop, lty=1,
           cex=0.7, title="Acceptance ratio")

  } else if(what == "deltas") {
    par(mfcol=c(2,2))
    for(i in 1:n_dimen) {
      plot(na.omit(LSPM_object$mcmc_chain[[paste0(n_dimen,"D")]]$deltas[,i]), type="l",
           ylab="Posterior Shrinkage Strength Samples", xlab="Iteration")

      acf(na.omit(LSPM_object$mcmc_chain[[paste0(n_dimen,"D")]]$deltas[,i]), main=paste("Delta", i, "ACF"))
    }
  } else {
    print("Invalid 'what' selected")
  }

}
