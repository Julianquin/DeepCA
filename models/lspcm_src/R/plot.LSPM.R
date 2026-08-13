#' Plotting method for LSPM
#'
#' Draw scatter plots or violin plots from the LSPM results based on the
#' parameters chosen.
#'
#' @param x A LSPM list containing the MCMC chains for the different parameters.
#' @param n_dimen The number of dimension to conditioned on the chain
#' @param parameter Specifying the type of parameter to output for the plot.
#' \describe{
#'  \item{\strong{dimensions}: }{Produces barplots of the proportion of
#'  posterior dimensions estimated under the LSPM.}
#'  \item{\strong{positions}: }{Produces scatterplots of the posterior mean
#'  latent positions estimated under the LSPM. Choosing dimensions more than 2
#'  will produce pair plots of the latent positions.}
#'  \item{\strong{deltas}: }{Produces violin plots of the posterior distribution
#'  of the shrinkage strength estimated under the LSPM.}
#'  \item{\strong{variances}: }{Produces violin plots of the posterior
#'  distribution of the variances estimated under the LSPM.}
#'  \item{\strong{alpha}: }{Produces violin plots of the posterior distribution
#'  of the alpha parameter estimated under the LSPM.}
#' }
#' @param ... Additional argument to be passed for graphical parameters.
#' @param true_values If simulated data is used, the true values of the
#' parameters can be passed on to be plotted with the estimated values.
#' @param transparency A number between 0 to 1 that specify the transparency
#' of the violin plot.
#'
#' @details Violin plots are posterior distributions from LSPM. Green dots are
#' the average posterior mean while red crosses are the true values if given in
#' the argument. The function is able to accept a list containing multiple
#' LSPM object and plot the different parameters (except for the latent
#' positions) with overlaying violin plots of certain transparency.
#'
#' @export
#'
#' @importFrom stats sd complete.cases na.omit
#' @importFrom grDevices rgb
#' @importFrom graphics plot title points pairs barplot arrows
#' @importFrom vioplot vioplot
#'
#' @examples
#' plot(lspm:::LSPM_sample, parameter = "dimensions") # dimensions
#' plot(lspm:::LSPM_sample, n_dimen = 2) # latent positions
#' plot(lspm:::LSPM_sample, parameter = "deltas", n_dimen = 2) # shrinkage strength
#' plot(lspm:::LSPM_sample, parameter = "variances", n_dimen = 2) # variances
#' plot(lspm:::LSPM_sample, parameter = "alpha", n_dimen = 2) # alpha
#'
#' @family functions
#'
plot.LSPM <- function(x, n_dimen,
                      parameter = c( "dimensions", "positions","deltas", "variances", "alpha"),
                      true_values, transparency = 0.1, ...) {
  LSPM_object <- x
  parameter <- match.arg(parameter)
  extra_arg <- list(...)

  switch(parameter,
         dimensions = ylab <- "Proportion",
         positions = ylab <- "",
         deltas = ylab <- expression(paste("Shrinkage strength")),
         variances = ylab <- expression(paste("Variance")),
         alpha = ylab <- expression(paste("", alpha)),
         stop("Invalid parameter")
  )

  switch(parameter,
         dimensions = xlab <- "Posterior active dimension",
         positions = xlab <- "",
         deltas = xlab <- "Dimension",
         variances = xlab <- "Dimension",
         alpha =  xlab <- "",
         stop("Invalid parameter")
  )

  if ("mcmc_chain" %in% names(LSPM_object) &
      "initialisation" %in% names(LSPM_object)) {
    if(parameter == "dimensions") {
      d_proportion = table(LSPM_object$iter_d)/length(LSPM_object$iter_d)

      if (!missing(true_values)) {
        d_col = rep(8, length(d_proportion))
        d_col_true = replace(d_col,(names(d_proportion) == true_values), 2)
        barplot(d_proportion, ylab = ylab, xlab=xlab, ylim = c(0,1), col = d_col_true)
        legend("topright", legend=true_values, lty=1, lwd=3,
               col=2, title="True dimension")
      } else {
        barplot(d_proportion, ylab = ylab, xlab=xlab, ylim = c(0,1))
      }

    } else if(parameter == "positions"){
      z_mean <- apply(LSPM_object$mcmc_chain[[paste0(n_dimen, "D")]][[parameter]], c(1,2), mean, na.rm=TRUE)

      if(n_dimen ==1){
        plot(z_mean[,1], y=rep(0,length(z_mean[,1])), xlab="Dimension 1",
             ylab="", yaxt="n",...)
      } else if(n_dimen ==2) {
        plot(z_mean[,1:2], xlab="Dimension 1", ylab="Dimension 2", ...)
      }
      else{
        pairs(z_mean[,1:n_dimen],
              labels=paste("Dim", 1:n_dimen), ...)
      }

    } else {
      if(parameter == "alpha"){
        vioplot(na.omit(LSPM_object[[parameter]][LSPM_object$iter_d==n_dimen]), rectCol=NA, border=NA, lineCol=NA,
                colMed="green", pchMed=20, cex=2, ...) }
      else {
        vioplot(na.omit(LSPM_object$mcmc_chain[[paste0(n_dimen, "D")]][[parameter]]), rectCol=NA, border=NA, lineCol=NA,
                colMed="green", pchMed=20, cex=2, ...)
      }
      title(ylab = ylab, line = 2.2)
      title(xlab = xlab, line = 2.2)
      if (!missing(true_values)) {
        points(true_values, col = "red", pch = 4, cex = 1.5, lwd = 4) # true
      }
    }

  } else if (class(LSPM_object) == "LSPM" | class(LSPM_object) != "LSPM_adapt") {
    max_d = max(do.call(c,lapply(LSPM_object, function(x) as.integer(names(table(x$iter_d))))))
    all_result <- lapply(LSPM_object, function(x) tabulate(x$iter_d, max_d)/length(x$iter_d))
    all_result_rbinded = do.call(rbind, all_result)
    colnames(all_result_rbinded) = 1:max_d
    all_result_rbinded_rm0 = all_result_rbinded[, colSums(all_result_rbinded != 0) > 0]
    d_posterior_mode = colnames(all_result_rbinded_rm0)[apply(all_result_rbinded_rm0,1,which.max)]
    d_posterior_mode_prop = table(d_posterior_mode)/length(LSPM_object)

    if(parameter=="dimensions") {

      if(!missing(n_dimen)){
        all_result_rbinded_rm0_d = all_result_rbinded_rm0[d_posterior_mode==n_dimen,, drop=F]
        all_result_rbinded_rm0_d = all_result_rbinded_rm0_d[, colSums(all_result_rbinded_rm0_d != 0) > 0, drop=F]
        d_proportion_mean = apply(all_result_rbinded_rm0_d,2,mean)
        d_proportion_sd = apply(all_result_rbinded_rm0_d,2,sd)

        plot_data <- d_proportion_mean
        xlab = "Posterior active dimension"
      } else {
        plot_data <- d_posterior_mode_prop
        xlab = "Posterior mode dimension"
      }

      if (!missing(true_values)) {
        d_col = rep(8, length(d_posterior_mode_prop))
        d_col_true = replace(d_col,(names(d_posterior_mode_prop) == true_values), 2)
        xbar = barplot(plot_data, ylab = ylab, xlab=xlab, ylim = c(0,1), col = d_col_true)

        legend("topright", legend=true_values, lty=1, lwd=3,
               col=2, title="True dimension")
      } else {
        xbar = barplot(plot_data, ylab = ylab, xlab=xlab, ylim = c(0,1))
      }

      if(!missing(n_dimen)){
        error.bar(xbar,plot_data, d_proportion_sd)
      }
      return(invisible())

    } else if(parameter=="positions") {
      return("Positions not supported for multi LSPM results")
    }

    LSPM_containing_d = do.call(rbind, lapply(LSPM_object, function(x) t(tabulate(x$iter_d, n_dimen))))[,n_dimen] != 0
    d_posterior_mode = d_posterior_mode[LSPM_containing_d]
    if(parameter=="alpha") {
      all_result <- lapply(LSPM_object[LSPM_containing_d], function(x) as.matrix(x[[parameter]][x$iter_d==n_dimen]))
    } else {
      all_result <- lapply(LSPM_object[LSPM_containing_d], function(x) as.matrix(x$mcmc_chain[[paste0(n_dimen, "D")]][[parameter]]))
    }

    # print(names(all_result)[lapply(all_result,length)<= 0]) # printing those that doesnt have the specified dimension
    all_result = all_result[lapply(all_result,length)>0][d_posterior_mode==n_dimen]
    all_result = lapply(all_result, function(x) x[complete.cases(x),,drop=F])
    vioplot(do.call(rbind, all_result),
            drawRect = FALSE, border = NA,
            col = rgb(red = 0, green = 0, blue = 0, alpha = 0), ...
    )
    title(ylab = ylab, line = 2.2)
    title(xlab = xlab, line = 2.2)

    for (i in 1:length(all_result)) {
      if (ncol(all_result[[i]]) == 1) {
        vioplot(all_result[[i]][, 1],
                add = TRUE, at = 1,
                drawRect = FALSE, border = NA,
                col = rgb(red = 0, green = 0, blue = 0, alpha = transparency)
        )
      } else if (ncol(all_result[[i]]) > 1) {
        for (dimen in 1:ncol(all_result[[i]])) {
          vioplot(all_result[[i]][, dimen],
                  add = TRUE, at = dimen,
                  drawRect = FALSE, border = NA,
                  col = rgb(red = 0, green = 0, blue = 0, alpha = transparency)
          )
        }
      }
    }
    if (!missing(true_values)) {
      points(true_values, col = "red", pch = 4, cex = 1.5, lwd = 4) # true
    }

    if (ncol(all_result[[1]]) == 1) {
      points(unlist(lapply(all_result, mean)), pch = 20, cex = 2, col = "green")
    } else if (ncol(all_result[[1]]) > 1) {
      points(apply(t(as.data.frame(lapply(all_result, function(x)
        apply(x, 2, mean)))), 2, mean), pch = 20, cex = 2, col = "green")
    }
  } else {
    return(print("Invalid object, please insert results from LSPM"))
  }
}
