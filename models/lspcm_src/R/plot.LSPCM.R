#' Plotting method for LSPCM
#'
#' Draw barplots, scatter plots or violin plots from the LSPCM results based
#' on the parameters chosen.
#'
#' @param x A LSPCM list containing the MCMC chains for the different parameters.
#' @param n_dimen The number of dimensions to conditioned on the chain
#' @param parameter Specifying the type of parameter to output for the plot.
#' \describe{
#' \item{\strong{clusters}: }{Produces barplots of the proportion of
#'  posterior clusters estimated under the LSPM.}
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
#' @param conditional A TRUE/FALSE statement indicating to return the results
#' containing only on the parameters given
#'
#' @details Violin plots are posterior distributions from LSPCM. Green dots are
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
#' plot(lspm:::LSPCM_sample, parameter = "clusters") # clusters
#' plot(lspm:::LSPCM_sample, parameter = "dimensions") # dimensions
#' plot(lspm:::LSPCM_sample, parameter = "positions", n_dimen = 2) # latent positions
#' plot(lspm:::LSPCM_sample, parameter = "deltas", n_dimen = 2) # shrinkage strength
#' plot(lspm:::LSPCM_sample, parameter = "variances", n_dimen = 2) # variances
#' plot(lspm:::LSPCM_sample, parameter = "alpha", n_dimen = 2) # alpha
#'
#' @family functions
#'
plot.LSPCM <- function(x, n_dimen,
                           parameter = c("clusters", "dimensions", "positions",
                                         "deltas", "variances", "alpha"),
                           conditional = TRUE, true_values,
                           transparency = 0.1, ...) {
  LSPM_object <- x
  parameter <- match.arg(parameter)
  extra_arg <- list(...)

  switch(parameter,
         clusters = ylab <- "Posterior proportion",
         dimensions = ylab <- "Posterior proportion",
         positions = ylab <- "",
         deltas = ylab <- expression(paste("Shrinkage strength")),
         variances = ylab <- expression(paste("Variance")),
         alpha = ylab <- expression(paste("", alpha)),
         stop("Invalid parameter")
  )

  switch(parameter,
         clusters = xlab <- expression(paste(G["+"])),
         dimensions = xlab <- expression(hat(p)),
         positions = xlab <- "",
         deltas = xlab <- "Dimension",
         variances = xlab <- "Dimension",
         alpha =  xlab <- "",
         stop("Invalid parameter")
  )
  # browser()
  if ("cluster" %in% names(LSPM_object) &
      "initialisation" %in% names(LSPM_object)) {
    if(parameter == "clusters") {
      G_proportion = table(LSPM_object$iter_G)/length(LSPM_object$iter_G)

      if (!missing(true_values)) {
        G_col = rep(8, length(G_proportion))
        G_col_true = replace(d_col,(names(G_proportion) == true_values), 2)
        barplot(G_proportion, ylab = ylab, xlab=xlab, ylim = c(0,1), col = G_col_true)
        legend("topright", legend=true_values, lty=1, lwd=3,
               col=2, title="True cluster")
      } else {
        barplot(G_proportion, ylab = ylab, xlab=xlab, ylim = c(0,1))
      }

    } else if(parameter == "dimensions") {
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
      if(conditional){
        z_mean <- apply(LSPM_object$positions[,1:n_dimen,LSPM_object$iter_d == n_dimen,drop=FALSE], c(1,2), mean, na.rm=TRUE)
        cluster_mean_mean <- apply(LSPM_object$cluster_mean[,1:n_dimen,LSPM_object$iter_d == n_dimen,drop=FALSE], c(1,2), mean, na.rm=TRUE)
        var_mean <- apply(LSPM_object$variances[LSPM_object$iter_d == n_dimen,1:n_dimen,drop=FALSE], 2, mean, na.rm=TRUE)
        sd95 <- 2*sqrt(var_mean)
      } else {
        z_mean <- apply(LSPM_object$positions[,1:n_dimen,,drop=FALSE], c(1,2), mean, na.rm=TRUE)
        cluster_mean_mean <- apply(LSPM_object$cluster_mean[,1:n_dimen,,drop=FALSE], c(1,2), mean, na.rm=TRUE)
        var_mean <- apply(LSPM_object$variances[,1:n_dimen,drop=FALSE], 2, mean, na.rm=TRUE)
        sd95 <- 2*sqrt(var_mean)
      }
      # browser()
      number_dim_plot = n_dimen
      if(n_dimen ==1){
        plot(z_mean[,1], y=rep(0,length(z_mean[,1])), xlab="Dimension 1",
             ylab="", yaxt="n",...)
      } else if(number_dim_plot ==2) {
        plot(z_mean[,1:2], xlab="Dimension 1", ylab="Dimension 2", ...)
        # points(cluster_mean_mean, col=1:8, pch=4, cex=3)
        # draw.ellipse(x = cluster_mean_mean[,1], y = cluster_mean_mean[,2],
        #              a = sd95[1], b = sd95[2], border =1:8 )
      }
      else{
        pairs(z_mean[,1:n_dimen],
              labels=paste("Dim", 1:n_dimen), ...)

        # idx <- subset(expand.grid(x=1:n_dimen,y=1:n_dimen),x!=y)
        # i <- 1
        # pairs(z_mean[,1:n_dimen],pch=20,
        #       panel=function(x, y, ...) {
        #         points(x, y, pch=20)
        #         points(cluster_mean_mean[,idx[i,'y']],cluster_mean_mean[,idx[i,'x']],
        #                cex=4,pch=4,col=1:3)
        #         draw.ellipse(cluster_mean_mean[,idx[i,'y']],cluster_mean_mean[,idx[i,'x']], border=1:8,
        #                      a = sd95[idx[i,'y']], b = sd95[idx[i,'x']])
        #         i <<- i +1
        #       })
      }

    } else {
      if(conditional) {
        plot_data <- LSPM_object[[parameter]][LSPM_object$iter_d==n_dimen, , drop=FALSE]
        plot_data_rm_na_col <- plot_data[,colSums(!is.na(plot_data)) > 0, drop=FALSE]
      }
      else {
        plot_data <- LSPM_object[[parameter]]
        plot_data_rm_na_col <- apply(plot_data,2,sort, na.last=T)[,colSums(!is.na(plot_data)) > 0, drop=FALSE]
      }
      vioplot(plot_data_rm_na_col, rectCol=NA, border=NA, lineCol=NA,
              colMed="green", pchMed=20, cex=2, ...)
      title(ylab = ylab, line = 2.2)
      title(xlab = xlab, line = 2.2)
      if (!missing(true_values)) {
        points(true_values, col = "red", pch = 4, cex = 1.5, lwd = 4) # true
      }
    }
  } else if (class(LSPM_object) == "LSPCM") {
    max_d = max(do.call(c,lapply(LSPM_object, function(x) as.integer(names(table(x$iter_d))))))
    all_result <- lapply(LSPM_object, function(x) tabulate(x$iter_d, max_d)/length(x$iter_d))
    all_result_rbinded = do.call(rbind, all_result)
    colnames(all_result_rbinded) = 1:max_d
    all_result_rbinded_rm0 = all_result_rbinded[, colSums(all_result_rbinded != 0) > 0, drop=F]
    d_posterior_mode = colnames(all_result_rbinded_rm0)[apply(all_result_rbinded_rm0,1,which.max)]
    d_posterior_mode_prop = table(d_posterior_mode)/length(LSPM_object)

    d_proportion_mean = apply(all_result_rbinded_rm0,2,mean)
    d_proportion_sd = apply(all_result_rbinded_rm0,2,sd)

    if(parameter=="clusters"){
      max_G = max(do.call(c,lapply(LSPM_object, function(x) as.integer(names(table(x$iter_G))))))
      all_G_result <- lapply(LSPM_object, function(x) tabulate(x$iter_G, max_G)/length(x$iter_G))
      all_G_result_rbinded = do.call(rbind, all_G_result)
      colnames(all_G_result_rbinded) = 1:max_G
      all_G_result_rbinded_rm0 = all_G_result_rbinded[, colSums(all_G_result_rbinded != 0) > 0, drop=F]
      G_posterior_mode = colnames(all_G_result_rbinded_rm0)[apply(all_G_result_rbinded_rm0,1,which.max)]
      G_posterior_mode_prop = table(G_posterior_mode)/length(LSPM_object)
      xlab = expression(G["+"])

      G_proportion_mean = apply(all_G_result_rbinded_rm0,2,mean)
      G_proportion_sd = apply(all_G_result_rbinded_rm0,2,sd)

      plot_data <- G_proportion_mean

      if (!missing(true_values)) {
        G_col = rep(8, length(G_proportion_mean))
        G_col_true = replace(G_col,(names(G_proportion_mean) == true_values), 2)
        xbar = barplot(plot_data, ylab = ylab, xlab=xlab, ylim = c(0,1), col = G_col_true)
        text(xbar, plot_data, labels=round(plot_data,4), pos=3)
        # legend("topright", legend=true_values, lty=1, lwd=3,
        #        col=2, title="True cluster")
      } else {
        xbar = barplot(plot_data, ylab = ylab, xlab=xlab, ylim = c(0,1))
        text(xbar, plot_data, labels=round(plot_data,4), pos=3)
      }

      return(invisible())
    } else if(parameter=="dimensions") {

      if(!missing(n_dimen)){
        if(!(n_dimen %in% d_posterior_mode)){
          return(print("Chosen dimension does not have posterior modal"))
        }
        all_result_rbinded_rm0_d = all_result_rbinded_rm0[d_posterior_mode==n_dimen,, drop=F]
        all_result_rbinded_rm0_d = all_result_rbinded_rm0_d[, colSums(all_result_rbinded_rm0_d != 0) > 0, drop=F]
        d_proportion_mean_d = apply(all_result_rbinded_rm0_d,2,mean)
        d_proportion_sd_d = apply(all_result_rbinded_rm0_d,2,sd)

        plot_data <- d_proportion_mean_d
        xlab = "Posterior mode dimension"
      } else {
        plot_data <- d_proportion_mean
        xlab = expression(hat(p))
      }

      if (!missing(true_values) & !missing(n_dimen)) {
        d_col = rep(8, length(d_posterior_mode_prop))
        d_col_true = replace(d_col,(names(d_posterior_mode_prop) == true_values), 2)
        xbar = barplot(plot_data, ylab = ylab, xlab=xlab, ylim = c(0,1), col = d_col_true)

        legend("topright", legend=true_values, lty=1, lwd=3,
               col=2, title="True dimension")
      } else if(!missing(true_values) & missing(n_dimen)) {
        d_col = rep(8, length(d_proportion_mean))
        d_col_true = replace(d_col,(names(d_proportion_mean) == true_values), 2)
        xbar = barplot(plot_data, ylab = ylab, xlab=xlab, ylim = c(0,1.1), col = d_col_true)
        text(xbar, plot_data, labels=round(plot_data,4), pos=3)
        # legend("topright", legend=true_values, lty=1, lwd=3,
        #        col=2, title="True dimension")
      } else {
        xbar = barplot(plot_data, ylab = ylab, xlab=xlab, ylim = c(0,1.1))
        text(xbar, plot_data, labels=round(plot_data,4), pos=3)
      }

      if(!missing(n_dimen)){
        error.bar(xbar,plot_data, d_proportion_sd_d)
      }
      return(invisible())

    } else if(parameter=="positions") {
      return("Positions not supported for multi LSPM results")
    } else{

      if(!missing(n_dimen) & conditional){
        LSPM_containing_d = do.call(rbind, lapply(LSPM_object, function(x) t(tabulate(x$iter_d, n_dimen))))[,n_dimen] != 0
        d_posterior_mode = d_posterior_mode[LSPM_containing_d]
        if(parameter=="alpha") {
          all_result <- lapply(LSPM_object[LSPM_containing_d], function(x) x[[parameter]][x$iter_d == n_dimen, ,drop=F])
        } else {
          all_result <- lapply(LSPM_object[LSPM_containing_d], function(x) x[[parameter]][x$iter_d == n_dimen, 1:n_dimen, drop=F])
        }
        all_result = all_result[lapply(all_result,length)>0][d_posterior_mode==n_dimen]
        all_result = lapply(all_result, function(x) x[,colSums(!is.na(x)) > 0, drop=FALSE])
      } else {
        all_result <- lapply(LSPM_object, function(x) x[[parameter]])
        all_result = all_result[lapply(all_result,length)>0]
        all_result <- lapply(all_result, function(x) apply(x,2,sort, na.last=T)[,colSums(!is.na(x)) > 0, drop=FALSE])
      }
      # print(names(all_result)[lapply(all_result,length)<= 0]) # printing those that doesnt have the specified dimension


      vioplot(do.call(rbind, all_result),
              drawRect = FALSE, border = NA,
              col = rgb(red = 0, green = 0, blue = 0, alpha = 0), ...
      )
      if("add" %in% names(extra_arg)){

      } else{
        title(ylab = ylab, line = 2.2)
        title(xlab = xlab, line = 2.2)
      }

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

      # if (ncol(all_result[[1]]) == 1) {
      #   points(unlist(lapply(all_result, mean)), pch = 20, cex = 2, col = "green")
      # } else if (ncol(all_result[[1]]) > 1) {
      #   points(apply(t(as.data.frame(lapply(all_result, function(x)
      #     apply(x, 2, mean, na.rm=TRUE)))), 2, mean), pch = 20, cex = 2, col = "green")
      # }
    }
  } else {
    return(print("Invalid object, please insert results from LSPM"))
  }
}
