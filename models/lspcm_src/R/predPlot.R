#' Posterior Predictive Plots for LSPM
#'
#' Draw overlaying violin plots representing the posterior predictive
#' distributions.
#'
#' @param ... Accepts predcheck object(s) from binary or count network
#' \describe{
#' \item{Binary}{Accepts comma seperated objects to be indicated as separate
#' violin plots}
#' \item{Count}{Accepts comma seperated objects to show as separate violin plots
#' or/and accept a list of objects to give values or draw within violin plots }}
#' @param type The network type with edges being binary valued or count valued
#' @param obs_dens The observed density of the network for binary network
#' metrics
#' @param obs_trans The observed transitivity of the network for binary network
#' metrics
#' @param x_axis_name Used to input the x-axis tick name for the violin plots
#' that is produced for count network metrics.
#' @param true_position The true latent position from networkMGP used to
#' simulate the network. Currently support within list but not across list.
#'
#' @details Returns violin plots of the posterior predictive distribution.
#' If the observed network density and transitivity is given in the function,
#' they will appear as red lines in the plot.
#' \describe{
#' \item{Binary}{
#' \describe{
#'
#' \item{Accuracy}{Measure of all the correctly identified edges.
#'  \deqn{\frac{TP + TN}{n},} where TP is true positive, TN is true negative,
#'  and n is total number of node.}
#'  \item{F1 score}{Harmonic mean of precision (P) and recall (R).
#'  \deqn{F1 = 2 \times \frac{P \times R}{P + R}.}}
#'  \item{Density}{Network density is the ratio of the number of observed
#'  dyadic connections over the number of possible dyadic connections.}
#'  \item{Transitivity}{Number of triangles divided by the total number of
#'  connected triples}
#'  \item{Hamming distance}{The normalised simple differences between the
#'  observed network and network simulated from the posterior predictive
#'  distribution}}
#' }
#' \item{Count}{
#' \describe{
#' \item{Mean Absolute Difference}{The mean absolute count difference between
#' the observed network and the network simulated with parameters from the
#' posterior distribution}
#' \item{log Count}{The logarithmic count distribution of the networks}
#' \item{Ratio of pairwise distance}{The ratio between the estimated latent
#' positions and the true latent positions.}
#' }
#' }
#' }
#'
#' @export
#'
#' @importFrom stats density
#' @importFrom graphics title par abline
#' @importFrom grDevices colorRampPalette
#' @importFrom vioplot vioplot
#'
#' @family functions
#'
#'
#' @examples
#' # Obtain posterior predictive distribution results
#' predLSPM <- predcheck(10, 2, lspm:::LSPM_sample, type = "binary")
#'
#' # Plot the posterior predictive distribution results for binary network
#' predPlot(predLSPM)
#'
#' # Plot the posterior predictive distribution results for count network
#' countMTGP_2D <- networkMTGP(n = 50, alpha = 1, deltas = c(0.5,1.1),
#' type = "count", seed = 11)
#'
#' countLSPM <- LSPM(countMTGP_2D$network, family = "Poisson",
#' step_size = c(3,0.6), iter = 1e4, burnin = 1000, thin = 10)
#'
#' predLSPM_count <- predcheck(10, 2, countLSPM, type = "count")
#'
#' predPlot(predLSPM_count, type="count")
#'
predPlot <- function(..., type=c("binary", "count"), obs_dens, obs_trans, x_axis_name, true_position) {
  posterior_results <- list(...)
  obj_length = length(posterior_results)
  edgetype <- match.arg(type)

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if(edgetype == "binary") {
    par(mfrow=c(1,5))
    for(check_type in c("Density","Transitivity","Accuracy","F1score","Hamming")){
      vioplot(lapply(posterior_results, function(x) x[,check_type]), drawRect=FALSE)
      title(ylab=check_type, line=2.2)

      if(!missing(obs_dens)) {
        if(check_type == "Density"){
          abline(h=obs_dens, col="red", lwd=2)
        }
      }
      if(!missing(obs_trans)) {
        if(check_type == "Transitivity"){
          abline(h=obs_trans, col="red", lwd=2)
        }
      }
    }
  } else if(edgetype == "count") {
    if(all(sapply(posterior_results, names) == c("Networks", "alpha_mean", "z_mean", "observed_net" ))) {


      collection_of_MAS <- sapply(posterior_results,
                                  function (y) mean(apply(y$Networks, 3, function(x) abs_diff(x, y$observed_net))))
      cat("Mean Absolute Difference:", collection_of_MAS, fill = TRUE)

      collection_of_ratio = c()
      # Ratio of pairwise distance
      if(!missing(true_position)) {
        par(mfrow=c(1,2))
        true_dist = as.matrix(dist(true_position, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2
        est_dist = lapply(posterior_results,
                          function(x) as.matrix(dist(x$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
        ratio = lapply(est_dist, function(x) x/true_dist)
        collection_of_ratio = ratio
        obj_length = length(posterior_results)
        plot(density(unlist(ratio),na.rm = T, bw=.02), type="n",
             main = "", yaxt="n", ylab="", xlab = "Ratio of pairwise distance", ylim=c(0,20), xlim=c(0,2))


        colors_seq = colorRampPalette(c("#3300CC", "#00FFCC"))(obj_length)
        for(i in 1:obj_length) {
          lines(density(as.vector(ratio[[i]]),na.rm = T, bw=.02), col=colors_seq[i])
        }
        legend("topright", legend=1:obj_length, col=colors_seq, lty=1)
      }

      tableCount = do.call(rbind,lapply(posterior_results, function(x)
        t(log(apply(x$Networks + 1, 3, tabulate,100)))))
      colnames(tableCount) = 0:99
      show_dim = sum(colSums(tableCount) >= 0)
      vioplot(tableCount[, 1:show_dim], drawRect = FALSE, border = NA,
              ylim=c(min(tableCount[tableCount > 0])/1.05, max(tableCount[tableCount < Inf])*1.05),
              xlab = "Count", ylab = "log frequency")
      invisible(list("MAS" = collection_of_MAS, "Ratio_z" = collection_of_ratio ,"tableCount" = tableCount))
    } else if(all(sapply(posterior_results, function(x) sapply(x, names)) == c("Networks", "alpha_mean", "z_mean", "observed_net" ))) {
      par(mfrow=c(1,2))

      collection_of_MAS <- do.call(cbind,
                                   lapply(posterior_results,
                                          function(z) sapply(z,
                                                             function (y) mean(apply(y$Networks, 3, function(x) abs_diff(x, y$observed_net))))))
      colnames(collection_of_MAS) <- `if`(!missing(x_axis_name), x_axis_name, 1:obj_length)
      vioplot(collection_of_MAS, rectCol = NA, lineCol = NA,
              colMed = NA, border=NA, ylab="Mean Absolute Difference")

      # Ratio of pairwise distance
      if(!missing(true_position)) {
        true_dist = lapply(true_position, function(x) as.matrix(dist(x, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2)
        est_dist = lapply(posterior_results, function(y) lapply(y,
                                                                function(x) as.matrix(dist(x$z_mean, diag = TRUE, upper = TRUE, method = "euclidean")) ^ 2))


        ratio = list()
        ratio[[obj_length+1]] = NA
        obj_spec_length = sapply(est_dist, length)
        for(i in 1:obj_length){
          for(j in 1:obj_spec_length[i])
            ratio[[i]][[j]] = est_dist[[i]][[j]]/true_dist[[i]]
        }

        plot(density(unlist(ratio),na.rm = T, bw=.02), type="n",
             main = "", yaxt="n", ylab="", xlab = "Ratio of pairwise distance", ylim=c(0,20), xlim=c(0,2))

        colors_seq = colorRampPalette(c("#3300CC", "#00FFCC"))(obj_length)
        for(i in 1:obj_length) {
          for(j in 1:obj_spec_length[i]) {
            lines(density(as.vector(ratio[[i]][[j]]),na.rm = T, bw=.02), col=colors_seq[i])
          }

        }
        legend("topright", legend=`if`(!missing(x_axis_name), x_axis_name, 1:obj_length), col=colors_seq, lty=1)
      }
    }

  }

}
