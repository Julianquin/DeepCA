#' LSPCM estimated representative cluster
#'
#' Finds the representative partition of the posterior through some type of
#' clustering strategy.
#'
#' @param LSPCM_object A LSPCM list containing the MCMC chains for the
#' different parameters resulting from running the LSCPM function.
#' @param type The types of strategy used to find the representative partition.
#' Here, it accepts "PEAR".
#' @param true_labels A vector of true labels of the nodes if they are known.
#'
#' @return Returns a list containing the posterior similarity matrix and the
#' representative cluster labels. If the nodes' true labels are provided, the
#' adjusted Rand index is calculated and added into the list of results.
#' @export
#'
#' @importFrom mclust adjustedRandIndex
#' @importFrom mcclust comp.psm maxpear relabel
#'
#' @examples
#' # Simulate binary network from positions of 2 dimensions and 3 clusters
#' sampleLSPCM_2D <- networkMTGP_cluster(n = 50, n_dimen = 2, G = 3, alpha = 7,
#' cluster_mean = matrix(c(0,0, -5,0, -5,5), ncol = 2, byrow = TRUE), seed = 1,
#' deltas = matrix(rep(c(.5,1.05), 3), nrow = 3, byrow = TRUE))
#'
#' # Fitting a logistic LSPCM.
#' resultLSPCM_2D <- LSPCM(sampleLSPCM_2D$network, n_dimen = 4, G = 10,
#' adapt_param = c(4, 1e-5), iter = 5e4, burnin = 1e3, thin = 300,
#' dim_threshold = c(0.8,0.9,5), step_size = c(3.3,1), mix_prior = 0.05)
#'
#' # To find the representative cluster
#' resultLSPCM_cluster <- LSPCM_labels(resultLSPCM_2D, type = "PEAR",
#' true_labels = sampleLSPCM_2D$cluster)
#'
LSPCM_labels <- function(LSPCM_object, type = "PEAR", true_labels) {
  results <- list()
  psm <- comp.psm(LSPCM_object$cluster)
  results$psm <- psm
  if(type=="PEAR"){
    mpear <- maxpear(psm,
                     LSPCM_object$cluster, method = "all",
                     max.k = calculate_mode(LSPCM_object$iter_G))
    results$mpear <- mpear$cl[1,]

    if(!missing(true_labels)){
      ARI <- adjustedRandIndex(mpear$cl[1,], true_labels)
      results$ARI_pear <- ARI
    }
  }

  # @importFrom mcclust.ext minVI minbinder.ext plotpsm
  # if(type=="VI"){
  #   vi <- minVI(psm,
  #                    LSPCM_object$cluster, method = "all",
  #                    max.k = calculate_mode(LSPCM_object$iter_G))
  #   results$vi <- vi$cl[1,]
  #
  #   if(!missing(true_labels)){
  #     ARI <- adjustedRandIndex(vi$cl[1,], true_labels)
  #     results$ARI_vi <- ARI
  #   }
  # }
  #
  # if(type=="Binder"){
  #   binder <- minbinder.ext(psm,
  #                    LSPCM_object$cluster, method = "all",
  #                    max.k = calculate_mode(LSPCM_object$iter_G))
  #   results$binder <- binder$cl[1,]
  #
  #   if(!missing(true_labels)){
  #     ARI <- adjustedRandIndex(binder$cl[1,], true_labels)
  #     results$ARI_binder <- ARI
  #   }
  # }

  # if(type == "Stephen"){
  #   stephen <- relabel(LSPCM_object$cluster, print.loss = FALSE)
  #   results$stephen <- stephen
  #
  #   if(!missing(true_labels)){
  #     ARI <- adjustedRandIndex(stephen, true_labels)
  #     results$ARI_stephen <- ARI
  #   }
  # }
  class(results) <- "LSPCM_labels"
  return(results)
}
