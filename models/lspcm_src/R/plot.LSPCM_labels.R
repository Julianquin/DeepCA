#' Posterior similarity matrix heatmap
#'
#' @param x A LSPCM list obtained after running the
#' LSPCM_labels function.
#' @param ... Additional argument to be passed for graphical parameters.
#'
#' @return Plots the heat map of the posterior similarity matrix ordered by the
#' representative cluster labels
#' @export
#'
#' @importFrom seriation pimage
plot.LSPCM_labels <- function(x, ...) {
  cluster_ordering <- order(x$mpear)
  pimage(x$psm[cluster_ordering,cluster_ordering])
}
