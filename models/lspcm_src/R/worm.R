#' Worm network
#'
#' This binary directed network contains n = 272 nodes of neurons from the
#' nervous system of the Caenorhabditis elegans adult male worm, with each of
#' the 4451 edges representing the presence of either a chemical or electrical
#' interaction between nodes. The data were reconstructed from serial electron
#' micrograph sections by Jarrell et al. (2012)
#'
#' @usage data("worm")
#' @format A 272 x 272 binary adjacency matrix, with 0 down the diagonal.
#' @source See reference below
#' @references
#' \describe{
#'  \item{}{Jarrell, T. A., Wang, Y., Bloniarz, A. E., Brittin, C. A., Xu, M.,
#'  Thomson, J. N., Albertson, D. G., Hall, D. H., and Emmons, S. W. (2012).
#'  ``The connectome of a decision-making neural network." Science, 337(6093):
#'  437–444.}
#'  }
"worm"
