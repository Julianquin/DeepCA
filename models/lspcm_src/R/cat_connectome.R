#' Cat connectome network
#'
#' Adjacency matrix detailing the presence or absence of the corticocortical
#' connections in the cat brain where the cat cortex is divided into 65
#' distinct, non-overlapping regions based on the work of Scannell et al. (1995)
#'
#' @usage data("cat_connectome")
#' @format A 65 x 65 binary adjacency matrix, with 0 down the diagonal.
#' @source See reference below
#' @references
#' \describe{
#'  \item{}{Scannell JW, Blakemore C, Young MP (1995) ``Analysis of
#' connectivity in the cat cerebral cortex." J Neurosci 15:1463-1483,
#' pmid:7869111}
#'  \item{}{de Reus, M. A. and van den Heuvel, M. P. (2013). ``Rich Club
#'  Organization and Intermodule Communication in the Cat Connectome."
#'  Journal of Neuroscience, 33(32): 12929-12939.}
#' }
"cat_connectome"
