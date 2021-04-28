#################
## Iterate over loci
#################

#' Fit \code{\link{main_p2}()} across many SNPs.
#'
#' Support is provided for parallelization through the future package.
#'
#' @param nmat A matrix of counts where the columns index the genotypes and
#'     the rows index the loci. \code{nmat[i, j]} contains the number
#'     of individuals with genotype \code{j-1} at locus \code{i}.
#'
#' @return A matrix with two columns. The first column contains the estimated
#'     allele frequency, the second column contains the estimated double
#'     reduction parameter. The rows index the loci.
#'
#' @author David Gerard
#'
#' @export
#'
#' @importFrom foreach %dopar%
#' @importFrom doRNG %dorng%
#' @import doFuture
#' @importFrom foreach foreach
#'
#' @examples
#' ## Fake data
#' nmat <- matrix(c(1, 2, 3, 2, 1,
#'                  3, 2, 1, 2, 3),
#'                nrow = 2,
#'                byrow = TRUE)
#'
#' ## Run the analysis in parallel on the local computer with two workers
#' future::plan(future::multisession, workers = 2)
#' main_multi(nmat = nmat)
#'
#' ## Shut down parallel workers
#' future::plan("sequential")
#'
#' @references
#' \itemize{
#'   \item{Jiang, Libo, Xiangyu Ren, and Rongling Wu. 2021. "Computational Characterization of Double Reduction in Autotetraploid Natural Populations." \emph{The Plant Journal} 105 (6): 1703–9. \doi{10.1111/tpj.15126}}
#' }
#'
main_multi <- function(nmat) {
  stopifnot(ncol(nmat) == 5)

  ## Register doFutures() ----
  oldDoPar <- registerDoFuture()
  on.exit(with(oldDoPar, foreach::setDoPar(fun=fun, data=data, info=info)), add = TRUE)

  ## Run foreach ----
  nvec <- NULL
  outmat <- foreach(nvec = iterators::iter(nmat, by = "row"),
                   .combine = rbind) %dorng% {
                     ## 4:0 because they code number of alternative, not reference, alleles
                     nm <- unlist(mapply(x = 4:0, times = nvec, FUN = rep))
                     main_p2(nm = nm)
                   }
  colnames(outmat) <- c("r", "alpha")

  return(data.frame(outmat))
}
