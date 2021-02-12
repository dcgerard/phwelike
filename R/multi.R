#################
## Iterate over loci
#################


#' Fit \code{\link{main_p2}()} across many SNPs
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
main_multi <- function(nmat) {
  stopifnot(ncol(nmat) == 5)

  outmat <- matrix(NA_real_, ncol = 2, nrow = nrow(nmat))
  colnames(outmat) <- c("r", "alpha")
  for (i in seq_along(nrow(nmat))) {
    nvec <- nmat[i, ]
    nm <- unlist(mapply(x = 0:4, times = nvec, FUN = rep))
    outmat[i, ] <- main_p2(nm = nm)
  }
  return(data.frame(outmat))
}
