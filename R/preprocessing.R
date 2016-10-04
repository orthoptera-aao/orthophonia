#@include 
#' Applies automatic bandpass filter to an AM sound
#' 
#' This function assumes that the wave is a recording featuring mainly 
#' a single source, amplitude modulated song and tries to find and apply automatic bandpass filter
#' in order to reduce noise.
#' 
#'
#' @param wave an R object
#' @param bps the bandpass size relative to \deqn{\sigma}{sigma}
#' @return a bandpass filter wave of the same type as \code{wave}
#' @note 
#' todo ref (Dietrich et al., 2004)
#' @details 
#' The \code{bps} arguments refers to the selectivity of the filter.
#' It represents the width of the bandpass filter relative to
#' the standard deviation of the PDF, in frequency range.
#' In the original publication \code{bps} = 2 (i.e. \deqn{2\sigma}{2 sigmas}, see eq. 3).
#' The present implementation allow to alter this parameter. 
#' Higher values will result in broader (i.e. less selective) filter.
# @examples
# @seealso \code{\link{...}}
#' @export
autoBandPassFilter <- function(
  wave,
  bps=2
  ){
  
  
  return(wave)
}