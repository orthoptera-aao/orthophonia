#' @include 
NULL
#' Applies automatic bandpass filter to an AM sound
#' 
#' This function assumes that the wave is a recording containing principally 
#' a single source and amplitude modulated song. 
#' It tries to find and apply automatic bandpass filter in order to reduce noise.
#' 
#' @param wave an R object
#' @param bps the bandpass size relative to \deqn{\sigma}{sigma}
#' @param min_freq the lowest expected frequency, in Hz
#' @param wl the window length for spectrogram generation
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
# @seealso \code{\link{meanspec}}, internally used to build spectrogram.
#' @export
autoBandPassFilter <- function(
  wave,
  bps=2, 
  min_freq=1000,
  wl=2^10
  ){
  

  std_wave <- standardiseWave(wave)
  spec <- meanspec(std_wave, wl=wl,ovlp = 75, plot=F)
  
  spec <- spec[spec[,"x"] > min_freq/1e3,]
  f <- spec[which.max(spec[,"y"]),"x"] * 1e3
  sigma <- sd(spec)* 1e3
  filt_wave <- bwfilter(std_wave, 
                       from = (f  - sigma * bps/2), 
                       to = (f + sigma * bps/2), output="Wave")
  filt_wave <- standardiseWave(filt_wave)
  return(filt_wave)
}
NULL
#' Standardise a Wave object for package consistency
#' 
#' This function defines the internal data representation used in this package.
#' It is interanlly called in most other functions. 
#' 
#' @param wave an R object
#' @param f the expected target frequency
#' @param stereo wether the expected object is stereo (else it is mono)
#' @param bit the bit depth
#' @return a \code{Wave}  object that may have been altered to match package standard representation
#' @export
standardiseWave <- function(wave, f=44100, stereo=FALSE, bit=1){
  out <- wave

  if(wave@stereo & !stereo){
    out <- mono(out,which="both")  
    warning("Averaging left and right stereo channels to make a mono track")
  }
  
  if(!wave@stereo & stereo){
    out <- stereo(out,out)  
    warning("Mono to stereo conversion. Both channels will be duplicates")
  }
  
  if(wave@samp.rate > f)
    out <- downsample(out, f)
  if(wave@samp.rate < f)
    stop("Not implemented, need to upsample sound")
  print(out)
  out <- normalize(out, unit=as.character(bit))
  return(out)
}

