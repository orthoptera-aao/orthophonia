#' Applies automatic bandpass filter to an AM sound
#' 
#' This function assumes that the wave is a recording containing principally 
#' a single source and amplitude modulated song. 
#' It tries to find and apply automatic bandpass filter in order to reduce noise.
#' 
#' @param wave an R object or path to a wave file
#' @param bps the bandpass size relative to \deqn{\sigma}{sigma}
#' @param min_freq the lowest expected frequency, in Hz
#' @param max_freq the highest expected frequency, in Hz (Default: NULL to use Nyquist frequency)
#' @param wl the window length for spectrogram generation
#' @param plot whether to plot the power spectrum
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
  max_freq=NULL,
  wl=2^10,
  plot=F
){
  #If max_freq not set then use half of sample rate
  if (is.null(max_freq) {
    max_freq <- wave@samp.rate / 2
  }
  
  std_wave <- standardiseWave(wave)
  spec <- meanspec(std_wave, wl=wl,ovlp = 75, plot=plot)
  
  spec <- spec[spec[,"x"] > min_freq/1e3,]
  f <- spec[which.max(spec[,"y"]),"x"] * 1e3
  props <- specprop(spec)
  sigma <- props$sd
  
  min_f <- max(c(0, f  - sigma * bps/2))
  max_f <- min(max_freq, f + sigma * bps/2)
  filt_wave <- bwfilter(std_wave, 
                        from = min_f, 
                        to = max_f, output="Wave")
  filt_wave <- standardiseWave(filt_wave)
  return(filt_wave)
}
NULL
#' Standardise a Wave object for package consistency
#' 
#' This function defines the internal data representation used in this package.
#' It is interanlly called in most other functions. 
#' 
#' @param wave an R object or the path of a wave file
#' @param f the expected target frequency
#' @param stereo wether the expected object is stereo (else it is mono)
#' @param bit the bit depth
#' @return a \code{Wave}  object that may have been altered to match package standard representation
#' @export
standardiseWave <- function(wave, f=44100, stereo=FALSE, bit=1){
  # we also allow wave to be a file
  if(is.character(wave))
    wave <- readWave(wave)
  
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
  out <- normalize(out, unit=as.character(bit))
  return(out)
}



