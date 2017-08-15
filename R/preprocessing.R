autoBandPassFilter <- function(
  wave,
  bps=2, 
  min_freq=1000,
  max_freq=NULL,
  wl=2^10,
  plot=FALSE
){
  #If max_freq not set then use half of sample rate
  if (is.null(max_freq)) {
    max_freq <- wave@samp.rate / 2
  }
  
  std_wave <- standardiseWave(wave)
  spec <- seewave::meanspec(std_wave, wl=wl,ovlp = 75, plot=plot)
  
  spec <- seewave::spec[spec[,"x"] > min_freq/1e3,]
  f <- seewave::spec[which.max(spec[,"y"]),"x"] * 1e3
  props <- seewave::specprop(spec)
  sigma <- props$sd
  
  min_f <- max(c(0, f  - sigma * bps/2))
  max_f <- min(max_freq, f + sigma * bps/2)
  filt_wave <- seewave::bwfilter(std_wave, 
                        from = min_f, 
                        to = max_f, output="Wave")
  filt_wave <- standardiseWave(filt_wave)
  return(filt_wave)
}

standardiseWave <- function(wave, f=44100, stereo=FALSE, bit=1){
  # we also allow wave to be a file
  if(is.character(wave))
    wave <- tuneR::readWave(wave)
  
  out <- wave

  if(wave@stereo & !stereo){
    out <- tuneR::mono(out,which="both")  
    warning("Averaging left and right stereo channels to make a mono track")
  }
  
  if(!wave@stereo & stereo){
    out <- tuneR::stereo(out,out)  
    warning("Mono to stereo conversion. Both channels will be duplicates")
  }
  
  if(wave@samp.rate > f)
    out <- tuneR::downsample(out, f)
  if(wave@samp.rate < f)
    stop("Not implemented, need to upsample sound")
  out <- normalize(out, unit=as.character(bit))
  return(out)
}



