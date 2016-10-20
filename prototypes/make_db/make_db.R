rm(list=ls())
library(bioacoustica)
library(data.table)



getAnnotationFromID <- function(uid=NULL,
                           path="/R/annotations") {
  annotations <- as.data.table(bioacoustica.call(path))
  if(!is.null(uid))
    return(annotations[id == uid])
  return (annotations)
}


#a <- getAnnotationFromID(uid =202)
#a <- getAnnotationFromID()
#a[type == "Call", .(taxon, recording,id)]

#' Get data from a unique annotation identifier
#' 
#' Annotations refer implicitly to a section of an audio file, 
#' along with some metadata such as taxon, type...
#' This function retreives the metadata for an annotation_id and, optionally, 
#' downloads the corresponding file section to a local .wav.
#' 
#' @param annotation_id the unique bioacoustic identifier
#' @param dst_dir where to save the file. If null (default), no file is saved
#' @param overwrite whether to overwrite preexiting local files (e.g. if upstream annotation was updated)
#' @return a data.table containing the annotation id \code{id} 
#' the filename allocated to this annotation (\code{file_name}) as well as
#' the assotiated metadata
#' @examples 
#' # make a small library of annotations
#' annotations_to_fetch <- c(197, 200, 202)
#' all_annotations <- lapply(annotations_to_fetch,
#'                          getDataFromAnnotation,
#'                          dst_dir = "~/Desktop/ortho_data/")
#' # Merge all data in one table
#' dt <- rbindlist(all_annotations)
#' # taxom, type and other data is there
#' print(dt)
#' @export
getDataFromAnnotation <- function(annotation_id, 
                                  dst_dir=NULL,
                                  overwrite=F) {
  all_annotations <- getAnnotationFromID(uid=annotation_id)
  target_annotation <- all_annotations[id == annotation_id]
  if(nrow(target_annotation) == 0)
    stop(paste("No such annotation:", annotation_id))
  if(nrow(target_annotation) > 1)
    stop(paste("Duplicated annotation:", annotation_id, 
               ". This is an upstream issue.",
               "Contact database maintainer"))
  
  url <- as.character(target_annotation[, file])
  dst_filename <- paste(annotation_id, basename(url), sep="_")
  target_annotation[,file_name := dst_filename]
  
  if(is.null(dst_dir)){
    return(target_annotation)
  }
  if( !dir.exists(dst_dir))
    stop(paste("No such directory:", dst_dir))
  
  dst_full_path <- paste(dst_dir, dst_filename, sep="/" )
  if(!overwrite & file.exists(dst_full_path)){
    warning(paste(dst_filename, "Already exists in", 
                  dst_dir, "not overwriting"))
    return(target_annotation)
  }
    
  tmp_file <- tempfile("bioacoustica_", fileext = ".wav")
  # todo ensure file is removed (via trycatch)
  print(annotation_id)
  download.file(url, destfile=tmp_file);
  long_wave <- readWave(tmp_file);
  unlink(tmp_file)
  
  f <- long_wave@samp.rate;
  sub_wave <- cutw(long_wave, f=f, 
               from=target_annotation[,start], 
               to=target_annotation[,end], 
               method="Wave",# what does that do?
               output="Wave");
  
  savewav(sub_wave,
          filename=dst_full_path)
  
  target_annotation
}

annotations_to_fetch <- c(197, 200, 202, 204, 
                          205, 207, 209, 211, 
                          213, 218, 220, 222,
                          224, 225, 227, 229,
                          231, 232,46)

all_annotations <- lapply(annotations_to_fetch,
                          getDataFromAnnotation,
                          dst_dir = "~/Desktop/ortho_data/")

dt <- rbindlist(all_annotations)
setkeyv(dt, "id")


testFun <- function(file_name, data_dir){
  path <- paste(data_dir,file_name, sep="/")
  wave <- standardiseWave(path)
  if(round(length(wave@left) / wave@samp.rate, 2) > 30.01)
    wave <- cutw(wave, f=wave@samp.rate, 
       from=0, 
       to=30, 
       method="Wave",# what does that do?
       output="Wave");
  
  print(file_name)
  
  beatSpectrum(wave)
}



o <- dt[,testFun(file_name, data_dir="~/Desktop/ortho_data/"),by=id]
dt <- o[dt]

ggplot(dt, aes(period*1000, power,color=as.character(id))) + 
  geom_line() + scale_x_log10()  + facet_grid( taxon ~ . )
  annotate("segment", 
           x = struct, xend = struct,
           y = -Inf, yend = +Inf,
           colour = "blue")
