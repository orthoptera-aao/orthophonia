rm(list=ls())
library(bioacoustica)

fetchBioAcousticaFile <- function(annotation_id, dst_dir){
  # todo change r package upstream to be able
  # to save in a dst directly
  if(!dir.exists(dst_dir))
    stop("distination directory does not exist")
  current_wd = getwd()
  setwd(dst_dir)
  file_name = bioacoustica.getAnnotationFile(annotation_id)
  setwd(current_wd)
  return(file_name)
}

library(data.table)
q <- fread("query.csv")
q2 <- q[,
       .(file_name = fetchBioAcousticaFile(bioacoustica_id, 
                                           "/tmp/make_db")),
         by=id]








# 
# ?bioacoustica.downloadDemoData
# bioacoustica.downloadDemoData(bioacoustica.britishOrthoptera, sep_dirs=FALSE)