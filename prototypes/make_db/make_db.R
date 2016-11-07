rm(list=ls())
library(bioacoustica)
library(orthophonia)
library(data.table)
library(ggplot2)

DATA_DIR <- "~/Desktop/ortho_data/"
MAX_ANNOT_DURATION <- 30 #s

pipelineFunction <- function(file_name){
  print(file_name)
  wave <- standardiseWave(file_name)
  wave <- autoBandPassFilter(wave)
  beatSpectrum(wave)
}

all_annotations = getAllAnnotationData()
# we filter for only those made by one author before a certain date
query = all_annotations[id %in% c(31, 277)]
query[,start :=as.numeric(start)]
query[,end :=as.numeric(end) ]
# we force annotations to be shorter than MAX_ANNOT_DURATION
query[, end := ifelse( end < start + MAX_ANNOT_DURATION, end, start + MAX_ANNOT_DURATION)]

# we make one file for each annotation and save it in dst_dir
my_annotations <- dowloadFilesForAnnotations(query,
                                             dst_dir = DATA_DIR,
                                             verbose=T)
my_annotations[]

dt <- my_annotations[ id != 290,
                pipelineFunction(file_name = annotation_path)
                ,by=id]
dt <- dt[my_annotations]

pdf("/tmp/beat_spec.pdf", h=16, w=9)
ggplot(dt, aes(period*1000, power,color=as.character(id))) +
  geom_line() + scale_x_log10()  + facet_grid( taxon_id ~ . ) + scale_y_sqrt()

ggplot(dt[taxon_id == 7], aes(period*1000, power)) +
  geom_line() + scale_x_log10()  + facet_wrap(  ~ id  ) + scale_y_sqrt()

dev.off()
