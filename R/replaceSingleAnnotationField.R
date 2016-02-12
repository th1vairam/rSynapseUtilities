replaceSingleAnnotationField <- function(synId,oldField,newField){
  foo <- synGet(synId,downloadFile=F)
  anno <- as.list(synGetAnnotations(foo))
  w1<-which(names(anno)==oldField)
  names(anno)[w1]<-newField
  synSetAnnotations(foo) <- anno
  foo <- synStore(foo,forceVersion=FALSE)
}
