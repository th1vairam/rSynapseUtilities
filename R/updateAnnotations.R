updateAnnotations <- function(synId,field,value,rmVar=NULL){
  library(synapseClient)
  synapseLogin()
  foo <- synGet(synId,downloadFile = F)
  bar <- synGetAnnotations(foo)
  bar <- as.list(bar)
  if(!is.null(rmVar)){
    bar <- bar[which(!names(bar)%in%rmVar)]
  }
  bar[field] <- value
  synSetAnnotations(foo) <- bar
  foo <- synStore(foo,forceVersion=F)
}
