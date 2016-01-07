updateAnnotations <- function(synId,field,value){
  library(synapseClient)
  synapseLogin()
  foo <- synGet(synId,downloadFile = F)
  bar <- synGetAnnotations(foo)
  bar <- as.list(bar)
  bar[field] <- value
  synSetAnnotations(foo) <- bar
  foo <- synStore(foo,forceVersion=F)
}
