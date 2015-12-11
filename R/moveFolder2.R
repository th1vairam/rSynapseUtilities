moveFolder2 <- function(folderId,newParentId){
  foo <- synapseClient::synGet(folderId,downloadFile = F)
  oldParentId <- synapseClient::synGetProperty(foo,'parentId')
  synapseClient::synSetProperty(foo,'parentId') <- newParentId
  foo <- synapseClient::synStore(foo,forceVersion=F)
}
