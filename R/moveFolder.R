moveFolder <- function(folderId,originalParentId,newParentId){
  folderId <- synapseClient::synGet(folderId,downloadFile=F)
  synObj <- crawlSynapseObject(folderId)



}
