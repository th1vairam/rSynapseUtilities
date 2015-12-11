moveSingleFile <- function(synId,newParentId){
  #function to move a single file (synId) and place a link where it was given a new parentId (newParentId)
  foo <- synapseClient::synGet(synId,downloadFile=F)
  oldParentId <- synapseClient::synGetProperties(foo)$parentId
  fileName <- synapseClient::synGetProperties(foo)$name
  moveFile(synId,newParentId)
  makeLink(synId,oldParentId,linkName=fileName)
}
