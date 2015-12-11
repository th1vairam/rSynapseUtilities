#reverse link function
reverseLink <- function(synId){
  foo <- synapseClient::synGet(synId,downloadFile = F)
  fooProperties <- synapseClient::synGetProperties(foo)
  targetId <- fooProperties$linksTo$targetId
  parentId <- fooProperties$parentId
  synapseClient::synDelete(synId)
  moveFile(targetId,parentId)
  #synDelete()
}
