moveFile <- function(fileId,parentId){
  myFile <- synapseClient::synGet(fileId,downloadFile = FALSE);
  myFile$properties$parentId <- parentId;
  myFile <- synapseClient::synStore(myFile,forceVersion=F);
}
