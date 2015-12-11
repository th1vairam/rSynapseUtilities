makeNewFolder <- function(parentId,folderId,Q,G){
  folderName <- G$name[folderId];
  folderParentId <- Q$newid[parentId];
  myFolder <- synapseClient::Folder(name=folderName,parentId=folderParentId);
  myFolder <- synapseClient::synStore(myFolder);
  return(myFolder$properties$id);
}
