copyFile <- function(fileId, parentId, version, setAnnotations=TRUE, setActivity=TRUE) {
  # Get the file
  myFile <- synapseClient::synGet(fileId, version=version,
                                  downloadFile = TRUE)

  # Create a new file
  newFile <- synapseClient::File(file=synapseClient::getFileLocation(myFile),
                                 name=synapseClient::synGetProperties(myFile)$name,
                                 parentId=parentId)

  if (setAnnotations) {
    synapseClient::synSetAnnotations(newFile) <- synapseClient::synGetAnnotations(myFile)
  }

  if (setActivity) {
    synapseClient::generatedBy(newFile) <- synapseClient::synGetActivity(myFile, version)
  }

  synapseClient::synStore(newFile)
}

copyFileAllVersions <- function(fileId, parentId) {
  res <- synapseClient::synRestGET(sprintf('/entity/%s/version', fileId))

  obj <- lapply(rev(res$results),
                function(x) copyFile(fileId=x$id,
                                     version=x$versionNumber))

}
