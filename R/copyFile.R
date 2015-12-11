copyFile <- function(fileId, parentId, version, setAnnotations=TRUE, setActivity=TRUE) {
  # Get the file
  myFile <- synapseClient::synGet(fileId, version=version,
                                  downloadFile = TRUE)

  # Create a new file
  newFile <- File(file=synapseClient::getFileLocation(myFile),
                  name=synapseClient::synGetProperties(myFile)$name,
                  parentId=parentId)

  if (setAnnotations) {
  synSetAnnotations(newFile) <- synGetAnnotations(myFile)
  }

  if (setActivity) {
    generatedBy(newFile) <- synGetActivity(myFile, version)
  }

  synapseClient::synStore(newFile)
}

copyFileAllVersions <- function(fileId, parentId) {
  res <- synRestGET(sprintf('/entity/%s/version', fileId))

  obj <- lapply(rev(res$results),
                function(x) copyFile(fileId=x$id,
                                     version=x$versionNumber))

}
