copyFile <- function(fileId, parentId, version, setAnnotations=TRUE, setActivity=TRUE) {
  # Get the file
  print(sprintf("Getting file %s", fileId))

  myFile <- synapseClient::synGet(fileId, version=version,
                                  downloadFile = FALSE)


  if (!(grepl("^file:///",getFileLocation(myFile)))) {
    myFile <- synapseClient::synGet(fileId, version=version,
                                    downloadFile = TRUE)
    # Create a new file
    newFile <- synapseClient::File(path=synapseClient::getFileLocation(myFile),
                                   name=synapseClient::synGetProperties(myFile)$name,
                                   parentId=parentId)

  }
  else {
    # Create a new file
    newFile <- synapseClient::File(path=synapseClient::getFileLocation(myFile),
                                   name=synapseClient::synGetProperties(myFile)$name,
                                   parentId=parentId,
                                   synapseStore=FALSE)
  }

  print(sprintf("Got file to %s", synapseClient::getFileLocation(myFile)))
  print(sprintf("Putting new file in %s", parentId))


  print(sprintf("Created new file %s", synapseClient::synGetProperties(newFile)$name))

  if (setAnnotations) {
    synapseClient::synSetAnnotations(newFile) <- synapseClient::synGetAnnotations(myFile)
  }

  if (setActivity) {
    synapseClient::generatedBy(newFile) <- synapseClient::synGetActivity(myFile)
  }

  print(newFile)
  newFile <- synapseClient::synStore(newFile)
}

copyFileAllVersions <- function(fileId, parentId) {
  res <- synapseClient::synRestGET(sprintf('/entity/%s/version', fileId))

  obj <- lapply(rev(res$results),
                function(x) copyFile(fileId=x$id,
                                     version=x$versionNumber))

}
