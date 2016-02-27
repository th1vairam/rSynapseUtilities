#' @export
copyFile <- function(fileId, parentId, version, setAnnotations=TRUE, setActivity=TRUE) {
  # Get the file
  print(sprintf("Getting file %s", fileId))

  myFile <- synapseClient::synGet(fileId, version=version,
                                  downloadFile = FALSE)

  # Check if this file is in Synapse. When getting a file through synGet
  # with downloadFile=FALSE, if the file is in Synapse (not local or a link)
  # a character(0) is returned for getFileLocation().
  fileInSynapse <- length(getFileLocation(myFile)) == 0

  # If the file is in Synapse, download it and make a new copy
  # to go in the new project.
  # If not, just make a new File that looks like the old one.
  if (fileInSynapse) {
    cat(sprintf("Got file from Synapse to %s\n", synapseClient::getFileLocation(myFile)))
    myFile <- synapseClient::synGet(fileId, version=version,
                                    downloadFile = TRUE)
    # Create a new file
    newFile <- synapseClient::File(path=synapseClient::getFileLocation(myFile),
                                   name=synapseClient::synGetProperties(myFile)$name,
                                   parentId=parentId)

  }
  else {
    # Create a new file
    cat(sprintf("Got file from external reference to %s\n", synapseClient::getFileLocation(myFile)))
    newFile <- synapseClient::File(path=synapseClient::getFileLocation(myFile),
                                   name=synapseClient::synGetProperties(myFile)$name,
                                   parentId=parentId,
                                   synapseStore=FALSE)
  }

  cat(sprintf("Putting new file in %s\n", parentId))

  cat(sprintf("Created new file %s\n", synapseClient::synGetProperties(newFile)$name))

  if (setAnnotations) {
    synapseClient::synSetAnnotations(newFile) <- as.list(synapseClient::synGetAnnotations(myFile))
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
