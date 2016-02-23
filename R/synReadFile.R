#' #' Read a file directly from Synapse into
#' #'
#' #' @param id Synapse ID.
#' #' @param readfunction A function to read the file.
#' #' @param ... Additional parameters to synGet and user defined readfunction.
#' #' @return Results of the file reading function.
synReadFile <- function(id, readfunction=read.csv, ...) {
  dots <- list(...)

  areSynArgs <- names(dots) %in% c("version", "downloadFile",
                                   "downloadLocation", "ifcollision", "load")

  synArgs <- c(list(id=id), dots[areSynArgs])
  notSynArgs <- dots[!areSynArgs]

  do.call(readfunction, c(synapseClient::getFileLocation(do.call(synapseClient::synGet,
                                                                 synArgs)),
                          notSynArgs))
}

