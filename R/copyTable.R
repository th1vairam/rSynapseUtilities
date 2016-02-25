#' @export
#' @param tableId Synapse ID of the table to copy.
#' @param parentId Synapse ID of the project to copy this table to.
#' @param setAnnotations Copy the annotations from the existing table to the new copy.
copyTable <- function(tableId, parentId, setAnnotations=FALSE) {

  # Get the file
  print(sprintf("Getting table %s", tableId))
  myTableSchema <- synapseClient::synGet(tableId)

  # Query data from table
  d <- synTableQuery(sprintf('select * from %s', foo@properties$id))@values

  # Remove row names, otherwise making a new table will try to perform
  # an update instead of insert
  rownames(d) <- NULL

  # Get column IDs as we don't need to recreate them
  colIds <- unlist(lapply(myTableSchema@columns@content, function(x) x$id))

  newTableSchema <- TableSchema(name=myTableSchema@properties$name,
                                parent=parentId,
                                columns=colIds)

  if (setAnnotations) {
    synapseClient::synSetAnnotations(newTableSchema) <- synapseClient::synGetAnnotations(myTableSchema)
  }

  newTable <- Table(tableSchema = newTableSchema, values=d)

  cat(sprintf("Created new table using schema %s", synapseClient::synGetProperties(newTableSchema)$name))

  newTable <- synStore(newTable)

}
