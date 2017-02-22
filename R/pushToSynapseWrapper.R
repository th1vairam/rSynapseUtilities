pushToSynapseWrapper <- function(df,
                                 fileName,
                                 synapseFolderId,
                                 annos,
                                 comment,
                                 usedVector,
                                 executedVector,
                                 activityName1,
                                 activityDescription1){

  library(synapseClient)
  synapseClient::synapseLogin()

  #write df to file
  write.csv(df,file=fileName,quote=F)

  #make File object (where it goes on synapse and comment)
  foo <- synapseClient::File(fileName,
                             parentId=synapseFolderId,
                             versionComment=comment)

  #apply annotations
  synSetAnnotations(foo) = as.list(annos)

  #push to synapse
  foo = synStore(foo,
                 used = as.list(usedVector),
                 executed = as.list(executedVector),
                 activityName = activityName1,
                 activityDescription = activityDescription1)

  #return the synapse object
  return(foo)
}
