mintDOI = function(synId){
  #mints a doi for the current version of the file
  #library(synapseClient)
  #synapseLogin()
  synObj <- synGet(synId,downloadFile=F)


  if(synGetProperty(synObj,'entityType')=='org.sagebionetworks.repo.model.FileEntity'){

  version <- synGetProperty(synObj,'versionNumber')
  synRestPUT(paste0('/entity/',synId,'/version/',version,'/doi'),body=list())

  }else{

    synRestPUT(paste0('/entity/',synId,'/doi'),body=list())

  }
}
