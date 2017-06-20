auditSingleFile <- function(synId,dictionaryId,summary=T){
  require(synapseClient)
  synapseLogin()
  dictionaryObj <- synTableQuery(paste0('SELECT * FROM ',dictionaryId))

  #turn dictionary into list
  uniqueFields <- unique(dictionaryObj@values$field)
  dictionary <- lapply(uniqueFields,utilityFunctions::listify,dictionaryObj@values$value,dictionaryObj@values$field)
  names(dictionary) <- uniqueFields


  foo <- synGet(synId,downloadFile=F)
  bar <- as.list(synGetAnnotations(foo))


  annoExist <- length(bar)>0
  fieldsAudit <- utilityFunctions::getNames(bar)
  dictionaryFields <- names(dictionary)
  fieldAuditNew <- dictionaryFields %in% fieldsAudit
  auditResult <- c(annoExist,fieldAuditNew)
  names(auditResult) <- c('annoExists',paste0('has',dictionaryFields))

  checkValues <- function(x,dictionaryFields,dictionary,i){
    foobaz <- NA
    try(foobaz <- x[[dictionaryFields[i]]] %in% dictionary[[dictionaryFields[i]]],silent=T)
    return(foobaz[1])
  }

  valueAuditNew <- fieldAuditNew
  for (i in 1:length(valueAuditNew)){
    valueAuditNew[i] <- checkValues(bar,dictionaryFields,dictionary,i)
  }
  names(valueAuditNew) <- paste0(dictionaryFields,'ValueInDictionary')
  auditResult <- c(auditResult,valueAuditNew)
  if(summary){
    res <- list()
    res$trueVal <- names(auditResult[auditResult==TRUE])
    res$falVal <- names(auditResult[auditResult==FALSE])
    res$minAnno <- mean(auditResult[c('hasconsortium','hascenter','hasstudy','hasassay','hasfileType','hasmodelSystem','hastissueType','hasorganism')])
    t2 <- auditResult[c('hasconsortium','hascenter','hasstudy','hasassay','hasfileType','hasmodelSystem','hastissueType','hasorganism')]
    res$missingFields <- names(t2[which(t2==FALSE)])
    t1 <- auditResult[names(valueAuditNew)]
    res$dictionaryErrors <- names(t1[which(t1==FALSE)])
    return(res)
  }else{
    return(auditResult)
  }
}
