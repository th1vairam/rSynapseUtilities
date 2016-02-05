makeTable <- function(df,tableName,projectId){
  library(synapseClient)
  synapseLogin()
  tcresult<-as.tableColumns(df)
  cols<-tcresult$tableColumns
  fileHandleId<-tcresult$fileHandleId
  schema<-TableSchema(name=tableName, parent=projectId, columns=cols)
  table<-Table(schema, fileHandleId)
  table<-synStore(table, retrieveData=TRUE)
}
