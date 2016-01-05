#####
## FUNCTION FOR COPYING ALL SYNAPSE WIKI CONTENT (AND SUBPAGES) FROM ONE OWNER RESOURCE TO ANOTHER
## AUTHOR: BRIAN M. BOT
#####
## ARGUMENTS:
##   oldOwnerId - the Synapse ID of the resource (e.g. Project) to copy the wiki content from
##   newOwnerId - the Synapse ID of the resource (e.g. Project) to copy the wiki content to
#####
## VALUE:
##   Silently returns the Wiki headers from the new owner
#####
## NOTES:
##   If a the newOwnerId already has a Wiki page associated with it, the function fails
#####

#' @export
copyWiki <- function(oldOwnerId, newOwnerId, entityMap=NULL){
  oldOwn <- synapseClient::synGet(oldOwnerId)
  oldWh <- synapseClient::synGetWikiHeaders(oldOwn)

  newOwn <- synapseClient::synGet(newOwnerId)
  wikiIdMap <- list()
  newWikis<-list()
  for( i in 1:length(oldWh) ){
    attDir <- tempfile(pattern="attdir", tmpdir=tempdir(), fileext="")
    dir.create(attDir)
    w <- synapseClient::synGetWiki(oldOwn, oldWh[[i]]@id)
    message(sprintf('Got wiki %s', oldWh[[i]]@id))
    if( length(w@properties$attachmentFileHandleIds) == 0 ){
      dl <- list()
    } else if( length(w@properties$attachmentFileHandleIds) > 0 ){
      fhs <- synapseClient::synRestGET(sprintf('/entity/%s/wiki/%s/attachmenthandles', oldOwnerId, as.character(w@properties$id)))
      dl <- sapply(fhs$list, function(x){
        if(x$concreteType != "org.sagebionetworks.repo.model.file.PreviewFileHandle") {
          downloadUri<-sprintf('/entity/%s/wiki/%s/attachment?fileName=%s&redirect=FALSE', oldOwnerId, as.character(w@properties$id), URLencode(x$fileName))
          thisFile <- synapseClient:::retrieveAttachedFileHandle(downloadUri,"REPO",x,TRUE,attDir,"overwrite.local",FALSE)
          return(file.path(attDir, x$fileName))
        } else{
          return(NULL)
        }
      })
      dl <- as.list(unlist(dl))
    }
    if(is.null(w@properties$parentWikiId)){
      wNew <- synapseClient::WikiPage(owner=newOwn, title=w@properties$title, markdown=w@properties$markdown, attachments=dl)
      wNew <- synapseClient::synStore(wNew)
      parentWikiId <- wNew@properties$id
    } else{
      wNew <- synapseClient::WikiPage(owner=newOwn, title=w@properties$title, markdown=w@properties$markdown, attachments=dl, parentWikiId=wikiIdMap[[w@properties$parentWikiId]])
      wNew <- synapseClient::synStore(wNew)
    }
    newWikis[[wNew@properties$id]]<-wNew
    wikiIdMap[[w@properties$id]] <- wNew@properties$id
  }
  cat("Updating internal links:\n")
  for (oldWikiId in names(wikiIdMap)) {
    # go through each wiki page once more:
    newWikiId<-wikiIdMap[[oldWikiId]]
    newWiki<-newWikis[[newWikiId]]
    cat(sprintf("\tPage: %s\n", newWikiId))
    s<-newWiki@properties$markdown
    # in the markdown field, replace all occurrences of oldOwnerId/wiki/abc with newOwnerId/wiki/xyz,
    # where wikiIdMap maps abc->xyz
    # replace <oldOwnerId>/wiki/<oldWikiId> with <newOwnerId>/wiki/<newWikiId>
    for (oldWikiId2 in names(wikiIdMap)) {
      oldProjectAndWikiId<-sprintf("%s/wiki/%s", oldOwnerId, oldWikiId2)
      newProjectAndWikiId<-sprintf("%s/wiki/%s", newOwnerId, wikiIdMap[[oldWikiId2]])
      s<-gsub(oldProjectAndWikiId, newProjectAndWikiId, s, fixed=TRUE)
    }
    # now replace any last references to oldOwnerId with newOwnerId
    s<-gsub(oldOwnerId, newOwnerId, s, fixed=TRUE)

    if (!is.null(entityMap)) {
      cat("Updating Synapse references:\n")
      for (oldSynId in names(entityMap)) {
        # go through each wiki page once more:
        newSynId<-entityMap[[oldSynId]]
        s<-gsub(oldSynId, newSynId, s, fixed=TRUE)
      }
      cat("Done updating Synapse IDs.\n")
    }

    newWiki@properties$markdown<-s
    # update the wiki page
    newWiki<-synapseClient::synStore(newWiki)
  }
  cat("Done updating internal links.\n")

  newWh <- synapseClient::synGetWikiHeaders(newOwn)
  return(invisible(newWh))
}
