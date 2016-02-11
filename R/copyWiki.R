## AUTHOR: BRIAN M. BOT

#' Copy all Synapse Wiki content (including sub-pages) from one owner Entity to another.
#'
#' If a the newOwnerId already has a Wiki page associated with it, the function fails
#'
#' @param oldOwnerId the Synapse ID of the resource (e.g. Project) to copy the wiki content from.
#' @param newOwnerId the Synapse ID of the resource (e.g. Project) to copy the wiki content to.
#' @param updateLinks Update the internal Wiki sub-page links.
#' @param updateSynIds Update Synapse entity IDs reference in the Wiki using the supplied entityMap.
#' @param entityMap a list whose keys are the original Synapse entity IDs referenced in the Wiki pages and values are new IDs.
#' @return The Wiki headers from the new owner.
#' @export
copyWiki <- function(oldOwnerId, newOwnerId, updateLinks=TRUE, updateSynIds=TRUE, entityMap=NULL){
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

  if (updateLinks) {
    cat("Updating internal links:\n")
    for (oldWikiId in names(wikiIdMap)) {
      # go through each wiki page once more:
      newWikiId<-wikiIdMap[[oldWikiId]]
      newWiki<-newWikis[[newWikiId]]
      cat(sprintf("\tUpdating internal links Page: %s\n", newWikiId))
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

      newWikis[[newWikiId]]@properties$markdown <- s

    }
  }

  if (updateSynIds & !is.null(entityMap)) {
    cat("Updating Synapse references:\n")
    for (oldWikiId in names(wikiIdMap)) {
      # go through each wiki page once more:
      newWikiId<-wikiIdMap[[oldWikiId]]
      newWiki<-newWikis[[newWikiId]]
      cat(sprintf("\tUpdating Synapse references for Page: %s\n", newWikiId))
      s<-newWiki@properties$markdown

      for (oldSynId in names(entityMap)) {
        # go through each wiki page once more:
        newSynId<-entityMap[[oldSynId]]
        s<-gsub(oldSynId, newSynId, s, fixed=TRUE)
      }
      cat("\tDone updating Synapse IDs.\n")
      newWikis[[newWikiId]]@properties$markdown <- s
    }
  }

  # update the wiki pages
  cat("Storing new Wikis\n")
  for (oldWikiId in names(wikiIdMap)) {
    newWikiId <- wikiIdMap[[oldWikiId]]
    newWikis[[newWikiId]] <- synapseClient::synStore(newWikis[[newWikiId]])
    cat(sprintf("\tStored: %s\n", newWikiId))

  }

  newWh <- synapseClient::synGetWikiHeaders(newOwn)
  return(invisible(newWh))
}
