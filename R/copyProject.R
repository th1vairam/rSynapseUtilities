###Function to take a crawled synapse object and populate a new directory with the identified structure.
###Make new folders
###Move files in a given folder
###Replace old files with Links

#' @export
copyProject <- function(synId,G,Q=NULL,topId){
  ######Run a depth first search
  if(is.null(Q)){
    Q <- list();
    Q$adj <- G$adj;
    Q$adj[1:length(Q$adj)] <- 0;
    Q$id <- G$id;
    Q$id[1] <- topId;
    G$id[1] <- topId;
    rownames(G$adj) <- G$id;
    colnames(G$adj) <- G$id;
    rownames(Q$adj) <- G$id;
    colnames(Q$adj) <- G$id;
    names(G$type) <- G$id;
    names(G$name) <- G$id;
    Q$newid <- Q$id;
    names(Q$newid) <- Q$id;
    synId <- topId;

  }

  e <- names(which(G$adj[synId,]==1))
  if(length(e)>0){
    for(i in 1:length(e)){
      #print(e)
      #print(Q$adj)
      if(Q$adj[synId,e[i]]==0){
        #print(e)
        Q$adj[synId,e[i]]<- 1;
        if(G$type[e[i]]=='org.sagebionetworks.repo.model.Folder'){
          #check if folder already exists
          Q$newid[e[i]] <- makeNewFolder(synId,e[i],Q,G);
          Q <- copyProject(e[i],G,Q,topId);
        }else if (G$type[e[i]]=='org.sagebionetworks.repo.model.FileEntity'){
          #moveFile(e[i],Q$newid[synId]);
          #makeLink(G$name[e[i]],e[i],synId)
          #makeLink(G$name[e[i]],e[i],synId)
          w1 <- which(Q$adj[,Q$newid[e[i]]]==1)
          #print(Q$newid[w1])

          cat(sprintf("Copying file named = %s, id = %s to parent %s\n", G$name[e[i]], e[i], Q$newid[w1]))

          copyFile(fileId=as.character(e[i]), parentId=as.character(Q$newid[w1]),version=NULL)
        }else if (G$type[e[i]]=='org.sagebionetworks.repo.model.table.TableEntity'){
          w1 <- which(Q$adj[,Q$newid[e[i]]]==1)
          #print(Q$newid[w1])
          print(c(G$name[e[i]],Q$newid[w1],e[i]))
          copyTable(tableId=as.character(e[i]), parentId=as.character(Q$newid[w1]))
        }
        else if (G$type[e[i]]=='org.sagebionetworks.repo.model.Link') {
          cat(sprintf("Found Link at %s, skipping.", e[i]))
        }else{
          stop(sprintf('Object type not recognized for %s\n', e[i]))
        }
      } else {
        return(Q);
      }
    }
  }else{
    return(Q)
  }
  return(Q)
}
