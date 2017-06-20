# Function to change entity ACL (also creates new acls if it doesn't exist)
synChangeEntityACL <- function(id, new.racl){
  tryCatch({ 
    # If old acl exist update
    old.acl = synGetEntityACL(id)
    old.acl@resourceAccess = new.racl
    synUpdateEntityACL(old.acl)
  }, error = function(e){ 
    # If not create a new acl
    new.acl = AccessControlList(id = id, resourceAccess = new.racl)
    synCreateEntityACL(new.acl)
  })
}