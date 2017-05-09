getGrandParent = function(x){
  foo <- synGet(x,downloadFile=F)
  bar <- synGet(properties(foo)$parentId,downloadFile=F)
  return(properties(bar)$parentId)
}
