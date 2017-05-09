longQueries <- function(query,blockSize=100){
  foo <- synQuery(query,blockSize=blockSize)
  foo$fetch()
  results <- foo$results
  while(nrow(foo$results)>0){
    foo$fetch()
    results <- rbind(results,foo$results)
  }
  return(results)
}
