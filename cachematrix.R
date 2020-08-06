
##Assignment: Caching the Inverse of a Matrix
##  makeCacheMatrix creates a special object that stores a matrix and cache's its inverse.


makeCacheMatrix<- function(x=matrix()){
  invrs=NULL
  set<-function(y){
    x<<-y
    invrs=NULL
  }
  get<-function() x
  setInverse<-function(inverse) invrs<<-inverse
  getInverse<-function() invrs
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## cacheSolve calculates the inverse of the matrix, which is returned by makeCacheMatrix. If the matrix matches with a previously calculated matrix, it retrieves the result (inverse) from the cache.

cacheSolve<-function(x, ...){
  invrs<-x$getInverse()
  if(!is.null(invrs)){
    message("Getting cached data...")
    return(invrs)
  }
  data<- x$get()
  invrs<-solve(data, ...)
  x$setInverse(invrs)
  invrs
}