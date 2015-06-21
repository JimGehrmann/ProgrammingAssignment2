## makeCacheMatrix creates four functions, get, set,
## setmatinv, and getmatinv
## get will get the value of a matrix (square and invertible)
## set will change values in a matrix
## getmatinv displays the inverse of a square invertible matrix
## setmatinv will change values of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
matinv<-NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setmatinv <- function(inverser) matinv <<- inverser
  getmatinv <- function() matinv
  
  list(set = set, get = get,setmatinv=setmatinv,getmatinv=getmatinv)
}


## cacheSolve will compute the inverse of a square invertible matrix if
## the inverse hasn't been created and cached.
## If the inverse has been cached, a statement saying the matrix inverse has
## been found will be displayed

cacheSolve <- function(x, ...) {

  matinv<-x$getmatinv()
  if(!is.null(matinv))
  {message("getting cached data")
    return(matinv)
    
  }
  data<-x$get()
  matinv<-solve(data,...)
  x$setmatinv(matinv)
  matinv

}
