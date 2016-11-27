## These functions cache the inverse of matrix,  
## so that it need not be calculated repeatedly

## This function creates a matrix that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
    x <<- y
    i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set=set, get=get,
       setinverse=setinverse,getinverse=getinverse)
  
}


## This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i            
  ## Return a matrix that is the inverse of 'x'
}
