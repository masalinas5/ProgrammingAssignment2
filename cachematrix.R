## These functions caches the inverse of a matrix

## makeCacheMatrix:This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  mtx <- function (y){
    x <<- y
    xi <<- NULL
  }
  mtz <- function () x
  mtxinverse <- function(inverse) xi <<- inverse
  mtzinverse <- function() xi
  list(mtx = mtx, mtz = mtz, mtxinverse = mtxinverse, mtzinverse = mtzinverse)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xi <- x$mtzinverse()
  if (!is.null(xi)){
    message ("getting cached data")
    return (xi)
  }
  data <- x$mtz()
  xi <- solve (data, ...)
  x$mtxinverse(xi)
  xi
}