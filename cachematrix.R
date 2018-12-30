## The two functions below, would create a matrix object, cache its inverse, and
## compute its inverse or retrieve it.

##The function below would create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inve <<- inverse
  getinverse <- function() inve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inve <- x$getinverse()
  if (!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  matrix <- x$get()
  inve <- solve(matrix, ...)
  x$setinverse(inve)
  inve
}
