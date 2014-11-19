## Cache the inverse of a Matrix. Call makeCacheMatrix first,
## and then use it to access the cached matrix

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseResult <- NULL
  set <- function(y){
    x <<- y
    inverseResult <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    inverseResult <<- inverse
  }
  getInverse <- function(){
    inverseResult
  }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of a special "matrix" returned by makeCacheMatrix. If
## the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache. Assume that all
## matrix are inversable or square matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseResult <- x$getInverse()
  if(!is.null(inverseResult)){
    message("getting cached data")
    return(inverseResult)
  }
  data <- x$get()
  inverseResult <- solve(data)
  x$setInverse(inverseResult)
  inverseResult
}
