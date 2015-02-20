## pairs of functions that compute the inverse of a invertible matrix

## makes a cacheable matrix object 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) inverse <<- solve
  getInverseMatrix <- function() inverse
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## compute the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inverse <- x$getInverseMatrix()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverseMatrix(inverse)
  inverse
}
