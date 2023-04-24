## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function helps to store matrix and inverses it's cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(i) {
    inverse <<- i
  }
  getInverseMatrix <- function() inverse
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix created by the above function.
## If the inverse is already created, then the inverse from the cache should be retrieved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverseMatrix()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverseMatrix(inverse)
  inverse
}
