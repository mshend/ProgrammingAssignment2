## This program contains a pair of functions to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function (invMat) inverseMatrix <<- invMat
  getInverseMatrix <- function() inverseMatrix
  list(CMset=set, 
       CMget = get, 
       CMsetInverseMatrix = setInverseMatrix, 
       CMgetInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invMat <- x$CMgetInverseMatrix()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$CMget()
  invMat <- solve(data, ...)
  x$CMsetInverseMatrix(invMat)
  invMat
}
