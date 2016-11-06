#makeCacheMatrix: This function creates a special "matrix" object that can #cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  Mtx_Inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    Mtx_Inverse <<- NULL
  }
  getMatrix <- function() x
  setMatrixInverse <- function(Inverse) Mtx_Inverse <<- Inverse
  getMatrixInverse <- function() Mtx_Inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" #returned by makeCacheMatrix above. If the inverse has already been calculated #(and the matrix has not changed), then the cachesolve should retrieve the #inverse from the cache.

cacheSolve <- function(x, ...) {
  Mtx_Inverse <- x$getMatrixInverse()
  if(!is.null(Mtx_Inverse)) {
    message("getting cached data")
    return(Mtx_Inverse)
  }
  data <- x$getMatrix()
  Mtx_Inverse <- solve(data, ...)
  x$setMatrixInverse(Mtx_Inverse)
  Mtx_Inverse
}


