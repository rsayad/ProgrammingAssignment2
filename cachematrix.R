## perform cached inerse operation on amatrix
## 

## create an empty caching matrix structure

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  ## when setting a new matrix, invalidate inverse
  set <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  get <- function() m
  
  ## get cached inverse value
  getInverse <- function() inverse
  ## set cached inverse value
  setInverse <- function(i) inverse <<- i
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Perform a cached inverse matrix operation on x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  originalMatrix <- x$get()
  inverseMatrix <- x$getInverse()
  
  ## if the inverse is already called, use value in cache
  if (is.null(inverseMatrix))
  {
    inverseMatrix = solve(originalMatrix)
    m$setInverse(inverseMatrix)
  }
  
  inverseMatrix
}
