## makeCacheMatrix function will create an object that will store a matrix and cache its inverse.

## makeCacheMatrix has functions set, get, setInverse and getInverse 

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of a matrix created by makeCacheMatrix
## if the inverse was previously calculated and there have been no changes, then
## it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
      message("getting cached data")
      return (i)
  }
  d <- x$get()
  i <- solve(d, ...)
  x$setInverse(i)
  i
}
