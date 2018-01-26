## These functions are written with the purpose of caching the inverse of the matrix, which saves a lot of time over computing it over and over.

## This function makes a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) { 
  y <- NULL
  set <- function(z) {
    x <<- z
    y <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) y <<- inverse
  getinverse <- function() y
  list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the  special "matrix" returned by makeCacheMatrix above. IF the inverse has already been calculated (and the matrix has not been changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  y <- x$getinverse()
  if(!is.null(y)) {
    message("getting cached data")
    return(y)
  }
  data <- x$get()
  y <- solve(data, ...)
  x$setinverse(y)
  y
}
