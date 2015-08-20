## It will create a matrix, invert it, store the results,
## check if the results are cached and, if so, use them.

## this function creates special matrix to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## this one return inverse matrix above and use the cached data

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}