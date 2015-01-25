## The below functions help cache the complex inverse operation
## If value is present in cache, it is pulled from the cache, else
## the value is created

## makeCacheMatrix function creates a special matrix function which 
## has sub-functions to get and  set Matrix data, and get and set 
## inverse value for provided Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function checks whether the object provided has any 
## cached data. If yes, it returns the cached value, else fetches 
## the information.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
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
