  ## Cache of inverse of matrix with Lexical scoping
  ## functions do
  
  ## This function creates the object inverse of the matrix and appears to be stored in the cache
  
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
  
  
  ## The function returns a matrix which is inverse of x, if there is already stored cache,
  #The cached value is returned
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
  
}
