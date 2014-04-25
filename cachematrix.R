## Together, these functions reduce the time needed to calculate
## the inverse of a cached matrix

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {   #Sets the value of the object
    x <<- y
    m <<- NULL
  }
  get <- function() x     #Extracts the value from the object
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function generates the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()         #Extracts the matrix
  if(!is.null(m)) {           # Checks to see if the inverse has already been calculated  
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
