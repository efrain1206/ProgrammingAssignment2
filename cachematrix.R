## Below two functions set a matrix object and caches its inverse.
## To create this special matrix m you execute \code{m <- makeCacheMatrix(x)}
## You can get the inverse with \code{cacheSolve(m)}.


# Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Below function sets the value of the matrix and clears the old
  # inverse from the cache
  set <- function(y) {
    x <<- y    
    m <<- NULL
  }
  # Below function gets the value of the matrix
  get <- function() x
  # Below function sets the inverse
  setInverse <- function(inverse) m <<- inverse
  # Below function gets the inverse
  getInverse <- function() m
  
  # Return a list with the above four functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Below function computes the inverse of the "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.
cacheSolve <- function(x) {
  m <- x$getInverse() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If the cache was empty we need to calculate it, cache it, and then return it.
  data <- x$get()  
  m <- solve(data) # Calculate inverse
  x$setInverse(m)  # Cache the result
  m                # Return the inverse
}