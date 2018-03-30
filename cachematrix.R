## makeCacheMatrix function creates a special matrix object getInverse that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  get <- function() x 
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list( get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function solves for the inverse of the matrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) 
  x$setInverse(i)
  i ## Return a matrix that is the inverse of 'x'
        
}
