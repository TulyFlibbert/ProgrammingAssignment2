## Return the inverse of a matrix by either 
## returning a cached inverse matrix or 
## computing the inverse using the solve function


## Create a list of functions to set the value of a matrix, get a cached value of a matrix,
## set the value of an inverse matrix, and get the cached value of an inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calls the list of cached inverse matrices and checks for an existing entry, 
## otherwise returns the computed value and adds it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
