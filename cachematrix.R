## Functions allow users to store repeatable, complex operations in R so they can be called up and used as needed
## rather than having to retype them on the command line each time.

## The MakeCacheMatrix function creates a list storing a number of functions to set/get the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get, 
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## The cacheSolve function calculates the inverse of the matrix but first checks to see if the inverse has already been calculated and stored in the cache.
## If the inverse is already stored in the cache it skips the computation.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

}
