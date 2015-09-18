### makeCacheMatrix creates a special matrix object that
### has the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    get <- function()x
    setimatrix <- function(solve)m <<- solve
    getimatrix <- function()m
    list(set = set, get = get,
          setimatrix = setimatrix,
          getimatrix = getimatrix)
  }


### cacheSolve will either retrieve the inverse of your matrix
### in the list you created from makeCacheMatrix 
### or, if it has not been stored, it will solve for your inverse.

cacheSolve <- function(x, ...) {
    m <- x$getimatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setimatrix(m)
    m
  }