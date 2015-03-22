## There are 2 Main functions:
##1. makeCacheMatrix : This creates a special object used for matrix inversion
##2. cacheSolve : This function is called for cached matrix inversion
##
##

##makeCacheMatrix creates a special Matrix object, which is
##really a list containing a function to

##1.  set the value of the  matrix
##2.  get the value of the  matrix
##3.  set the value of the matrix inverse
##4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve solves for the inverse of a square matrix
## if the result is already cached, return cached value directly
## else need to solve for inverse using the solve function
## and cache the result

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
