## My functions creates an object which is a closure environment that has a list contains 4 other functions
## and the associate variables x (which stores the matrix) and i (which stores x'inverse)

## This function creates an object, which is a closure environment, that has a list of 4 functions
## and the variables x (for matrix) and i (for x inverse)

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


## This function takes in argument object x (a closure environment), check to see within that environment,
## the inverse of the matrix that was put into x already exists. If it does, load it. If not, set the inverse of x
## into its environment

cacheSolve <- function(x, ...) {
  
  ## load the inverse matrix within the closure environment of object x
  i <- x$getinv()
  
  ## if the inverse is there (not NULL), return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## if not, solve for inverse of the matrix of object x, and put it into x's environment
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
