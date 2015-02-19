## Example: 
## x<-makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## x$get() %*% cacheSolve(x)

## A Function that creates a special  "matrix object" that stores a matrix itself,
## its inverse ,and four methods/functions for getting access to the matrix and 
## its inverse. The four methods are set, get, getinverse and setinverse. 

## The "solve" function has been used to calculate the inverse of the matrix m

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


## It's the function which decides what function of the object created in 
## "makeCacheMatrix" to use. If the inverse of a particular matrix has been
## calculated once, "cacheSolve" executes "solve" and stores the data 
## otherwise get the matrix with x$getinverse. 

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
