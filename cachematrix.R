## These functions create a caching environment whereby a matrix's
##  inverse can be kept for posterity to avoid recalculating if it's
## already been done once.

## makeCacheMatrix creates a special matrix which is really a list, to
##  set/get the value of the matrix and set/get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve calculates the matrix inverse of the special "matrix" created by
## makeCacheMatrix. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the matrix inverse of x and sets the
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


########################################################################
## Code to illustrate the makeCacheMatrix and cacheSolve functions
########################################################################

# create a cached matrix
example.matrix <- makeCacheMatrix(matrix(data=c(4,2,-1,0.6,5,1,-2,0,8),
                                         nrow=3,ncol=3))

# take a look at the matrix
example.matrix$get()

# try to take a look at its inverse, but it isn't there because it hasn't
#   been calculated yet
example.matrix$getinverse()

# now calculate and cache its inverse
cacheSolve(example.matrix)

# when we use getinverse() it works this time:
example.matrix$getinverse()

# calculate inverse again, we see the message that the inverse is retrieved
#   from cache
cacheSolve(example.matrix)
