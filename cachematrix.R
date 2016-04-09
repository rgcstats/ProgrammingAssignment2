## Put comments here that give an overall description of what your
## functions do

## makeVector creates a special vector which is really a list, to
##  set/get the value of the vector and set/get the value of the mean
##  (cut and pasted from https://github.com/rdpeng/ProgrammingAssignment2)

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## cachemean calculates the mean of the special "vector" created by makeVector.
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the
## mean in the cache via the setmean function.
##  (cut and pasted from https://github.com/rdpeng/ProgrammingAssignment2)

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

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
