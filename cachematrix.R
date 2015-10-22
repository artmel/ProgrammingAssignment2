# 2 functions for making and using cached inverse matrices

#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the matrix
#get the matrix
#set the inverse of the matrix
#get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cache the inverse of the matrix by utilizing the makeCacheVector
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Testing

# create invertable matrix
# calculate inverse of matrix
# and cache the result
m<-matrix(runif(9),3,3)
m
z<-makeCacheMatrix(m)
cacheSolve(z)
# Should output first 'getting cached data'
# followed by the inverse matrix
cacheSolve(z)