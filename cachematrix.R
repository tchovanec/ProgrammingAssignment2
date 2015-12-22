## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inversematrix <- NULL
  set <- function(y) {
    x <<- y 
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inversematrix <<- inv
  getinverse <- function() inversematrix
  list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The following function return the inverse of a matrix
# The first thing the function does is check to see if the inverse has already be computed
# If it has it returns a message along with the cached data. 
# If it has not already been computed it then computes the inverse using the solve function, sets the value in the cache
# via the setinverse function
cacheSolve <- function(x, ...) {
   
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data)
  x$setinverse(inversematrix)
  
## Return a matrix that is the inverse of 'x'
  inversematrix
}
