## This is my solution for Coursera rprog-005 programming assignment #2.
## For more information, see https://class.coursera.org/rprog-005/human_grading/view/courses/972576/assessments/3/submissions.

## The following is an implementation of a matrix inverse function which keeps its 
## results cached.  The caller will first pass a matrix to the 'makeCacheMatrix' function
## which will return a special list with get/set functions to access the matrix and/or
## caclulated results.

## Sample usage:
##    > v <- makeCacheMatrix(matrix(2))
##    > cacheSolve(v)
##    [,1]
##    [1,]  0.5
##    > cacheSolve(v)
##    getting cached data
##    [,1]
##    [1,]  0.5


## makeCacheMatrix returns a special function list plus a copy of the matrix passed to us.
##   This result list can then be used in other functions to access either the original
##   matrix or the calculated results of a matrix operation.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mat) m <<- mat
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'.
##   Note that 'x' is a special list created by 'makeCacheMatrix'.
cacheSolve <- function(x, ...) {
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
