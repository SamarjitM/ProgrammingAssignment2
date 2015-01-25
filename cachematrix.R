## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## This function takes a matrix as an argument  and returns a list of functions
## to set/get matrix and set/get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function checks if input matrix inverse has already been calculated or not 
## If value available in cache retrives it else calculates
## If matrix is changed it calculates value without going to cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("Found Inverse of input Matrix cached. Retrieving cache data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data) ## Returns the inverse of the input matrix
  x$setinverse(m)
  m
}
