## R Programming Assignment 2: Lexical Scoping

## The makeCacheMatrix function creates a special "matrix", which is 
## really a list containing function to 
## 1. set a value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y){
    x <<- y
    mat_inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) mat_inv <<- inverse
  getinverse <- function() mat_inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix"
## created with the above function. However, it schecks to see if the 
## inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache
## via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getinverse()
  if(!is.null(mat_inv)){
    message("getting cashed data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinverse(mat_inv)
  mat_inv
  
}
