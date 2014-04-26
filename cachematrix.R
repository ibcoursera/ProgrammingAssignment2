## cachematrix.R improves efficiency by caching the inverse of a given matrix
## The function checks to see if the inverse of a given matrix was previously cached
## If yes, return cached inverse
## If no, calculate the inverse and cache the result

## makeCacheMatrix creates a special matrix object
## which is a list containing 4 functions
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) invert the matrix
## (4) set the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  matrixinv <- NULL
  
  set <- function(y){
    x <<- y
    matrixinv <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInverse <- function(inverse){
    matrixinv <<- inverse
  }
  
  getInverse <- function(){
    matrixinv
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks to see if the inverse of the matrix already exists
## If yes return matrix inverse
## If no invert matrix using the solve() and return the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixinv <- x$getInverse()
  
  if(!is.null(matrixinv)){
    message("getting cached data")
    return(matrixinv)
  }
  
  data <- x$get()
  matrixinv <- solve(data)
  x$setInverse(matrixinv)
  matrixinv
}
