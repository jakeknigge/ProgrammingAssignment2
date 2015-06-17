## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function takes a matrix as an input and 
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      n <- NULL
      set <- function(y){
            x <<- y
            n <<- NULL
      }
      ## Assign value of x to get
      get <- function() x
      
      ## Compute matrix inverse
      setMatrixInv <- function(solve) n <<- solve
      
      ## Assign value of n to getMatrixInv variable
      getMatrixInv <- function() n
      
      ## Compile computations into a list
      list(set = set, get = get,
           setMatrixInv = setMatrixInv,
           getMatrixInv = getMatrixInv)
}


## Write a short comment describing this function

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.
      ## Note: If the inverse has already been calculated 
      ## (and the matrix has not changed), then 
      ## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## Assign list value of x$getMatrix() to n
      n <- x$getMatrixInv()
      
      ## If n is not NULL then display message and return
      ## the value of n
      if(!is.null(n)) {
            message("getting cached data")
            return(n)
      }
      
      ## Assign value of x$get (from list) to data variable
      data <- x$get()
      
      ## Invert data matrix
      n <- solve(data, ...)
      x$setMatrixInv(n)
      
      ## Return result
      n
}
