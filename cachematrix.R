## Put comments here that give an overall description of what your
## functions do
## Adapted the example code for the assignment.
## The makeCacheMatrix function will take a matrix as an input,
## inverse that matrix with the solve function, and then store
## that inverse into a vector called 'setinverse'. The result of
## makeCacheMatrix should be stored in a separate vector so that
## it can be called upon by cacheSolve.
## The cacheSolve function will take the result of makeCacheMatrix
## and return the inverse of the matrix originally cached by 
## makeCacheMatrix.

## The first function follows the same routine as in the example:
## It establishes the objects x and m, where x is an argument and m
## in an object defined within the environment of makeCacheMatrix.
## It sets m to be null initially.
## The get function remains defined as in the example, it is defined
## within the environment of the function and can be called upon by
## the cacheSolve function.
## The setmean function has been changed to setinverse. Instead of 
## the mean function it now uses the solve function to calculate the
## inverse of an invertible square matrix.
## The list section has been updated, such that setmean and getmean
## are now called setinverse and getinverse.

## Write a short comment describing this function

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


## Write a short comment describing this function
## cacheSolve will take the result of makeCacheMatrix
## as its argument. It will then call upon the 
## getinverse function within makeCacheMatrix and assign
## it to m. It will then check if m is null. If it is not,
## it will return m. If it is, it will call upon the get
## function within makeCacheMatrix and assign its value to
## data. Then it will run the solve function on data, thus
## finding the inverse of the matrix, and store the result
## in m. After that, it will call on the setinverse function
## in makeCacheMatrix, taking m as an argument, so that the
## value is cached for future use.

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
