## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {## define the argument with default mode of "matrix"
    inver <- NULL                             ## initialize inver as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
      x <<- y                             ## value of matrix in parent environment
      inver <<- NULL                        ## if there is a new matrix, reset inver to NULL
    get <- function() x                     ## define the get function - returns value of the matrix argument
    setinversed <- function(inversed) inver <<- inversed  ## assigns value of inver in parent environment
    getinversed <- function() inver                     ## gets the value of inver where called
    list(set = set, get = get, setinversed = setinversed, getinversed = getinversed)  
  }

}


## Write a short comment describing this function
## This function will compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inver <- x$getinversed()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinversed(inver)
  inver## Return a matrix that is the inverse of 'x'
}
