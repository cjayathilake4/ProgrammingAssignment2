#this function calculates the inverse of a matrix efficiently

## define the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) { 
  ## declaring the variable inv to store the inverse matrix later  
  inv <- NULL
  ## defining the set function to set the matrix
  set <- function(y) {
    ##matrix value in the parent environment
    x <<- y  
    ## reset inv to NULL
    inv <<- NULL                        
  }
  
  ## define the get function to get the value of the matrix
  get <- function() x 
  
  ## setting the value of inv in parent environment
  setinverse <- function(inverse) inv <<- inverse
  ## getting the value of inv
  getinverse <- function() inv  
  ## returning the defined functions in a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## cacheSolve function to compute the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Referring to the getinverse function defined in the previous function
  inv <- x$getinverse()
  ##to check whether the inv is already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##referring to get function defined in the previous function
  data <- x$get()
  ##calculating the inverse of the matrix and storing it in the inv 
  inv <- solve(data, ...)
  ##setting the inverse value
  x$setinverse(inv)
  ##final output
  inv
}
