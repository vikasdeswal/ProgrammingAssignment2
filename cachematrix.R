
## This function has getter and setter methods for the input matrix and the 
## inverted matrix. Note that the both the original and inverted matrices are 
## set to the global invironment using supperassignment operator (<<-). 
## Doing this preserves the inverted matrix in the global environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
 
  ## This function is to set the original matrix to variable x with a global scope
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## This function is to fetch the original matrix
  get <- function() x
  
  ## This function sets the inverse of the matrix supplied as an argument to the 
  ## inv variable. This variable is stored in global environment using 
  ##superassignment operator (<<-)
  setInvrs <- function(invrs) inv <<- invrs
  
  ## inverted matrix is returned
  getInvrs <- function() inv
  list(set = set, get = get,
       setInvrs = setInvrs,
       getInvrs = getInvrs)
}



## This function inverts the matrix and saves it to global enviroment using 
## the setter function of makeCacheMatrix(). Check is performed to see if the  
## set to the global invironment using supperassignment operator (<<-). 
## Doing this preserves the inverted matrix in the global environment.

## This method takes an object of makeCacheMatrix. 
cacheSolve <- function(x, ...) {
  
  ## getInvrs method is envoked to fetch the cached inverse of the matrix.
  inv <- x$getInvrs()
  
  ## In case, the cached inverse exists, it is returned. 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## data contains the original matrix 
  data <- x$get()
  
  ## Inverse of the matrix is calculated in case it is not already calculated and cached.
  inv <- solve(data, ...)
  
  ## Inverse thus calculated in the step above is cached to global environment using the 
  ## setInvrs method of makeCacheMatrix(). 
  x$setInvrs(inv)
  inv
  
}


