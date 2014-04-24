## The file contains two functions 'makeCacheMatrix' and 'cacheSolve'
## 
## 'makeCacheMatrix' creates a matrix with further specific functions to store
## inverse of the matrix to the parent environment, making it avaialble to other
## functions as well.
## 
## 'cacheSolve' finds inverse of a matrix created through the above mentioned 
## function. It checks if a cached copy of the inverse already exists and
## returns it. Otherwise it calculates the inverse of matrix and stores it back
## with the object.

## 'makeCacheMatrix' initializes an empty matrix or with the specified data and
## initializes inverse to NULL. It contains further two methods to set or get
## the matrix data and two functions to set or get the matrix inverse. At the
## time of setting matrix data it sets inverse back to NULL

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize inverse to NULL
  inverse <- NULL
  
  # sets the value of the variables
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  #get matrix data
  get <- function() x
  
  #sets the value of inverse
  setinverse <- function(sInverse) inverse <<- sInverse
  
  #get value of inverse
  getinverse <- function() inverse
  
  #return list of methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## 'cacheSolve' takes an object created through the 'makeCacheMatrix' function 
## It then checks if the inverse has already been calculated through
## 'makeCacheMatrix$getInverse' function. If cached inverse is found, it is
## returned or else it calculates the inverse, sets it through
## 'makeCacheMatrix$setInverse' function back to the object and returns the
## inverse
##
## The function takes an option 'force' flag to force calculatio of inverse
## despite cache version availability just to check the time it takes to
## calculate the inverse all over again

cacheSolve <- function(x, force=FALSE,...) {
  # Take system time to see how fast the routine executes
  ptm <- proc.time()
  
  # Check if a cached inverse already exists
  inverse <- x$getinverse()
  if( !is.null(inverse) & force==FALSE ){
    message("getting cached data")
    
    # Print how long it took to execute the processing
    print(proc.time() - ptm)
    
    #return cached inverse
    return (inverse)
  }
  # If cached version is not found, calculate inverse, set it, and return
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  
  # See how long it took to execute the function
  print(proc.time() - ptm)
  inverse
}
