## MakeCacheMatrix gets an invertible square matrix. If no input has been passed 
## to it, it creates such matrix as defalut and starts processing on it.
## MakeCacheMatrix creates a list as output. If this output is fed into casheSolve
## function, then it checks if the inverse of out matrix has been calculated.
## If has already been calculated, it loads the information form cache. 
## If not, the input matrix is read and stored in data variable. then the inverse 
## of the data is calculated and stored in Minverse which gets returned finally.

## This function gets a matrix and creates a special list as output.

makeCacheMatrix <- function(q = matrix(c(2,2,3,2), nrow=2, ncol=2)) {
    Minverse <- NULL
    set <- function(p) {
      q <<- p
      Minverse <<- NULL
    }
    get <- function() q
    setinverse <- function(inverse) Minverse <<- inverse
    getinverse <- function() Minverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }  


## This function gets the special list created by above function and computes 
## the inverse of its input. If we already have the inverse matrix, it get loaded
## from cache otherwise it gets computed.

cacheSolve <- function(q, ...) {
  
  Minverse <- q$getinverse()
  if(!is.null(Minverse)) {
    message("getting cached data")
    return(Minverse)
  }
  
  data <- q$get()
  ## Return a matrix that is the inverse of 'x'
  Minverse <- solve(data, ...)
  q$setinverse(Minverse)
  Minverse
  
}
