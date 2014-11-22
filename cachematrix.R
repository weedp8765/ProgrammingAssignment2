#### General Comments ####
## These two functions enable the storing of matrices along with their
## corresponding inverted matrix, calculated through the "solve" function,
## in cache. 
## 
## Important notes on usage:
## 1) These functions do *not* check for data types or the invert-ability 
##    of the input matrix. 
## 2) The makeCacheMatrix function *must* be called first to create a valid 
##    input for the cacheSolve function

## Notes on makeCacheMatrix function:
## This function takes a matrix variable as an input (ie, x), 
## saves it to cache, and establishes the functional methods to get and set 
## an associated cached inverted matrix (ie, the inv variable) via a 
## list that contains four functions to get and set the cached variables

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Notes on cacheSolve function:
## This function must take a list created via our makeCacheMatrix
## function above as input. It verifies whether or not the inverse (ie, inv) 
## has already been computed and stored in this cached list via the getinverse
## method. If it has, the cached calculation will be returned with confirmation 
## printed to the console. If not, it will call the function via the setinverse 
## method of the input list to compute/store the inverse of our cached matrix

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
