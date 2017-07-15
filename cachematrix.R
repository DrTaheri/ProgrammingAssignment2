## Here two functions are made to cache the invere of a matrix. 

## This function takes a matrix "x" as the input argument.
## It returns a list (set, get, setinverse, getinverse)
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

  
  
  }


## Cach solve takes a matrix "x" as the input argument of the function
## and check if the inverse of matrix is already available in cache or not.
## if not, calculates it by "solve" function and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
