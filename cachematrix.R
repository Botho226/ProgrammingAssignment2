## The functions below create a matrix and computes its inverse
## 

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
    

}


## the function below computes the inverse of the matrix. If
## the inverse has already been calculated, the it returns the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setInv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

