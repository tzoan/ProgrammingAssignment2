
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             ##initialize inv as NULL
  set <- function(y) {                    ##define the set function to assign new 
    x <<- y                             
    inv <<- NULL                        ##if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     ##returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinverse() 
  if(!is.null(inv)) { ##if inv is not NULL (it has been calculated before)
    message("getting cached data")  ## get the inverse from cache
    return(inv)
  }
  data <- x$get()   ## else calculate the inverse and save it to cache
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
