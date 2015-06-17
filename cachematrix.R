## These 2 functions allows to cache a matrix inverse to avoid computing it 
## several times (e.g. in a loop).


## makeCacheMatrix creates a list of functions used to cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Initiate the inverse matrix
  inv <- NULL
  
  ## Build the set, get, setinverse and getinverse functions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  ## Return the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve checks if the inverse is cached, otherwise calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Check first if the inverse is cached
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise solve the matrix, store the inverse in cache and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
