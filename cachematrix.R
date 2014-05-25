## Below functions can calculate the inverse of the matrix and cache the result.  
## This cached result can be re-used if the inverse logic is to be computed repeatedly


## This function will cache the inverse of the matrix result
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    inv <<- NULL
    
  }
  
  get <- function() x
  setSolve <- function(inverse)  inv <<- inverse
  getSolve <- function() inv
  
  list(set = set, get = get
       , setSolve = setSolve, 
       getSolve = getSolve)
}


## This function will compute the inverse of the matrix
cacheSolve <- function(x, ...) 
{
  ## Get matrix inverse from buffer
  m <- x$getSolve()
  
  if (!is.null(m)) {
    message("Getting Cache data")
    return(m)
  }  
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setSolve(m)
  
  m
  
}