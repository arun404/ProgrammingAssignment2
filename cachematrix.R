## Below functions can calculate the inverse of the matrix and cache the result.  
## This cached result can be re-used if the inverse logic is to be computed repeatedly


## This function can be used to cache the inverse of the matrix result
## Function formal argument input is a matrix 
## Output is a list with result from 4 other defined functions 
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
  m_inv <- x$getSolve()
  
  ## if matrix inverse exists in the buffer
  if (!is.null(m_inv)) {
    message("Getting Cache data")
    return(m_inv)
  }  
  
  ## Call get() to return the original matrix
  data <- x$get()
  
  ## Compute matrix inverse
  m_inv <- solve(data)
  
  ## Set the matrix inverse value in inv object
  x$setSolve(m_inv)
  
  
  m_inv
  
}