## The function below creates a special "matrix" object that can cache its inverse.
## Key Assumption: the argument provided to makeCacheMatrix is invertible

makeCacheMatrix <- function(m = matrix()) {
        
        inv = NULL ## initializes the matrix inverse as an object in the makeCacheMatrix environment
        
        set = function(y) {
          x <<- y ## assigns new argument to makeCacheMatrix function via lexical scoping rules.
          inv <<- NULL ## resets 'inv' object so that matrix inverse is recomputed in makeCacheMatrix
                       ## with the new argument.
        } 
          
        get <- function() m ## calls the current matrix argument in makeCacheMatrix function
        
        setinv <- function(inverse) inv <<- inverse ## calls the matrix inverse without having to recompute
        
        getinv <- function() inv ## calls the current matrix inverse computed by the makeCacheMatrix function
        
list(set = set, get = get, setinv = setinv, getinv = getinv)
## creates and names a list of setters and getters for later use. This is the cache. Elements in the list
## are named to enable partial matching of list elements.

  
## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.    

cacheSolve <- function(m, ...)
  
  inv <- m$getinv() ##calls the matrix inverse from the list of elements in the function above.

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } ## if the inverse has already been computed, then return the computed value

## If the inverse has not been computed, then compute the value by first retrieving the current matrix object
  data <- m$get()
  
  inv = solve(data, ...) ## compute the matrix inverse
  
  m$setinv(inv) ## cache the newly computed matrix inverse in the list of elements in the makeCacheMatrix fn.
  
  return(inv) ## return the newly computed matrix inverse to end the cacheSolve function.

}