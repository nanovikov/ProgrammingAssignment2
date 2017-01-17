# Programming Assignment 2
# -----------------------------
# Goal: Cache the inverse of an invertible matrix
# 
# makeCacheMatrix() is a function that creates a special "matrix" object that
# can cache its inverse

makeCacheMatrix <- function(m = matrix()) {  ## initializes an empty matrix
  inv_of_m <- NULL                           ## initializes a NULL object
  
  set <- function(m_new) {   ## sets a new matrix to evaluate
    m <<- m_new              ## re-assigns the matrix object
    inv_of_m <<- NULL    ## resets the inverse matrix object
  }
  
  get <- function() m        ## returns the matrix object
  
  setinverse <- function(new_inverse) {
    inv_of_m <<- new_inverse ## re-assign the inverse matrix
  }
  
  getinverse <- function() inv_of_m ## returns the matrix inverse object
  
  list( set = set,           ## returns a list of functions 
        get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

# cacheSolve() is a function that computes the inverse of a special "matrix"
# returned by the makeCacheMatrix function. If the inverse has already
# been calculated, then the cachesolve should retrieve the inverse from
# the cache

cacheSolve <- function(x) {
  inv_of_m <- x$getinverse()   ## get the inverse from makeCacheMatrix
  if(!is.null(inv_of_m)) {     ## if the inverse has already been calculated
    message("getting cached data")  ## then retrieve the cached data
    return(inv_of_m)
  }
  data <- x$get()              ## otherwise retrieve the matrix 
  inv_of_m <- solve(data)      ### calculate the inverse matrix
  x$setinverse(inv_of_m)       ### cache the inverse matrix
  inv_of_m                     ### return the inverse matrix
}


