## ================================================================================
##   OVERVIEW
##   The functions contained in this file calculate & cache the inverse of a matrix
##   This ensures that the inverse only needs to be calculated once
##
##   EXAMPLE USAGE
##   source("cachematrix.R")
##   myMatrix <- matrix(rnorm(10000), 100, 100)
##   cacheMatrix <- makeCacheMatrix(myMatrix)
##
##   ## first run creates and caches a new matrix
##   result <- cacheSolve(cacheMatrix)
##   print(result[1:5,1:5])
##
##   ## future runs return cached matrix
##   result <- cacheSolve(cacheMatrix)
##   print(result[1:5,1:5])
## ================================================================================

## --------------------------------------------------------------------------------
##   makeCacheMatrix: Creates a "matrix" object that can store its inverse
## --------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix())
{
    ## initialize the inversed matrix to null
    cachedInverse <- NULL
    
    ## set: caches matrix and clears stored inverse
    set <- function(y)
    {
        x <<- y
        cachedInverse <<- NULL
    }
    
    ## get: returns the cached matrix
    get <- function() x
    
    ## setinverse: saves the inverse passed to it
    setinverse <- function(inverse) cachedInverse <<- inverse
    
    ## getinverse: returns the cached inverse
    getinverse <- function() cachedInverse
    
    ## makeCacheMatrix returns a list that exposes the sub-function calls
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## --------------------------------------------------------------------------------
##   cacheSolve:
##       returns the cached inverse, if previously calculated
##       otherwise, calculates the inverse, caches it and returns it
## --------------------------------------------------------------------------------

cacheSolve <- function(x, ...)
{
    ## retrieve the currently cached inverse
    i <- x$getinverse()
    
    ## if cached version is null, calculate & store the inverse
    if (is.null(i))
    {
        message("generating new inverse")
        
        ## retrieve the saved matrix
        cachedMatrix <- x$get()    
    
        ## generate the inverse & cache it for future queries
        i <- solve(cachedMatrix, ...)
        x$setinverse(i)
    }
    else
    {
        message("returning cached inverse")
    }
    
    ## return the inverted matrix
    return(i)
}
