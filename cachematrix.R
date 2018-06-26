## This function creates a special matrix, which is a list containing functions 
## to set and/or get a matrix, as well as set and/or get its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## The following function calculates the inverse of a given special matrix.
## If it is already calculated, it will cache the results and return. Otherwise, 
## it will take the matrix as inputs, calculated its inverse and return the 
## results.

cacheSolve <- function(x, ...) {
    
    ## Get the inverse of the provides special matrix:
    m <- x$getinverse()
    
    ## If the inverse is provided, then return it:
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If no inverse is provides, take the matrix, calculate its inverse:
    data <- x$get()
    m <- solve(data)
    
    ## Set the results in the special matrix:
    x$setinverse(m)
    
    # return the inverse:
    return(m)
}
