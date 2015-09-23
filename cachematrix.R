## This set of functions is designed to optimize the calculation of 
## the inverse of a matrix. One function creates a matrix object
## that holds the matrix and caches its inverse. The other function returns 
## the inverse of the matrix and calculates it only if not in the cache.

## makeCacheMatrix creates a list of helper matrixk functions:
## set: populates the matrix
## get: returns the matrix
## setinverse: populates the inverse of the matrix, else it is NULL
## getinverse: returns the inverse of the matrix or NULL if not yet set

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        
        set <- function(somematrix) {
                x <<- somematrix ## stores matrix in external variable x
                mi <<- NULL  ## resets the inverse to not calculated
        }
        
        get <- function() x
        
        setinverse <- function(matrixinversed) { ## caches inverse matrix
                mi <<- matrixinversed          ## in external variable mi
        }
        
        getinverse <- function() mi  ## returns cached inverse matrix
        
        list(set = set, get = get, getinverse = getinverse,
             setinverse = setinverse)
}


## cacheSolve takes a makeCacheMatrix object as argument and returns
## its inverse. The inverse is calculated only if the cached inverse
## matrix is not set (NULL value).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        xi <- x$getinverse()
        
        if( !is.null(xi) ) { 
                message("Getting cached data")
                return(xi) ## returns cached inverse matrix
        }
        mdata <- x$get()    
        xi <- as.matrix( solve(mdata, ...) ) ## calculates the inverse matrix
        x$setinverse(xi)
        xi
}
