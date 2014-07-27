## makeCacheMatrix constructs a matrix object, a cache variable for storing the inverse of the 
## matrix, and method functions for setting and getting the matrix data and cached inverse.
## cacheSolve returns the inverse of matrix data stored in makeCacheMatrix, either from cache
## (if matrix data unchanged), or computed (and cache updated)


## Function makeCacheMatrix creates a matrix object (referenced by x), a local reference for caching the
## inverse of the matrix (matrix_inverse), with functions for setting and getting both the 
## matrix data and the matrix inverse. Returns a list including references to the four set/get functions

makeCacheMatrix <- function(x = matrix()) {
    ## x is a matrix object
    ## matrix_inverse references the inverse of the matrix, initialized to NULL
    matrix_inverse <- NULL
    
    ## set function receives reference to a matrix, stores the matrix data in x,
    ## and sets matrix_inverse to NULL
    set <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
    }
    
    ## get function returns the matrix
    get <- function() x
    
    ## set_inverse function stores reference to inverse of matrix in matrix_inverse
    set_inverse <- function(inverse) matrix_inverse <<- inverse
    
    ## get_inverse function returns the cached inverse of the matrix
    get_inverse <- function() matrix_inverse
    
    ## create list object containing references to setters and getters
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function
## Function cacheSolve accepts a reference to a makeCacheMatrix instance, checks to see if the inverse
## of the matrix is already cached, or, if not, calculates and stores the inverse of the matrix into
## the cache (referenced by matrix_inverse) of makeCacheMatrix. Returns the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## retrieve the reference to the cache of matrix inverse
    matrix_inverse <- x$get_inverse()
    
    ## if the returned reference is not NULL, then return the cached matrix inverse
    if(!is.null(matrix_inverse)) {
        message("getting cached data")
        ## use return to return the cached matrix inverse and skip rest of function
        return(matrix_inverse)
    }
    
    ## if the returned reference is NULL, then execute the following code to calculate
    ## and store the inverse of the matrix
    
    ## retrieve the stored matrix data using get
    data <- x$get()
    
    ## use the solve() function to compute the inverse of the matrix
    matrix_inverse <- solve(data, ...)
    
    ## store the inverse of the matrix in the cache using set_inverse() function
    x$set_inverse(matrix_inverse)
    
    ## return the inverse of the matrix
    matrix_inverse
}
