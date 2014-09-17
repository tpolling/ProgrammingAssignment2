# makeCacheMatrix and cacheSolve work to avoid computing the inverse of
# a square matrix if the inverse has already been computed once.
# Create a matrix cache wrapper by means of makeCacheMatrix.
# cacheSolve gets the inverse of the value and only performs the matrix
# inversion if it has not been already computed for the current martrix
# value in the cache wrapper.
# Setting a new matrix in the cache wrapper will clear out any before
# evaluated inverse matrix value.
    
# Get a cache wrapper for the given matrix x. The cached matrix allows
# to cache the inverse value of the given matrix x.
# Functions for the matrix cache wrapper
# - get: get the cached matrix
# - set: clear the cached matrix and inverted value and set a new matrix
# - getInverse: get the inverse of the matrix. This may be a cached value
# - setInverse: set the inverse of the matrix value.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    get <- function() x
    set <- function(aMatrix) {
        x <<- aMatrix
        inverseMatrix <<- NULL
    }
    getInverse <- function() inverseMatrix
    setInverse <- function(theInverseMatrix) inverseMatrix <<- theInverseMatrix
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

# Get the inverse of the square matrix m. solve(m) is peformed if the inverse
# has not been cached already.
# x is a cache wrapper to a matrix. Call makeCacheMatrix to get the wrapper
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    
    ## If there is no cached inverse, evaluate it and update the cache
    if (is.null(inverseMatrix)) {
        message("inverse matrix is not cached - reckoning inverse and caching")
        matrix <- x$get()
        inverseMatrix <- solve(matrix)
        x$setInverse(inverseMatrix)
    }
    
    # Return the cached inverse matrix value
    inverseMatrix
}
