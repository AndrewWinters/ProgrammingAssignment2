# This code is part of the Coursera R programming assigmnent. It contains two functions
# that jointly allow to cache outputs of inverse calculations in memory and retrieve the
# cached value instead of re-doing the calculation again.


# This function creates a list of functions applied to a matrix for which the inverse needs
# to be calculated.  Syntax FunctionList<-makeCacheMatrix(InputMatrix)

makeCacheMatrix <- function(InputMatrix = numeric()) {
    inv <- NULL
    # Function that sets the input matrix and clears cached inverse.
    set <- function(y) {
        InputMatrix <<- y
        inv <<- NULL
    }
    # Function that reports the underlying input matrix for the list
    get <- function() InputMatrix
    # Function that Caches the caluclated inverse in parent environment
    setinverse <- function(CalculatedInverse) inv <<- CalculatedInverse
    # Function that reports the calculated inverse for the input matrix
    getinverse <- function() inv
    # Generates the output of the makeCacheMatrix as a whole - a list vector of four functions
    # above linked to the underlying data in InputMatrix used.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
# This function takes the FunctionList (list of functions generated above) as the input and
# reports the inverse of the InputMatrix. If the inverse has already been calculated
# before, the function reports its cached value. If there is not cached value the function
# performs the calculation and stores the result.

cacheSolve <- function(FunctionList, ...) {
    # Check the cache for stored inverse value.
    inv <- FunctionList$getinverse()
    # Report the inverse if it is stored in cache.
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise calculate the inverse for the matrix linked to the FunctionList...
    data <- FunctionList$get()
    inv <- solve(data, ...)
    # ...save the inverse in cache...
    FunctionList$setinverse(inv)
    # ...and report it.
    inv
}