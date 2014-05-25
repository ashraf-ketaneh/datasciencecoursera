## This file contains tow functions: makeCacheMatrix and  cacheSolve which creates a special
## matrix and cache it is inverse and computes the inverse of this matrix in chacheSolve function.

# makeCacheMatrix: return a list of functions to:
# 1- Set the value of the matrix
# 2- Get the value of the matrix
# 3- Set the value of the inverse
# 4- Get the value of the inverse
makeCacheMatrix <- function(A = matrix()) {
        # store the cached inverse matrix into Inverse
        Inverse <- NULL
        
        # Set the value of the matrix
        set <- function(B) {
                A <<- B
                Inverse <<- NULL
        }
        # Get the value of the matrix
        get <- function() A
        
        # Set the value of the inverse
        setInverse <- function(inverse) Inverse <<- inverse
        # Get the value of the inverse
        getInverse <- function() Inverse
        
        # Return the matrix.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(A, ...) {
        Inverse <- A$getInverse()
        
        # If the inverse is already calculated, return it
        if (!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        
        # The inverse is not yet calculated, so we calculate it
        myData <- A$get()
        Inverse <- solve(myData, ...)
        
        # Cache the inverse
        A$setInverse(Inverse)
        
        # Return it
        Inverse
}
