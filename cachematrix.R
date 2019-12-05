## These set of functions help compute and cache the inverse of a square matrix


## The makeCacheMatric function manages the cached storage of the original matrix and its companion inverse.
## It returns a list of functions that can be called to get/set the data it tracks.

makeCacheMatrix <- function(x = matrix()) {
    
    # start with an empty inverse
    inverse <- NULL
    
    # setter function to track the matrix
    set <- function(y) {
        # remember this new matrix
        x <<- y
        
        # since this is a new matrix, forget the old inverse that may have been computed
        inverse <<- NULL
    }
    
    # getter function to return the matrix when needed
    get <- function() {
        x
    }
    
    
    # setter function to track the inverse of the matric
    setInverse <- function(newInverse) {
        inverse <<- newInverse        
    }
    
    
    # getter function to return the inverse of the matrix when needed
    getInverse <- function() {
        inverse
    }
    
    list(get = get, 
         set = set, 
         getInverse = getInverse, 
         setInverse = setInverse)

}


## This function wraps the standard solve() function and adds the ability use a cached result
## or record one, if it doesn't already exist.

cacheSolve <- function(x, ...) {
    
        # see if we have cached a previously computed inverse.  If so, return it.
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        
        # we don't have an inverse already computed, so let's compute it.
        m <- x$get()
        inverse <- solve(m)

        # cache the result so we can reuse in the future        
        x$setInverse(inverse)
        
        # return the result
        inverse
        
}
