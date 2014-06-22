## This is a set of functions to provide a matrix object
## with cached inverse calculation. makeCacheMatrix provides
## the inverse cachedobject. cacheSolve is the cached version
## of calculating the matrix inverse

## makeCacheMatrix makes a new object, optionally initialized
## with a provided matrix. It returns an object with functions
## to get/set the actual matrix, as well as to get/set the inverse
## of the stored matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y  # Updates the current matrix
                inv <<- NULL # Resets old inverse
        }
        get <- function() x
        setinv <- function(new_inv) inv <<- new_inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve , obtains the cached inv if any from the object
## if no inverse is available, i.e stored as NULL, inverse is
## calculated afresh, cached using setinv and returned

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
