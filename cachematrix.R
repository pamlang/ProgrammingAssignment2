## This function handles a list of functions that will check if the inverse
## of a matrix has been calculated.  If it has been calculated, it will 
## pull it from the cache.  If not, it will cacluate the inverse and return
## result and store it in cache.  This assumes that it is square matrix

## makeCacheMatrix creates a list of functions that stores and retrieves
## the matrix and inverse from cache

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(mx) {
                x <<- mx
                minverse <<- NULL
        }
        get <- function() x           
        setinverse <- function(solve) minverse <<- solve
        getinverse <- function() minverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve uses the functions above to check if the inverse is in cache
## and if not it will calculate it and use the fns above to store

cacheSolve <- function(x, ...) {
        minverse <- x$getinverse()
        if (!is.null(minverse)) {
                message("Retrieving cached inverse...")
                return(minverse)
        }       
        message("Calculating and caching new inverse..")
        mm <- x$get()
        invmatrix <- solve(mm)
        x$setinverse(invmatrix)
        invmatrix
}

