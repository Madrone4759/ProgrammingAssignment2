## two functions to calcualte the inverse of a matrix and store to the 
## cache for future use

## creates a list to set and retrieve matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    invrs = NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) invrs <<- inv
    getinverse <- function() invrs
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse )
}


## takes a matrix, returns the cached inverse matrix. If no value is cached, 
## then this calculates the inverse, returns it, and saves it to the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
