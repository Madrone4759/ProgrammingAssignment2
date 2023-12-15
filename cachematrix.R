## two functions to calculate the inverse of a matrix and store to the 
## cache for future use

## creates a list to set and retrieve matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    invrs = NULL
    
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    } #set matrix to input variable
    
    get <- function() x
    setinverse <- function(inv) invrs <<- inv #update cached inverse
    getinverse <- function() invrs #output inverse from cache
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse )
}


## takes a matrix, returns the cached inverse matrix. If no value is cached, 
## then this calculates the inverse, returns it, and saves it to the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    } #return inverse from cache, if available
    origMatrix <- x$get()
    inverse <- solve(origMatrix, ...) #compute inverse matrix
    x$setinverse(inverse)
    inverse
}
