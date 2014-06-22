## Overall goal is to have functions that cache the inverse of a matrix

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {
        ## m will flag if we have stored the inverse in cache
        ## default value is NULL
        m <- NULL
        set <- function(y) {
                ## Use <<- to store in cache
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Check if the value of the matrix inverse is already stored in cache.
        if(!is.null(m)) {
                message("getting cached data")
                ## Returns value of matrix inverse
                return(m)
        }
        ## If the value of the matrix inverse is not stored in cache, compute it.
        data <- x$get()
        ## Use the solve command to generate a matrix inverse
        m <- solve(data, ...)
        x$setinverse(m)
        ## Returns value of matrix inverse
        m
}
