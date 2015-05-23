## Caching the Inverse of a Matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        ## 1. set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## 2. get the value of the matrix
        get <- function() x
	## 3. set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
	## 4. get the value of the inverse of the matrix
        getsolve <- function() m
	# return our list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## Given a makeCacheMatrix object, returns the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # retrieve from cache

        if(!is.null(m)) { # we got something...
                message("getting cached data")
                return(m)
        }

	# cache isn't filled yet
        data <- x$get()       # get it
        m <- solve(data, ...) # solve it
        x$setsolve(m)	      # set it
        m		      # return it
}
