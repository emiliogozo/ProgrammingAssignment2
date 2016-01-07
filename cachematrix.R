## This contains a function that solves the inverse of a matrix
## that has a capacity to cache the result to be able to retrieve
## it when needed.

## This creates a special "matrix" by creating a list of 
## four functions:
##     (1) set the value of the matrix
##     (2) get the value of the matrix
##     (3) set the value of the matrix inverse
##     (4) get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	setinverse <- function(res) xinv <<- res
	getinverse <- function() xinv
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This computes the inverse of the special "matrix" returned by
## makeCacheMatrix then saves it in the cache. If the inverse has
## already been calculated, then it returns the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	xinv <- x$getinverse()
	if(!is.null(xinv)) {
		message("getting cached data")
		return(xinv)
	}
	data <- x$get()
	xinv <- solve(data, ...)
	x$setinverse(xinv)
	xinv
}
