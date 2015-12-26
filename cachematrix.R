## The following functions cache the inverse of a matrix and return
## the object

## makeCacheMatrix is a function that creates a matrix object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setMatrix <- function(solve) m <<- solve
	getMatrix <- function() m

	list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## cacheSolve computes the inverse of the matrix returned by 
## makeCacheMatrix.  If the inverse has already been calculated then
## cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$makeCacheMatrix()
	if (!is.null(m)) {
		return(m)
	}
	
	myMatrix <- x$get()
	m <- solve(myMatrix, ...)
	x$setMatrix(m)
	m
}
