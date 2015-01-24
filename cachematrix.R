## The functions are similar to that used for the mean of the vector.
##  I only adapted to the inverse matrix case

## makeCacheMatrix takes as input a matrix and returns a list of three functions
## that will be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 

	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## cacheSolve takes as input the output of the function makeCacheMatrix
## applied on the matrix
## In a first step, cacheSolve checks if the matrix has always been inverted in the past. If yes, then cacheSolve returns a message "getting cached data" and returns the spreviously set value.
## If not, then cacheSolve computes the inverse of the matrix and sets it in the makeCacheMatrix of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
