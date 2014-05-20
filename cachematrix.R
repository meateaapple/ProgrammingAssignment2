## Put comments here that give an overall description of what your
## functions do

	## The first function generates four functions around some input matrix. These four functions work together to initialize a global variable, inv, which will cache the inverse matrix that is created by the second function. When the second function is called a second time, it will see that the value of the inverse matrix has been cached, and rather than re-calculating it, it will get the value from the cache and return it.
	## the second function takes a list of the four created functions as input.

## Write a short comment describing this function
	## This function creates four other functions which together allow you to cache a value. All this function does is create the other functions; it doesn't call any of them.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
	## When this function is called, it checks to see if there is already a cached value for the inverse of the matrix. It does this by calling getInv, which was defined in the first main function (getInv is in the list of functions which is passed to cacheSolve as input). If there is a value for inv already, it looks up that value and returns it; otherwise, it computes the inverse of the matrix, and then uses setInv (also on the list) to set that value as a global variable which contains the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}
