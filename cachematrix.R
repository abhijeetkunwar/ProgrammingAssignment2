# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	matrix_inverse <- NULL
  	set <- function(y) {
    	x <<- y
    	matrix_inverse <<- NULL
  	}
  	get <- function() x
  	set_matrix_inverse <- function(inverse) matrix_inverse <<- inverse
  	get_matrix_inverse <- function() matrix_inverse
  	list(set = set, get = get,
       	set_matrix_inverse = set_matrix_inverse,
       	get_matrix_inverse = get_matrix_inverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## set_matrix_inverse function. 

cacheSolve <- function(x, ...) {
	matrix_inverse <- x$get_matrix_inverse()
	if(!is.null(matrix_inverse)) {
    	message("getting cached data")
    	return(matrix_inverse)
  	}
  	data <- x$get()
  	matrix_inverse <- solve(data, ...)
  	x$set_matrix_inverse(matrix_inverse)
  	matrix_inverse
}
