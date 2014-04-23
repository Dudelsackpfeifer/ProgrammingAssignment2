# Function to create a special matrix that allows to cache its inverse.
# It's basically two functions: one that allows to create the matrix and maintain it, and other to calculate the inverse.
# If the inverse has already been calculated, the cache is retrieved.

# Function to create the matrix and maintain it
makeCacheMatrix <- function(m = matrix()) {
	inv <- NULL
	# Function to set the value of the matrix and reset the value of the inverse
	set <- function(y) {
		m <<- y
		inv <<- NULL
	}
	# Function to get the value of the matrix 
	get <- function() m
	# Function to set the inverse of the matrix
	setinv <- function(inverse) inv <<- inverse
	# Function to get the inverse of the matrix in case it is saved
	getinv <- function() inv
	#return a list with the functions that allows to maintain the matrix object
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}

# Function to calculate the inverse of the matrix created with the makeCacheMatrix function.
# The input must be a makeCacheMatrix object with an inversible matrix.
cacheSolve <- function(m, ...) {
	# Get the current inverse of the matrix in the object
	inv <- m$getinv()
	# If the inverse is not null, we simply return it.
	if(!is.null(inv)) {
	message("getting cached data")
	return(inv)
	}
	# If the inverse is null, we have to calculate it.
	data <- m$get()
	inv <- solve(data)
	# Store the calculated inverse to be able to return it in the future.
	m$setinv(inv)
	# return the calculated inverse
	inv
}
