## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function allows to create a list for:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of matrix inverse
#4. get the value of matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(y){
		x <<- y
		minv <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse) minv <<- inverse
	getinverse <- function() minv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	## It first checks if the inverse has been already computed. If so, it returns that inverse; otherwise, it computes the inverse, sets the value in the cache via setinverse function.
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
