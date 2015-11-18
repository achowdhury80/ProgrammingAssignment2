## The functions accepts matrix and calculate the inverse of the same. 
## The inverse of the matrix is cached. 
## The cache value is returned, if the underlying matrix remains unchanged.
## If the underlying matrix is changed then new inverse matrix is computed and cached for future use.


## The function accepts numeric matrix. 
## If nothining is passed as argument, it takes an empty matrix as argument
## The function returns a list
## The returned list has four function objects
## the first funtion object in the list, can be used to set the matrix.
## the second funtion object in the list, can be used to get the matrix.
## the third funtion object in the list, can be used to set the inverse of the matrix.
## the fourth funtion object in the list, can be used to get the inverse of underlying matrix.

makeCacheMatrix <- function(x=matrix()){
	inverse <- NULL
	set <- function(m){
		x <<- m
		inverse <<- NULL
	}
	get <- function(){
		x
	}
	getInverse <- function(){
		inverse
	}
	setInverse <- function(inv) {
		inverse <<- inv
	}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function returns the inverse of previously saved matrix.
## If previously computed cached value exists, then cache value is returned.
## Otherwise, it computes the inverse of the matrxi, save it in cache and returns the computed inverse matrix.

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	x$setInverse(solve(x$get())) 
	x$getInverse()
}