## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix sets and gets the matrix and sets and gets the inverse
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
      	set <- function(y) {
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


## Write a short comment describing this function
## cacheSolve takes a list x of the functions defined in makeCacheMatrix and 
## gets the inverse of the matrix. If the matrix is the same, it gets the inverse already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		m <- x$getinverse()
        	if(!is.null(m)) {
                message("getting cached data")
                return(m)
        	}
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
