## These functions computes inverse of a matrix and caches matrix inverse asuming matrix is a square matrix

## makeCacheMatrix sets the matrix and caches matrix inverse computed by cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if there is already computed inversed matrix of a given matrix then it returns cached result if it
## is found. if there is no cached result then it computes inverse of a matrix via solve function and returns result

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
        ## Return a matrix that is the inverse of 'x'
