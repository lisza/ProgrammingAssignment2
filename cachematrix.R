##This set of functions calculates and caches the inverse of a matrix, such that an already calculated inverse will be retrieved from the cache function rather than calculated anew each time.

##This function takes as an input a square matrix. It creates as an output a special object that contains a list of functions pertaining to the input matrix. Those will be used to assign, store and retrieve the inverse of the input matrix as calculated by the cacheSolve function below. 
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##This function takes as an argument the output of makeCacheMatrix. It computes the inverse of the matrix that was used as input argument of makeCacheMatrix(x) above. It first checks if the inverse has been calculated previously, in which case the cacheSolve will retrieve the inverse value stored in makeCacheMatrix.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
