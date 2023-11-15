## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( mx = matrix() ) {

	## Initialize the inverse property
    in <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            mx <<- matrix
            in <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	mx
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        in <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        in
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    mx <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(mx) ) {
            message("getting cached data")
            return(mx)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    mx <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(mx)

    ## Return the matrix
    mx
}
