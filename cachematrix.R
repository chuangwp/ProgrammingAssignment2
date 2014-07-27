## create a special object that stores a matrix and cache's its inverse
## matrix.

## Make a special matrix vector contains which is really a list 
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
## The following funciton calculate the inverse matrix of the special"matrix 
## vector" created with the above function. However, it first checks to see if 
## the inverse matrix has already been calculated. If sp. ot get's the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates 
## the inverse matrix of the data and sets the value of the inverse matrix in
## the cache via the setsolve function
cachesolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
