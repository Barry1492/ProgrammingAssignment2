## cachematrix.R contains two functions: 
##      makeCacheMatrix()
##      cacheSolve()
## These functions run together will calculte the inverse of 
## an invertible matrix or will call that matirx from a cache
## if the matrix has already been calculated
##
##
##
## makeCacheMatrix creates a special matrix that can cache its inverse
##
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
##
##
## cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix.  If it has already been calculated, cacheSolve
## retrieves the inverse and prints "getting cahced data"
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message('getting cached data')
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
