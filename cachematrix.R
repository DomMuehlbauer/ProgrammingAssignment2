## Put comments here that give an overall description of what your
## functions do

## The following function creates a 'special matrix' that helps saving the value of the inverse of a squared matrix
## without the need to re-calculate it. makeCacheMatrix returns a list of 4 functions that allow setting/getting the 
## values of the matrix & its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) matrix <<- solve
        getmatrix <- function() matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


##   The following function calculates the inverse of the 'special matrix' if it's a new matrix & returns the previously
## calculated inverse if it has already been calculated (its value is cached) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$getmatrix()
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        data <- x$get()
        matrix <- solve(data, ...)
        x$setmatrix(matrix)
        matrix
}
