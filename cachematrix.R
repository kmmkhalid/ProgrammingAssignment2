## R Programming Assignment #2
## The following functions are able to compute and cache the inverse of a matrix.


##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        set_matinv <- function(solve) mat_inv <<- solve
        get_matinv <- function() mat_inv
        list(set = set, get = get,
             set_matinv = set_matinv,
             get_matinv = get_matinv)
}

##This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` 
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mat_inv <- x$get_matinv()
        if(!is.null(mat_inv)) {
                message("getting cached matrix inverse")
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$set_matinv(mat_inv)
        mat_inv
}
