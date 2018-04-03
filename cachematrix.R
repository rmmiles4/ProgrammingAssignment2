## cachematrix.R contains two functions 
## makeCacheMatrix creates takes in an invertible Matrix and creates an R object
## that stores that matrix and its inverse
## cacheSolve takes in a makeCacheMatrix object an returns the inverse cached in
## the makeCacheMatrix environment instead of recalculating the inverse


## Function to create the makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     
}


## Function to retrieve the inverse from memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv       
}