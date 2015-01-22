## Andrea Reif
## 2015 JAN 22
## rprog-010
## Assignment 2
## makeCacheMatrix creates a list containing the matrix and it's stored inverse and 
## functionality to set it
## cacheSolve will retrieve the cached inverse or set it if it does not exist

## function that creates a list containing the matrix passed in and member functions that
## cache it's inverse in the list and allow for its retrieval
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
    }


## Uses the list created by makeCacheMatrix to return inverse of x
## inverse of x is either retrieved from cache or calculated and
## stored to cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        ##getting cached data
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
