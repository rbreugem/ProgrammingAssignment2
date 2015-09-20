## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function generates a list with 3 functions, that are called from the CacheSolve
#function to perform some matrix calculations and cache assignment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    get <- function() x
    setInv <- function(invertedmatrix) m <<- invertedmatrix
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
#This function first uses the getInv function inside the created 'special' matrix object
#to retrieve inverted matrix from cache. If there is one stored, it will return the inverted
#matrix from cache, otherwise it will first get the matrix data from the 'special' matrix
#object, solve it, assign the outcome to cache and return the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
