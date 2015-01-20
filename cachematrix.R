## Functions to cache the inverse of a matrix such that the inverse of a matrix need only
## to be calcualted once.

## create a list of set/get functions to access cached matrix and its inverse
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL ## init inverse of m as null
    
    ## update matrix, reset inv to null 
    setM <- function(newM){
        m <<- newm
        inv <- NULL
    }
    getM <- function() m ## return the matrix stored
    setInv <- function(newInv) inv <<- newInv ## update inverse with new value
    getInv <- function() inv ## return stored inverse (may be NULL)
    
    ## collect functions into a list to be returned
    list(setM = setM, getM = getM, setInv = setInv, getInv = getInv)
}


## returns the inverse associated to the vector of functions provided
## makes use of possible cached answer, the inverse calcualtion is only
## done if required (i.e. when storedInv = NULL)
cacheSolve <- function(cache, ...) {
    
    storedInv <- cache$getInv()
    
    if (is.null(storedInv)) {
        message("No cached inverse, doing calcualtion!")
        m <- cache$getM()
        inv <- solve(m)
        cache$setInv(inv)
        
    } else {
        message("Cached value exists!")
    }
    
    cache$getInv()
}
