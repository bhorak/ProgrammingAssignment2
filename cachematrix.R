## makeCacheMatrix function creates a cached version of the matrix given as argument
## cacheSolve returns the inverse of the cached matrix

## creates a cache of the matrix
## has 4 methods

makeCacheMatrix <- function(x = matrix()) {
    
    ## initializez a variable, will be used for the inverse
    ## matrix
    m<- NULL
    
    ## allows the user to enter a new matrix to be cached 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## retrieves the matrix "x"
    get <- function() {
        x
    } 
    ## caches the inverse of matrix "X"
    setinverse <- function(inverse) {
        m <<- inverse
    }
    ## retrieves cached inverse of matrix "x"
    getinverse <- function() {
        m
    }
    ## returns a list to bes used be the "cacheSolve" function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## takes in the list returned by the makeCacheMatrix function
## and returns the inverse function of 'x'
## if the 'makeCacheMatrix' function was not called before
## each subsequent use of the 'cacheSolve' function then
## the cache solve function will just retrieve the cached inverse
## instead of trying to calculate it again

cacheSolve <- function(x, ...) {
    
    ## retrieves the cached inverse matrix,    
    m <- x$getinverse()
   
    ## checks if the cached version is not empty
    ## if isn't there is no need to solve for the 
    ## inverse again so it just returns the cached
    ## inverse "m" along with a message
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## in this case the "m" was empty and thus 
    ## the inverse will need to be solved for
    
    ## retrieves the supplied matrix
    data <- x$get()
    
    ## solves for the inverse and assigns it to "m"
    m <- solve(data, ...)
    
    ## calls the cache method to cache this new calculated
    ## inverse for future use
    x$setinverse(m)
    
    ## returns the inverse
    m
}
