# This program compute a matrix inverse
# Note: x must be a invertible matrix

# To generate inverse function, I uses solve() function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinver <- function(z) m <<- z
    getinver <- function() m
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
}


cacheSolve <- function(x, ...) {
    m <- x$getinver()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    x <- solve(data)
    x$setinver(m)
    m
}

