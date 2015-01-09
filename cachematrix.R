## makeCacheMatrix function creates a matrix object of type 'list'
## and stores the original matrix value and the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {       # input x will be a matrix
        m <- NULL                                 # m will be the inverse of the matrix initially set to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}                     # returns original matrix
        setinv <- function(inv) {m <<- inv}       # sets m to be the inverse of the original matrix
        getinv <- function() m                    # will return the cached value to cacheSolve() if exists
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function returns the cached inverse of the matrix if it is cached,
## if it is not cached then this function calculates the matrix inverse

cacheSolve <- function(x, ...) {                   # input is object created by makeCacheMatrix
        m <- x$getinv()                            # gets the inverse of the object
        if(!is.null(m)) {
                message("getting cached data")     # if inverse exists already then get display the cached inverse
                return(m)
        }
        data <- x$get()                            # if inverse does NOT exist we get the matrix
        m <- solve(data, ...)                      # and then we solve for the inverse
        x$setinv(m)                                # and finally set the inverse value in the object created by makeCacheMatrix
        m
}
