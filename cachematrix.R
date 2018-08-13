## The first function makeCacheMatrix defines 3 functions get (), setinv(inverse), geting () 
## Its prime purpose is to enable other functions to cache their computational output via setinv(inverse) funtion


makeCacheMatrix <- function (x = matrix (c(4,2,7,6),2,2)) {
        inv <- NULL
        get <- function () {
                message("The get function is called and returns the arg of makeCacheMatrix")
                x
        }
        
        setinv <- function (inverse) {
                inv <<- inverse
                ## message("The setinv function is called and sets the inv")
        }
        
        getinv <- function () {
                inv
                ## message("The getinv function is called and returns the inv")
        }
        list (get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function evaluates if a computational result for the specific matrix exists. 
## It then either return the stored value or computes the inverse matrix and caches it for later

cacheSolve <- function (x, ...) {
        inv <- x$getinv ()
        if (!is.null(inv)) {
                message ("The inverse matrix was cached and is returened:")
                return (inv)
        }
        matrix <- x$get ()
        inv <- solve(matrix)
        x$setinv(inv)
        inv
}