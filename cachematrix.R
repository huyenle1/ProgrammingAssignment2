## Caching the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)        
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m       

}

## Test matrices
x <- matrix (1:4,2,2)
x1 <- makeCacheMatrix(x)
x2 <- cacheSolve(x1)
x2

a = matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3,byrow = TRUE)
a1 <- makeCacheMatrix(a)
a2 <- cacheSolve(a1)
a2