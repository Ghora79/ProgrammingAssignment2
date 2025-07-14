print("This is a Peer-graded Assignment: Programming Assignment 2: Lexical Scoping")
print("This file lives in GitHub")

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
M <- makeCacheMatrix()
M$set(matrix(rnorm(16), nrow = 4))
M$get()

cacheSolve <- function(x, ...) {
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinverse(inver)
    inver
}
cacheSolve(M)

