print("This is a Peer-graded Assignment: Programming Assignment 2: Lexical Scoping")
print("This file lives in GitHub")

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    # set the value of the inverse
    setinverse <- function(inverse) inver <<- inverse
    # get the value of the inverse
    getinverse <- function() inver
    # return
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
        return(inver) # return ends the function
    }
    data <- x$get()
    # calculates the inverse and stores it as inv
    inver <- solve(data, ...)
    x$setinverse(inver)
    inver
}
cacheSolve(M)

