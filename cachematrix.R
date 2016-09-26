## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.



## This function creates a special "matrix" object(assume here always the squre and invertible
## matrix) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inver <<- inverse
    getInverse <- function() inver
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inver <- x$getInverse()
    if(!is.null(inver)) {
        message('getting cached inverse matrix!')
        return(inver)
    }
    dataMatrix <- x$get()
    inver <- solve(dataMatrix, ...)
    x$setInverse(inver)
    inver
}

        
## Sample solution
# > testMatrix <- makeCacheMatrix(matrix(c(8,12,9,10,11,5,7,6,13), 3, 3))
# > testMatrix$get()
#     [,1] [,2] [,3]
# [1,]    8   10    7
# [2,]   12   11    6
# [3,]    9    5   13
# > testMatrix$getInverse()
# NULL
# > cacheSolve(testMatrix)
#           [,1]       [,2]        [,3]
# [1,] -0.2904884  0.2442159  0.04370180
# [2,]  0.2622108 -0.1053985 -0.09254499
# [3,]  0.1002571 -0.1285347  0.08226221
# > cacheSolve(testMatrix)
# getting cached inverse matrix!
#           [,1]       [,2]        [,3]
# [1,] -0.2904884  0.2442159  0.04370180
# [2,]  0.2622108 -0.1053985 -0.09254499
# [3,]  0.1002571 -0.1285347  0.08226221
# > testMatrix$getInverse()
#           [,1]       [,2]        [,3]
# [1,] -0.2904884  0.2442159  0.04370180
# [2,]  0.2622108 -0.1053985 -0.09254499
# [3,]  0.1002571 -0.1285347  0.08226221
