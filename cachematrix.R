## Caching the Inverse of a Matrix

## return a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    get <- function(){
        x
    }
    set <- function(x2){
        x <<- x2
        inv_x <<- NULL
    }
    getinv <- function(){
        inv_x
    }
    setinv <- function(inv_x2){
        inv_x <<- inv_x2
    }
    list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv_m <- x$getinv()
    if(!is.null(inv_m)){
        print('getting cached data')
        return(inv_m)
    }
    m <- x$get()
    inv_m <- solve(m, ...)
    x$setinv(inv_m)
    inv_m
}
