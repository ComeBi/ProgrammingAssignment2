## The Function makeCacheMatrix return a list containing a function to :
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse of the matrix
##              4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The Function cacheSolve returns the inverse of the original matrix :
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## If not, it calculates the inverse and set it in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
