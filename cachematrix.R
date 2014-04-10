## Put comments here that give an overall description of what your
## functions do

## This function creates an object to hold the values
## associated with the function "cacheSolve"
## data is 'set' and can be accessed with 'get' (it is an invertible matrix)
## matrix inverse is 'set' with setInverse and can be accessed
## with getInverse

makeCacheMatrix <- function(x = matrix()) {
    ## set default matrix inverse
    mInverse <- NULL
    
    ## Create a function to set the value of 'x' (input matrix)
    set <- function(y) {
        
        x <<- y
        mInverse <<- NULL
    }
    ## A function to get the current value of 'x' (input matrix)
    get <- function() {
        x
    }
    ## function to set the matrix inverse
    setInverse <- function(solve) {
        mInverse <<- solve
    }
    ## function to retrieve the matrix inverse
    getInverse <- function()  {
        mInverse
    }
    ## return list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## This function returns the inverse of a matrix 'x'
## It checks to see whether the inverse has already been
## calculated, and if so, it retrieves the cached inverse matrix
## If not, it calculates the inverse, sets the result in makeCacheMatrix
## and returns the matrix inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## call the getInverse function inside makeCacheMatrix
    mInverse <- x$getInverse()
    ## if this is the first time executing (or matrix has changed)
    ## mInverse will have a NULL value
    ## If mInverse has been previously calculated, get the cached matrix inverse
    ## and return the matrix
    if (!is.null(mInverse)) {
        message("getting cached data")
        return(mInverse)
    }
    ## get the data (matrix) to be inverted
    data <- x$get()
    ## call the solve function to invert the matrix (assumes matrix is invertible)
    mInverse <- solve(data, ...)
    ## store the matrix inverse (cached)
    x$setInverse(mInverse)
    ## return the matrix inverse
    mInverse
}

## Testing

mat = matrix(c(0,7,1,2,5,0,3,0,7),3,3)
mat
mym <- makeCacheMatrix(mat)
ans = cacheSolve(mym)
ans

