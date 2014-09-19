## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a vector (actually, a list) of functions which set the value of the matrix, get the value of the matrix, set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
		x <<- y ## overwriting the value of the matrix
        inv <<- NULL
		}
    get <- function() x
    setinv <- function(i) inv <<- i ## overwriting the value of the inverse of the matrix
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    }


## cacheSolve returns the inverse of the inverse of the matrix using the vector created by the above function. The cacheSolve function firstly checks if there is the value of the inverse has been already calculated. If it is true, the function returns the inverse value from the cache. If not, it calculates and sets the inverse value of the matrix and returns this value. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    }
