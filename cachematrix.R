## This function takes an matrix and returns its inverse.
## For rerun of the code a cached result is returned.
## Please note that this only works for matrices whch are invertible.

## This function creates a Matrix with functions set, get, setinv & get inv as its elements
## These functions will be used later in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {    #Initializing the variable x
        i <- NULL    #Setting the variable i to a NULL value
        set <- function(y) {    #Setting the variables x,i to initial values
                x <<- y
                i <<- NULL
        }
        get <- function() x    #Returining the value of the matrix
        setinv <- function(inv) i <<- inv    #Assigning the value of inverse to the variable i in parent environment
        getinv <- function() i    #Returning the value of the inverse
        list(set = set, get = get,    #Assigning named variables to the functions, to be able to be called by $
             setinv = setinv,
             getinv = getinv)
}


## This function takes a makeCacheMatrix type object as input
## It calculates the inverse for a new matrix, and returns the cached inverse value for an old one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()    #Getting the inverse value from cache
        if(!is.null(i)) {     #If matrix ix old, cache will not be NULL and value will be returned
                message("getting cached data")
                return(i)
        }
        data <- x$get()    #Get the matrix 
        i <- solve(data, ...)    #Calculate the inverse
        x$setinv(i)    #Set the inverse value into the makeCacheMatrix object & cache
        i    #Return the inverse value
}
