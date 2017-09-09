## These two functions are used to create a special object that stores
## a matrix and caches its inversed

## This function creates a special 'matrix' which is really a list containing a funcion to
## 1- set the matrix; 2- get the matrix; 3- set the inversed matrix; 4- get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set<- function(y){
                x<<- y
                i<<- NULL
        }
        get<- function() x
        setInverse <- function(inversed) i <<- inversed
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inversed of the special 'matrix' created with the above function
## If calculation has already been done (cached so), it gets from cache   

cacheSolve <- function(x, ...) {
        i<- x$getInverse
        if (!is.null(i)){
                message("getting cached inverse of the matrix")
                return(i)
        }
        data <- x$get()
        i<- solve(data, ...)
        x$setInverse(i)
        i
}