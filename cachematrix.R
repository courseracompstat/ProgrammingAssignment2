## The following two functions compute and cache the inverse of a matrix.               
## It is useful to cache the result of matrix inversion, rather than compute it 
## repeatedly.


## This first function, makeCacheMatrix creates a special "matrix", which 
## is actually a list containing a function to do each of the following 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                  ## Value of 'i' is intially set to NULL
        set <- function(y) {                       ## This function assignes values to objects in their parent environment
                x <<- y					   ## Set the value of matrix 'x' in the parent evironment
                i <<- NULL                         ## Set the value of i to NULL in the parent environment
        }
        get <- function() x                        ## This function gets the value of the matrix 'x'
        setinverse <- function(solve) i <<- solve  ## This function sets the value of the inverse 'i' 
        getinverse <- function() i                 ## This function gets the value of the inverse 'i'
        list(set = set, get = get,                 ## Put the functions into a list so that they can be called by cacheSolve
             setinverse = setinverse,                 
             getinverse = getinverse)
}


## The following cacheSolve function computes the inverse of the special "matrix" created with the 
## above function. It first checks to see if the inverse has already been computed. 
## If so, it gets the inverse stored in cache and skips the computation. Otherwise, it computes
## the inverse and caches its value via the setinverse function.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()                        ## Assign current inverse value to 'i'
        if(!is.null(i)) {                          ## Statement is TRUE if inverse has already been computed
                message("getting cached data")     ## Print a message to let user know inverse is being retrieved from cache 
                return(i)                          ## Return the value of the cached inverse matrix
        }                                           
                                                   ## If inverse has not already been computed, then the following four lines are executed
        data <- x$get()                            ## Get the matrix 'x' 
        i <- solve(data, ...)                      ## Compute the inverse of the matrix 'x' and assign it to 'i' 
        x$setinverse(i)                            ## Cache the value of the inverse 'i'
        i                                          ## Return the value of inverse 'i'
}

                         