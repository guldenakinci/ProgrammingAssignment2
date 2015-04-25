## Below are two functions that are used to create a special object 
## that caches a numeric matrix and its inverse.

## The first function, `makeCacheMatrix`, creates a special "vector", 
## which is a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## x is a numeric matrix
        
        ## initialize the inverse of x to NULL
        inv <- NULL
        
        ## 'set' function caches its matrix argument.  
        set <- function(y) {
                ## cache the matrix in x
                x <<- y
                
                ## Initialize inverse to NULL
                inv <<- NULL
        }
        
        ## 'get' gets the matrix x 
        get <- function() x
        
        ## 'setinverse' caches the inverse of x. 
        ## Its argument 'solve' is a library function that gets the inverse of a matrix.
        setinverse <- function(solve) inv <<- solve
        
        ## 'getinverse' gets the inverse of x
        getinverse <- function() inv
        
        ## Create the output of 'makeCacheMatrix'  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## cacheSolve' calculates the inverse of the special "matrix"
## created by 'makeCacheMatrix'. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache via 'getinverse' function and skips the computation. 
## Otherwise, it calculates the value of the inverse and 
## stores it in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## x, the input, is the output, the list, created by 'makeVector' 
        
        ## Get the inverse from the cache
        inv <- x$getinverse()
        
        ## If the inverse exists (not NULL), return the inverse.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If the inverse dosn't exist (NULL), calculate the inverse:
        ## Get the matrix
        data <- x$get()
        
        ## Calculate and cache the inverse
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        ## return the inverse
        inv
}


