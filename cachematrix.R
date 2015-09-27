## These functions lets you input a square matrix
## and cache its inverse for later usage

#The function below grabs your matrix input and 
## store it and its inverse in a list to be used in the cache function below

makeCacheMatrix <- function(x = matrix())
{
         m <- NULL 
         set <- function(y)
         {
                x <<- y 
                m <<- NULL 
         }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse 
         getinverse <- function() m 
        list(set = set, get = get,
        setinverse= setinverse,
        getinverse = getinverse)
}


## The function belows calculates inverse of your matrix input
## or retrieve an inverse that's already cache and returns the matrix inverse

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() 
        if(!is.null(m)) { 
                message("getting cached data") 
                return(m) 
         }
        data <- x$get() 
         m <- solve(data, ...) 
         x$setinverse(m) 
         m 
}
