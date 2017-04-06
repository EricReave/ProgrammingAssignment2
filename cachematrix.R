## Two functions to do the following:
#       makeCacheMatrix: Creates a matrix object and cache the inverse
#       cacheSolve: Computes the inverse of the matrx returned by makeCacheMatrix - if the 
#       inverse have already been calculated, rather retrieve the inverse from cache

## We create a special Matrix object & cache the inverse 
#  x is the original matrix passed to function call 
#  im is the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
       # Matrix
        im <- NULL                               # init 
        set <- function(y){             
                im <<- y
                im <<- NULL}
        get<-function() x
        
        
       # Inverse (Solve) 
        setsolve <-function(solve) im <<- solve
        getsolve <-function() im
        
        list(set = set, 
             get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
        
}


## Check if cached version of matrix inverse exists and then use that
#       or create the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getsolve()   
        if(!is.null(im)){                #if it exists already, i.e. not NULL
                message('Using the cached matrix')
                return(im)               #then use it
        }
        data <- x$get()
        im <- solve(data, ...)           #otherwise create the inverse now
        x$setsolve(im)
        im
}
