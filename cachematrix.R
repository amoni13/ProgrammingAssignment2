## x<-makeCacheMatrix()will create a list of the original matrix and the solved matrix
## cacheSolve(x) used in conjunction with above, will look for the cached matrix or, if NULL, solve for its inverse

## makeCacheMatrix will create a list of the original matrix and the cached matrix

makeCacheMatrix <- function(x = matrix()) {##input "x" will be matrix
        m<-NULL ##reset to NULL for each makeCacheMatrix call
        set<-function(y){
                x<<-y  ##this will save the original matrix
                m<<-NULL
}
        get<-function() x  ## this returns the original matrix, when cacheSolve is called
        setsolve<- function(solve) m<<-solve ##this will cache the matrix, when cacheSolve is called 
        getsolve<-function()m ##this will return the cached value to cacheSolve
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve) ##this is the final list of values so cacheSolve knows what to do
}


## cacheSolve used in conjunction with above, will look for the cached matrix or, if NULL, solve for its inverse

cacheSolve<-function(x, ...) {#the name of the list created by makeCacheMatrix
        m<-x$getsolve()
            if(!is.null(m)) { ## if m has value available from the makeCacheMatrix list, gets value
                message("getting cached data")
                return (m)
            }
        data<-x$get() ##this is the else part, if m is NULL
        m<-solve(data,...) ##solves for inverse of matrix
        x$setsolve(m)  ## store the answer to the makeCacheMatrix list
        m ##return the answer, a matrix that is the inverse of 'x'
}
