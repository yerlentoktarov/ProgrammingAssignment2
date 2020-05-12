## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv1<-NULL
        set<-function(y){
                x<<-y
                inv1<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)inv1<<-inverse
        getinverse<-function()inv1
        list(set=set, get=get, setinverse=setinverse,
             getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        mat1<-x$get()
        inverse<-solve(mat1, ...)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
