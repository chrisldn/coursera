
##The first function, makeCacheMatrix creates a special "matrix", which is a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


##The following function calculates the inverse of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse matrix and sets the value in the cache via the setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        #check if the inverse has already been computed : 
        #m==Null means it was not and we need to do the computation
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

