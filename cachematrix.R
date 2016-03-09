
## The combination of the two functions can calculate the inverse of a matrix and save it in cache.

## The following function, makeCacheMatrix can

#set the value of matrix
#get the value of matrix
#set the value of the inverse of matrix
#get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set<-function(y){
                x<<-y 
                inv<-NULL}
        
        get<-function(){x}
        
        setinverse<-function(inversee)
                
        {
                inv<<-inversee
                
        }
        
        getinverse<-function()
        {
                inv
        }
        
        list(get=get,set=set,getinverse=getinverse,setinverse=setinverse)

}


## The following function calculates the inverse for the matrix provided by the above function
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv))
        {
                message("Cached Data")
                return(inv)
        }
        matrixx<-x$get()
        inv<-solve(matrixx, ...)
        x$setinverse(inv)
        inv
}

