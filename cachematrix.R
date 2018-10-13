## The whole function gets the inverse of a given matrix by acelerating the process using the cache

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to set de value of the matrix, get the value of the matrix, set and get de inverse


makeCacheMatrix<-function(x = matrix()){
inv<-NULL
set<-function(y){
x<<-y
inv<<-NULL
}
get<-function() x
setinverse<-function(inv) inv<-inverse
getinverse<-function() inv
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the given matrix and sets the value of the inverse in the cache via the setmean function.

cacheSolve<-function(x, ...){
invrs<-x$getinverse()
if(!is.null(inv)){
message("getting cached data")
return(inv)
}
mat<-x$get()
inv<-solve(mat, ...)
x$setinverse(inv)
inv
}
## Return a matrix that is the inverse of 'x' 
