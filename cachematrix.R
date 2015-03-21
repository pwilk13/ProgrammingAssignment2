## The functions take advantage of previously calculated inverse matrix data, storing
## it in a cache and returning the cached value instead of re-calculating
## should that data be called upon again. 

## This function sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse matrix, and gets the value of the inverse
## matrix.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
	x<<-y
	m<<-NULL
}
get<-function() x
setinverse<-function(inverse) m<<-inverse
getinverse<-function() m
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function checks if the inverse of a matrix has already been calculated.
## If so, it retrieves that data and returns it. If not, it calculates the 
## inverse of the matrix. 

cacheSolve <- function(x, ...) {
m<-x$getinverse()
if(!is.null(m)){
	message("getting cached data")
	return(m)
}
data<-x$get()
m<-solve(data,...)
x$setinverse(m)
m
}
