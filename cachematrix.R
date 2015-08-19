
##The function, makeCacheMatrix creates a special "matrix", which has below functions
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse of matrix
##Assumption- matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
	s<-NULL
	set<-function(y){
		#If input mattrix changes then it overwrite x and nullify s.
		x<<-y	
		s<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) s<<- solve
	getinverse<-function() s
	list(set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}  

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache else it calculates.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       s <-x$getinverse()
       if(!is.null(s)){
 	      message("getting cached data")
   	      return(s)
    }
      matrix<-x$get()
      s<-solve(matrix, ...) #Recalculating the inverse as input matrix changed.
      x$setinverse(s)
      s
}
