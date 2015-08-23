## makeCacheMatrix create a matrix 


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	#setting the matrix
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	#getting  the matrix
	get <- function(){
		 x
	}
	#set the inverse
	setinv <- function(inverse){
		 inv <<- inverse
	}
	#get the inverse 
	getinv <- function(){
		 inv
	}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        	
}


#cacheSolve calulate the inverse of the matrix created above 
# it first checks that if inverse has already been calculated. 
# if so it gets the inverse from the cache and skips the computation 
# otherwise it calculates the inverse of the martix and sets the inverse  
# in the cache via the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)){
		message("Getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
	
}

