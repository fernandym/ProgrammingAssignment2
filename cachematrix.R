#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     
     # 1.A. set the matrix with null (initial)
     inv <- NULL
     # 1.B. set function of the matrix itself when the user sets the value
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     # 2. get the matrix by get function of matrix but not the inverse
     get <- function() x
     
     # 3. set the inverse of the matrix manually
     setinverse <- function(inverse) inv <<- inverse
     
     # 4. get the inverse of the matrix
     
     getinverse <- function() inv
     # 5. encapsulate into a list
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     # 1. Get the current state of the inverse and see if whether has been inverted yet
     inv <- x$getinverse()
     
     # 2. If it inv its not null or exist then return the inverted matrix and exit
     if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
     }
     
     # 3. If it is null set the original matrix to data
     data <- x$get()
     
     # 4. Set and find the inverse variable
     inv <- solve(data)
     
     # 5. Cache this result in the object
     x$setinverse(inv)
     
     # 6. Return this new result
     inv
}
