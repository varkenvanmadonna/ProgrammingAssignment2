



#Creating a object of type makeCacheMatrix
#makeCacheMatrix does not really compute anything. It only allows us 
#to hold the inverse or set the value of a matrix
#x has to be initialized as a matrix
#the solved object contains the inverted matrix, which will be caculated by cacheSolve 
makeCacheMatrix <- function(x = matrix()) {
        solved <- NULL

#the setter - we assign any new matrix defined to x via special assignment

               set <- function(y) {
                x <<- y
                solved<<- NULL
        }
#The getter, used to retrieve the matrix

                       get <- function() x
                       
#the setter to store an inverted matrix result
 
        setinv <- function(inv) solved <<- inv
#the getter to retrieve the inverted matrix 
       
        getinv <- function() solved
#the list where it all gets stored so we can use it in cacheSolve

        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


#The cacheSolve function
# this function makes use the output of the makeCacheMatrix function
# to either retrieve an inverted matrix from cache or
# to calculate an inverted matrix and to store it in cache
# We assume the input matrix is invertible
cacheSolve <- function(x, ...) {
#applying the getter to retrieve any inverted matrix

        solved <- x$getinv()
        
#if the result is null, move on, otherwise return the inverted matrix 
        if(!is.null(solved)) {
                message("getting cached data.")
                return(solved)
        }
#call the getter to get the matrix data and assign it to data        
        data <- x$get()
#here is where we actually calculate the inverted matrix        
        solved <- solve(data)
#here is where we assign the inverted matrix to the solved variable
#so it can potentially be retrieved from cache
        x$setinv(solved)
        solved
}

