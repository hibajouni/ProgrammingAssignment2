makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse as NULL
     inv<- NULL
     set <- function(y){
          x<<-y
          inv<<- NULL
     }
     #function to get matrix
     get <- function(){x}
     setinverse <- function(inverse){inv<<- inverse}
     getinverse <- function(){inv} # function to obtain the inverse of the matrix
     list(set= set, get=get, setinverse= setinverse, getinverse=getinverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)){ #checking if the inverse is null
    message("chached data")
    return(inv) #return inverse value
    
  }
  mat <- x$get()
  inv <- solve(mat,...) #calculate inverse value
  x$setinverse(inv)
  inv #return a matrix that is inverse of x
}
