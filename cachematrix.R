#define the function makeCacheMatrix, where the argument is defined as a matrix as the default mode
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             # initialize inv (matrix inverse) as NULL
    set <- function(y) {                    # define the set function with argument y
        x <<- y                             # assign the value of matrix in parent environment
        inv <<- NULL                        # if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     # define get function which has no argument and returns the value of the matrix x
    
    setinverse <- function(inverse) inv <<- inverse  # defines setinverse function with argument inverse which assigns value of inv in parent environment
    getinverse <- function() inv                     # define getinverse function which has no arguments and returns the value of inv 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # refer to the functions with the $ operator
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() #we need the function getinverse, which is a component from the list created in makeCacheMatrix
    if(!is.null(inv)) {  #if inv is not null 
        message("getting cached data") #print the message getting cached data
        return(inv) #return inv
    }
    data <- x$get() #we need to call the function get created in makeCacheMatrix, so we want to extract the component get from the list 
    inv <- solve(data, ...) #we apply the function solve
    x$setinverse(inv) 
    inv
}

a=cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
#in order to check if the result is correct, we multiply the inverse matrix obtained by the initial matrix. The result should 
#be the identitiy matrix
a%*%matrix(1:4,2,2)

