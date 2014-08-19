#######              Caching the Inverse of a Matrix       ############
# Below are two functions that are used to create a special object that  
# stores a matrix and caches its inverse 

##          Test case for these functions
##      Create initial matrix
# > mdat <- matrix(c(1,-1/4,-1/4,1), nrow = 2, ncol = 2, byrow = T) 
# > a <- makeCacheMatrix(mdat)          # store the matrix in the environment
# > a$get()                             # displayed the stored matrix
# > cacheSolve(a)                       # initial use calculates inverse
# > cacheSolve(a)                       # subsequent use retrieves cached value
##      The second call indicates: getting cached inverse  
##      Now define a second matrix  
# > mdat2 <-matrix(4:7, 2, 2)           # simpler matrix creation syntax
##      Can use set() to change the matrix cached by cacheSolve:  
# > a$set(mdat2)                        # replace mdat with mdat2
# > a$get()                             # confirm matrix has been changed
# > cacheSolve(a)                       # calculate inverse on new matrix
# > cacheSolve(a)                       # access cached inverse of new matrix

#### This function creates a special "matrix" object that can cache its inverse 
# it makes use of the <<- operator which can be used to assign  
# a value to an object in an environment that is different from  
# the current environment.  
makeCacheMatrix <- function(x = matrix()) { 
     # makeCacheMatrix creates a special "matrix",  
     # which is really a list containing a function to 
     #      set the value of the matrix: set() 
     #      get the value of the matrix: get() 
     #      set the value of the inverse: setinverse() 
     #      get the value of the inverse: getinverse() 
     
     m <- NULL 
     
     ## set() 
     set <- function(y) { 
          x <<- y 
          m <<- NULL 
     } 
     
     ## get() 
     get <- function() {x} 
     
     ## setinverse() 
     setinverse <- function(solve) {m <<- solve} 
     
     ## getinverse() 
     getinverse <- function() {m} 
     
     # return a list of the functions 
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)      
} 

#### this function returns a matrix that is the inverse of 'x' 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# If the inverse has already been calculated (and the matrix has not changed),  
# then cachesolve will retrieve the inverse from the cache 
cacheSolve <- function(x, ...) { 
     #      The following function calculates the mean of the special "vector" created  
     #      with the makeVector() function. However, it first checks to see if the mean  
     #      has already been calculated.  
     #      If so, it gets the mean from the cache and skips the computation.  
     #      Otherwise, it calculates the mean of the data and sets the value of the mean  
     #      in the cache via the setmean function. 
     
     m <- x$getinverse() 
     if(!is.null(m)) { 
          message("getting cached inverse") 
          return(m) 
     } 
     matrix <- x$get() 
     m <- solve(matrix, ...) 
     x$setinverse(m) 
     m      
} 
