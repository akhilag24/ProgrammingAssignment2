## The Inverse of a Matrix s calculated and the result is cached in the below mentioned functions.
## Every time we try to calculate the inverse of matrix, it first looks if the same matrix
## was inversed before and the result cached. If yes, then it gets the result from the cached matrix
## else the inverse is calculated and the result is cached for future reference thereby reducing the cost.

## `makeCacheMatrix` creates a special "matrix" object that can cache its inverse.It contains a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of matrix(solve)
## 4.  get the value of the inverse of matrix(solve)

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse of matrix has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        i <- x$getinv()
        print(i)
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
