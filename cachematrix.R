##PROGRAMMING ASSIGMENT 2

#Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than computing it repeatedly.
#To avoid this the two funtions below are created in order 
#to cache the inverse of a matrix.

#the coding is based on the following example gien by the course:
#https://class.coursera.org/rprog-032/human_grading/view/courses/975106/assessments/3/submissions

##Function one: makeCacheMatrix does the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                                              
    set <- function(y) {
      x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inverse <<- solve
  getinv <- function() inverse
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

##Funtion two: cacheSolve does the following:
#1. checks if the inverse is calcualted before
#2. if true, it reteruns the inverse
#3. if not true, it calculates the inverse, caches it
#3. and returns the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}

#Lets check if it works:
#x <- matrix(c(1, 2, 2, 1), nrow=2, ncol=2)
#c_cache <- makeCacheMatrix(x)
#cacheSolve(c_cache)
#solve(x)
#Output:
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#> solve(x)
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333

