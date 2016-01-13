## *******************************************************************************
## ********** Programming Assignment 2 of Coursera R Programming Course **********
## ************************** Lexical Scope excercise ****************************
## ***************************** Otto Ramos, Mexico ******************************
## *******************************************************************************
##
## To test the functions:
##
## 01.- Create the Matrix:
##        myMatrix <- makeCacheMatrix(matrix(1:4,2,2))
## 02.- Check created matrix:
##        myMatrix$get()
## 03.- Check inversed matrix (must be NULL, not yed calculated):
##        myMatrix$getInversed()
## 04.- Calculate the Inverse Matrix, calculated for the first time:
##        cacheSolve(myMatrix)
## 05.- Calculate again the Inverse, must display "Cached data!":
##        cacheSolve(myMatrix)
## 06.- Check inversed matrix:
##        myMatrix$getInversed()
## 07.- Modify the values of the matrix:
##        myMatrix$set(matrix(c(5,7,3,8),2,2))
## 08.- Check modified matrix:
##      myMatrix$get()
## 09.- Check inversed matrix (must be NULL, because matrix changed and not yet calculated):
##        myMatrix$getInversed()
## 10.- Calculate the Inverse Matrix:
##        cacheSolve(myMatrix)
## 11.- Calculate again the Inverse, must display "Cached data!":
##        cacheSolve(myMatrix)
## 12.- Check inversed matrix:
##        myMatrix$getInversed()

## Function that creates a special matrix that can cache its inverse operation
makeCacheMatrix <- function(x = matrix())
{
  inversed <- NULL
  
  set <- function(y)
  {
    x <<- y
    inversed <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setInversed <- function(inverse)
  {
    inversed <<- inverse
  }
  
  getInversed <- function()
  {
    inversed
  }
  
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## Function to calculate the inversal matrix, if already calculated return cached value
## otherwise calculate it
cacheSolve <- function(x, ...)
{
  xInv <- x$getInversed()
  
  if(!is.null(xInv))
  {
    message ("Cached data!")
    return(xInv)
  }
  
  data <- x$get()
  
  xInv <- solve(data, ...)
  
  x$setInversed(xInv)
  xInv
}
