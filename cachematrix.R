## Put comments here that give an overall description of what your
## functions do

##-------------------
## makeCacheMatrix
##-------------------
## this function will create an object with the following properties
## - set(NewMatrix) #set existing matrix to a new matrix
## - get            #get exisitng matrix
## - setSolve       #set Solved Matrix (inverse matrix)
## - getSolve       #get Solved Matrix (inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
	sm <- NULL
	set <- function(y) {
		x  <<- y
		sm <<- NULL
	}
	get <- function() x
	setSolve <- function(solveMatrix) sm <<- solveMatrix
	getSolve <- function() m
	
	#Exposing internal function to caller
	list(set = set, get = get, 
	     setSolve = setSolve, getSolve = getSolve)
}


##--------------
## cacheSolve 
##--------------
## cacheSolve will take an object created by makeCacheMatrix
##  and returns a solved Matrix(inverse matrix)
##   it will first try to find if the matrix is solved
##    if it is then it will return the cached matrix 

cacheSolve <- function(x, ...) {
	sm <- x$getSolve()
	if (!is.null(sm)) {
		message("getting cached data")
		return(sm)
	}	
	m  <- x$get()
	sm <- solve(m)
	x$setSolve(sm)
	return(sm)
}
