## By citing most part of the example functions "makeVector" and "cachemean",
## the two new functions was aimed to calculate the inversed matrix of an 
## inversibel one, i.e. to calculate A^-1 if |A| is not 0. What is more, 
## since the calculation of A^-1 is an extremely time-cosuming job, the two 
## functions also cache the result, in case it may be calculated repeatedly 
## with the matrix A's content unchanged. 

## The function "makeCacheMatrix" was aimed to get both the value of the 
## original and inversed matrix and cache it into "x" and "rslt". Similar  
## to the function "makeVector", "makeCacheMatrix" also returns a list of 
## operations: set, get, setresult and getresult.

makeCacheMatrix <- function(x = matrix()) {
	rslt<-NULL
	set<-function(y){
		x<<-y
		rslt<<-NULL
	}
	get<-function()x
	setresult<-function(result)rslt<<-result
	getresult<-function()rslt
	list(set=set,get=get,
		setresult=setresult,
		getresult=getresult)

}


## The function "cachesolve" is the exact conductor of the solve() function, 
## which receives information form the function "makeCacheMatrix", check if 
## the inversed result has been calculated and cached. If the calculation do 
## happened, the function will directedly return the existed result, or it 
## will inverse the original matrix and return the inversed one. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	rslt<-x$getresult()
	if(!is.null(rslt)){
		message("getting cached data")
		return(rslt)
	}
	data<-x$get()
	rslt<-solve(data,...)
	x$setresult(rslt)
	rslt
}
## The following part is a test making sure the two functions work. 
cacheSolve(makeCacheMatrix(matrix(c(1,0,1,0,1,0,0,0,1),3)))
