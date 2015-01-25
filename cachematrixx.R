## makeCacheMatrix is function that makes matrix inverse with solve function.
## first, in makeCacheMatrix environment i created mat and assigned NULL. 
## Purpose is to place mat in makeCacheMatrix environment
## Set function resets data whenever matrix is changed by assigning NULL to mat
## in enclosing environment and y (free variable) to x
## Similar as setmean in example, setsolve function makes inverse matrix mat of 
## original x matrix and getsolve prints inverse matrix m
## List function is placed at the end for easier presenting function 
## (when calling makeCacheMatrix function)

makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        set<-function(y){x<<-y; mat<<-NULL}
        get<-function() {x}
        setmatrix<-function(solve) {mat<<-solve}
        getmatrix<-function(){mat}
        list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
        }


## Next function reterns inverse matrix. If inverse matrix is
## already in cache it gives a message and returns cached matrix
## inverse matrix is assigned to mat and if mat isn't null 
## (cached data have mat different than NULL, becouse inverse matrix
## is added in setmatrix environment but to be placed in enclosing env.
## which is makeCackeMatrix environment) then cached data are presented,
## If mat is NULL, then data are assigned to matrix and mat is 
## calculated as inverse(solve) of matrix and added in setmatrix function
## in makeCacheMatrix function (so the next time function is runned, mat will
## not be NULL anymore)

cacheSolve <- function(x=matrix(), ...) {
        mat<-x$getmatrix()
        if(!is.null(mat)){message("getting cached data") 
        return(mat)}
        matrix<-x$get()
        mat<-solve(matrix,...)
        x$setmatrix(mat)
        print(mat)
        }
