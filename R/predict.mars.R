
#' Title
#'
#' @param object the mars object return by mars
#' @param newdata a set of new data to predict on
#' @param ... generic parameter
#'
#' @return a list of data contain the predict values
#' @export
#'
#' @examples
#' mm <- mars(wage ~ age,data=ISLR::Wage)
#' pm <- predict(mm)
predict.mars = function(object,newdata,...) {
  if(missing(newdata) || is.null(newdata)) {
    X <- as.matrix(object$B)
  }
  else {
    tt = terms(object$formula,data = newdata)
    tt = delete.response(tt)
    mf = model.frame(tt, newdata)
    mt = attr(mf, "terms")
    X = model.matrix(mt, mf)[,-1]
    X = make_B(X, object$Bfuncs)
  }
  beta = object$coefficients
  drop(X %*% beta)
}

h <- function(s,v,t){
  return(pmax(0,s*(v-t)))
}

make_B = function(X, Bfuncs) {
  Xout = matrix(1, nrow = nrow(X), ncol = length(Bfuncs))
  for(i in 2:length(Bfuncs)) {
    Xout_col <- rep(1,nrow(X))
    for(j in 1:nrow(Bfuncs[[i]])){
      Xout_col <- Xout_col *h(Bfuncs[[i]][j,1],X[,Bfuncs[[i]][j,2]],Bfuncs[[i]][j,3])
    }
    Xout[,i] = Xout_col
  }
  Xout
}
