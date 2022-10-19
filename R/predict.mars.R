#' predict method for MARS (predict.mars)
#'
#' @description This is the predict method for MARS. It predicts the outcome of the MARS model based on a set of data.
#' @param object The mars object return by mars
#' @param newdata A set of new data to predict with. If it is missing, then the method will using the old data from the MARS object.
#' @param ... S3 generic parameter
#'
#' @details The predict method uses the statistic model from the MARS object and a set of new data to predict the future values.
#' @return a list of data contain the predict values
#' @export
#'
#' @examples
#' mymars <- mars(y~.,data = marstestdata)
#' predictmars <- predict(mymars)
#' @author Zihe Wang, Tianle Zhong, Xiaoying Qian
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
