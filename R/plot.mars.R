#' Plot method of MARS (plot.mars)
#'
#' @description This is the plot method of mars. It plot the results of mars into several graphs.
#' @param x the mars object return by mars
#' @param ... S3 generic parameter
#'
#' @details The plot method takes what returns by [mars()] and represents it in a visual way.
#' @return a few plots that demonstrates how explanary variable fits in the MARS model.
#' @export
#'
#' @examples
#' mymars <- mars(y~.,data = marstestdata)
#' plotmars <- plot(mymars)
#' @import graphics
#' @author Zihe Wang, Tianle Zhong, Xiaoying Qian
plot.mars = function(x,...)
{
  data = eval(x$call$data)
  tt <- terms(x$formula,data=data)
  tt <- delete.response(tt)
  mf <- model.frame(tt,data)
  mt <- attr(mf,"terms")
  X <- model.matrix(mt,mf)[,-1,drop = FALSE]
  Bf <- x$Bfuncs
  singleB <- which(sapply(Bf,function(x)NROW(x)==1))
  doubleB <- which(sapply(Bf,function(x)NROW(x)==2))
  for(i in singleB){    # the singe basis plot
    vv1 <- Bf[[i]][1,"v"]; varname1 <- x$x_names[[vv1]]
    xx <- seq(from=min(X[,vv1]),to=max(X[,vv1]),length=100)
    f <- function(x) {
      h(Bf[[i]][1,"s"],x,Bf[[i]][1,"t"])}
    yy <- f(xx)
    plot(xx,yy,main=varname1,type = "l",col="red",lwd=.1)
  }
  for(i in doubleB){   # the double basis plot
    vv1 <- Bf[[i]][1,"v"]; varname1 <- x$x_names[[vv1]]
    vv2 <- Bf[[i]][2,"v"]; varname2 <- x$x_names[[vv2]]
    xx <- seq(from=min(X[,vv1]),to=max(X[,vv1]),length=100)
    yy <- seq(from=min(X[,vv2]),to=max(X[,vv2]),length=100)
    ff <- function(x,y) {
      h(Bf[[i]][1,"s"],x,Bf[[i]][1,"t"])*
        h(Bf[[i]][2,"s"],y,Bf[[i]][2,"t"])}
    zz <- outer(xx,yy,FUN=ff)
    persp(xx,yy,zz,xlab=varname1,ylab=varname2,zlab="",
          main=paste0(varname1,":",varname2),theta=-30,phi=30,
          col="lightblue",lwd=.1)
  }
}

h <- function(s,v,t){
  return(pmax(0,s*(v-t)))
}
