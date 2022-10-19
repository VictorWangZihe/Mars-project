

#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @param formula an R formula
#' @param data a data frame containing the data for the model
#' @param control an object of class 'mars.control'
#' @param ... the generic parameter
#'
#' @return an object of class 'mars
#' @export
#'
#' @examples
#'  mm <- mars(wage ~ age,data=ISLR::Wage)
#' @import stats
#' @import ISLR
mars <- function(formula, data, control = NULL,...){
  if (is.null(control) || missing(control)) {
    control <- mars.control()
  }
  cc <- match.call() # save the call
  mf <- model.frame(formula,data)
  plotnames = colnames(mf)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1, drop = FALSE]
  x_names = colnames(x)
  fwd <- fwd_stepwise(y, x, control)
  bwd <- bwd_stepwise(fwd, control)
  model = lm(y ~.-1, data.frame(y = y, bwd$B))
  out = c(list(call = cc, formula = formula, y=y, B=bwd$B, Bfuncs = bwd$Bfuncs, x_names = x_names),
          model)
  class(out) = c("mars", class(model))
  return(out)
}

new_mars.control <- function(Mmax, d, trace){
  structure(list(Mmax=Mmax,d=d,trace=trace),class="mars.control")
}

validate_mars.control <- function(control){
  if (control$Mmax < 2) stop("Mmax is smaller than 2")
  if (!(class(control$d) == "numeric")) stop("d is not numeric")
  if (!(class(control$trace) == "logical")) stop("trace is not logical")
}

mars.control <- function(Mmax=2, d=3, trace=FALSE){
  if (Mmax < 2) {
    warning("Mmax is smaller than 2 we will take 2")
    Mmax = 2
  }
  structure(list(Mmax=Mmax,d=d,trace=trace),class="mars.control")
}

fwd_stepwise <- function(y,x,mc){

  N <- length(y)
  n <- ncol(x)
  B <- init_B(N,mc$Mmax)
  Bfuncs <- vector(mode = "list",length = mc$Mmax + 1)
  splits <- data.frame(m=rep(NA,mc$Mmax),v=rep(NA,mc$Mmax),t=rep(NA,mc$Mmax))

  for(i in 1:(mc$Mmax/2)) {
    M <- 2*i-1
    lof_best <- Inf
    for(m in 1:M) {
      svar <- setdiff(1:n,Bfuncs[[m]][,2])
      for(v in svar){
        tt <- split_points(x[,v],B[,m])
        for(t in tt) {
          Bnew <- data.frame(B[,(1:M)],
                             Btem1=B[,m]*h(1,x[,v],t),Btem2=B[,m]*h(-1,x[,v],t))
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat,mc) #should I use new LOF?
          if(lof < lof_best) {
            lof_best <- lof
            splits[M,] <- c(m,v,t)
          }#end for lof check
        }# end for t loop
      }#end for v loop
    }#end for m loop
    m <- splits[M,1]; v <- splits[M,2]; t <- splits[M,3]
    B[,M+1] <- B[,m]*h(-1,x[,v],t)
    B[,M+2] <- B[,m]*h(1,x[,v],t)
    Bfuncs[[M+1]] <- rbind(Bfuncs[[m]],c(-1,v,t))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[m]],c(1,v,t))
  }# end for M loop
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  for (i in 1:length(Bfuncs)){
    if (!is.null(Bfuncs[[i]])){
      colnames(Bfuncs[[i]]) <- c("s","v","t")
    }
  }
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}# end for function


bwd_stepwise <- function(fwd,mc){
  Jstar <- 1:(ncol(fwd$B))
  Kstar <- Jstar
  dat <- data.frame(y=fwd$y,fwd$B)
  lofstar <- LOF(y~.-1,dat,mc)
  for (M in (ncol(fwd$B)):2 ){
    b <- Inf
    L <- Kstar
    for (m in L){
      K <- setdiff(L,m)
      dat <- data.frame(y=fwd$y,fwd$B[,K])
      lof <- LOF(y~.,dat,mc)
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if(lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }
  }
  Jstar <- c(1,Jstar)
  B <- fwd$B[,Jstar]
  Bfuncs <- fwd$Bfuncs[Jstar]
  return(list(y = fwd$y,B = B,Bfuncs = Bfuncs))
}


init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax + 1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

h <- function(s,v,t){
  return(pmax(0,s*(v-t)))
}


LOF <- function(form,data,mc) {
  ff <- lm(form,data)
  err <- sum(residuals(ff)^2)
  N <- nrow(data)
  M <- length(ff$coefficients) -1
  CM <- sum(hatvalues(ff))
  CM_hat <- CM + mc$d*M
  value <- err*N/((N-CM_hat)^2)
  return(value)
}

