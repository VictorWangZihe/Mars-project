#' summary method for MARS (summary.mars)
#'
#' @description This is the summary method for MARS alogrithm.
#' @param object The MARS object return by mars() function.
#' @param ... S3 generic parameter
#'
#' @return A set of useful information of MARS object. Including coefficients, formula, variables, and Basis functions.
#' @export
#'
#' @examples
#' mymars <- mars(y~.,data = marstestdata)
#' summarymars <- summary(mymars)
#' @author Zihe Wang, Tianle Zhong, Xiaoying Qian
summary.mars = function(object,...)
{

  coefs = object$coefficients
  test = (object$formula)
  test = replace(test, c(1,2), test[c(2,1)])
  cat("Formula of the summary: ", as.character(test), "\n")
  j = 1
  for(i in names(coefs)){ # the intercept
    i <- as.numeric(gsub("B", "", i))
    if(i == 0)
    {temp = cat(paste0("B", as.character(i)))
    cat("\n", "Coefficients:",  temp, coefs[j],"variable:", "(intercept)", "\n")
    j = j+1
    }
    else{ # the variable
      temp = cat(paste0("B", as.character(i)))
      cat("\n", "Coefficients:", temp, coefs[j], "variable:",paste0( object$x_names[i],"\n"))
      j = j+1
    }
  }
  Bfuncs = object$Bfuncs
  cat("\n", "Below is the Basis functions", "\n") # Bfuncs
  print(Bfuncs)
}
