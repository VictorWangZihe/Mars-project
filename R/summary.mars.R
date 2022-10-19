

#' Title
#'
#' @param object the mars object return by mars
#' @param ... generic parameter
#'
#' @return the summary of mars object
#' @export
#'
#' @examples
#' mm <- mars(wage ~ age,data=ISLR::Wage)
#' sm <- summary(mm)
summary.mars = function(object,...)
{

  coefs = object$coefficients
  test = (object$formula)
  test = replace(test, c(1,2), test[c(2,1)])
  cat("Formula of the summary: ", as.character(test), "\n")
  for(i in seq_along(coefs))
  {
    if(i == 1) # ASSIGNING INTERCEPT
    {
      temp = cat(paste0("B", as.character(i-1)))
      cat("\n", "Coefficients:",  temp, coefs[i],"variable:", "(intercept)", "\n")

    }
    else if(i > 1)
    {
      temp = cat(paste0("B", as.character(i-1)))
      cat("\n", "Coefficients:", temp, coefs[i], "variable:",paste0("X", as.character(i-1),"\n"))
    }
  }
  vars = object$x_names
  Bfuncs = object$Bfuncs

}
