#' print method for MARS (print.mars)
#'
#' @description This is the print method for MARS that print a set of useful imformations.
#' @param x The MARS object return by mars() function.
#' @param ... S3 generic parameter
#'
#' @return print the coefficient of mars object
#' @export
#'
#' @examples
#' mymars <- mars(y~.,data = marstestdata)
#' printmars <- print(mymars)
#' @author Zihe Wang, Tianle Zhong, Xiaoying Qian
print.mars = function(x,...)
{
  coefs = x$coefficients

  for(i in seq_along(coefs)) # print out the coefficients.
  {
      temp = cat(paste0("B", as.character(i-1)))
      cat("\n", "Coefficients:",  temp, coefs[i], "\n")
  }
}
