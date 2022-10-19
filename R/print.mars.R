#' Title
#'
#' @param x the mars object return by mars
#' @param ... generic parameter
#'
#' @return print the coefficient of mars object
#' @export
#'
#' @examples
#' mm <- mars(wage ~ age,data=ISLR::Wage)
#' pm <- print(mm)
print.mars = function(x,...)
{
  coefs = x$coefficients

  for(i in seq_along(coefs))
  {
    if(i == 1) # ASSIGNING INTERCEPT
    {
      temp = cat(paste0("B", as.character(i-1)))
      cat("\n", "Coefficients:",  temp, coefs[i], "\n")

    }
    else if(i > 1)
    {
      temp = cat(paste0("B", as.character(i-1)))
      cat("\n", "Coefficients:", temp, coefs[i], "\n")
    }
  }
}
