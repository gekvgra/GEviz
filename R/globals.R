#' Define Global Variables
#'
#' Get all variables for functions which are using unbound variables, such that the note 'no visible binding for global variable' when checking the package disappear.
#'
#' @seealso \href{https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887}{Link1} and \href{https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/}{link2}
globals <- function(){"This is not a function"}
utils::globalVariables(c("."))
