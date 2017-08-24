#' Replace an empty vector with something else.
#'
#' Based on `\%||\%` from rlang.
#'
#' @param x,y if `x` is empty, will return `y`; otherwise returns `x`.
#' @export
#' @name op-empty-default
#' @examples
#' c(1, 2)[1] %|||% NA_real_
#' c(1, 2)[0] %|||% NA_real_
#'
#' df <- data.frame(
#'   name   = c("a",   "a", "b", "c", "c", "a", "a"),
#'   var    = c("hgt", "wt", "hgt", "wt", "hgt", "eye", "wt"),
#'   value  = c(72, 225, 64, 60, 142, 2, 210)
#' )
#' df$value[df$name == "b" & df$var == "wt"] %|||% NA_real_
#'
`%|||%` <- function (x, y)
{
  if (rlang::is_empty(x)) {
    y
  }
  else {
    x
  }
}
