##' @title Get function call
##' @param name
##' @param experiment_number
##' @return
##' @author Shir Dekel
##' @export
get_function_call <- function(name, experiment_number) {
  function_call <-
    str_c(
      "get",
      name,
      experiment_number,
      sep = "_"
    ) %>%
    rlang::syms()
  return(function_call)
}
