##' @title Mutate function call
##' @param data
##' @param name
##' @param experiment_number
##' @return
##' @author Shir Dekel
##' @export
mutate_function_call <- function(data, name, experiment_number) {
  data %>%
    mutate(
      "get_{ name }" := get_function_call(
        name,
        experiment_number
      )
    )
}
