##' @title Get static branching values
##'
##' @return
##' @author Shir Dekel
##' @export
get_values <- function() {


  tibble(
    experiment_number = seq_len(2),
    iv = list(
      ## Experiment 1
      c(
        "similarity",
        "awareness",
        "presentation"
      ),
      ## Experiment rlang::sym
      c(
        "awareness",
        "distribution",
        "presentation"
      )
    ),
    data = list(
      quote(aggregation1::data),
      quote(aggregation2::data)
    )
  ) %>%
    rowwise() %>%
    mutate_function_call(
      "plot",
      experiment_number
    ) %>%
    mutate_function_call(
      "results",
      experiment_number
    )
}
