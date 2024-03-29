##' @title Get results
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_results_2 <- function(data = aggregation2::data) {
  data_split <-
    split_data(data)

  choice <-
    get_results_glmer(data_split, "choice")

  proportion_omnibus <-
    list(
      data_split,
      names(data_split)
    ) %>%
    pmap(
      ~ .x %>%
        get_omnibus_aggregation("proportion", .y)
    )

  proportion <-
    proportion_omnibus %>%
    map(
      ~ .x %>%
        papaja::apa_print(es = "pes", mse = FALSE) %>%
        pluck("full_result") %>%
        unlist() %>%
        unname()
    )

  portfolio_binary <-
    get_results_glmer(data_split, "portfolio_binary")

  portfolio_number <-
    get_results_ttest(data_split, "portfolio_number")

  sample_size <-
    pwr.t.test(d = mean(c(0.47, 0.22)), power = 0.8) %>%
    .[["n"]] %>%
    prod(2) %>%
    ceiling()

  distribution_mean <-
    c("absent", "present") %>%
    map(
      ~ data_split$distribution %>%
        group_by(distribution) %>%
        summarise(across(
          c(choice, portfolio_binary),
          ~ .x %>%
            mean() %>%
            prod(100) %>%
            printnum() %>%
            str_c("%")
        )) %>%
        filter(distribution == .x)
    ) %>%
    set_names("absent", "present")

  portfolio_binary_distribution <-
    data_split$distribution %>%
    mutate(across(distribution, ~ .x %>%
      fct_relevel("absent"))) %>%
    nest_by(id, distribution, portfolio_binary) %>%
    glm(portfolio_binary ~ distribution, family = binomial, .) %>%
    apa_print_or() %>%
    filter(term == "distributionpresent") %>%
    pull(apa)

  choice_distribution <-
    data_split$distribution %>%
    mutate(across(distribution, ~ .x %>%
      fct_relevel("absent"))) %>%
    glmer(choice ~ distribution + (1 | id), family = binomial, .) %>%
    apa_print_or() %>%
    filter(term == "distributionpresent") %>%
    pull(apa)

  results_experiment2 <-
    lst(
      choice,
      proportion,
      portfolio_binary,
      portfolio_number,
      choice_distribution,
      portfolio_binary_distribution,
      distribution_mean
    )

  return(results_experiment2)
}
