##' @title Get descriptives
##' @param data
##' @param iv
##' @return
##' @author Shir Dekel
##' @export
get_descriptives <- function(data = aggregation1::data, iv) {
  condition_allocation_table <-
    data %>%
    get_condition_allocation_table(iv) %>%
    janitor::clean_names(case = "sentence")

  total <-
    condition_allocation_table %>%
    pull(N) %>%
    last()

  total_apa <-
    total %>%
    papaja::printnum(numerals = FALSE, capitalize = TRUE)

    sex <-
      data %>%
      nest_by(id, sex) %>%
      ungroup() %>%
      count(sex)

    sex_female <-
      sex %>%
      mutate(
        across(sex, str_to_lower)
      ) %>%
      filter(sex == "female") %>%
      pull(n)

    sex_text <- "The average age was "

  numerical_names_raw <-
    c("age", "business_exp", "business_edu", "total_time")

  unit_raw <-
    c(
      "years" %>%
        rep(3),
      "min"
    )

  numerical_names <-
    data %>%
    names() %>%
    extract_from(numerical_names_raw)

  unit <-
    numerical_names_raw %in%
    numerical_names %>%
    unit_raw[.]

  numerical <-
    numerical_names %>%
    map(
      ~ data %>%
        summarise(
          across(
            all_of(.x),
            lst(mean, sd, min, max),
            .names = "{fn}",
            na.rm = TRUE
          )
        ) %>%
        mutate(across(everything(), ~ .x %>%
          papaja::printnum(drop0trailing = TRUE)))
    ) %>%
    set_names(numerical_names) %>%
    map2(
      unit,
      ~ str_c(
        .x$mean,
        " ",
        .y,
        " (*SD* = ",
        .x$sd,
        ", *min.* = ",
        .x$min,
        ", *max.* = ",
        .x$max,
        ")"
      )
    )

  sample <-
    data %>%
    pull(sample) %>%
    unique()

  if (sample == "prolific") {
    sample_description <-
      "the online recruitment platform Prolific. Participants were compensated at a rate of \\pounds 5 an hour (Prolific is based in the UK)."
  } else if (sample == "sona") {
    sample_description <-
      "a cohort of psychology undergraduates at The University of Sydney. Participants were compensated with course credit."
  } else if (sample == "prolific_sona") {
    sample_description <-
      "both the online recruitment platform Prolific and a cohort of psychology undergraduates at The University of Sydney. Participants from Prolific were compensated at a rate of \\pounds 5 an hour (Prolific is based in the UK), and participants from the undergraduate sample were compensated with course credit."
  }

  apa <-
    str_c(
      total_apa,
      str_c(
        "participants (",
        sex_female
      ),
      "female) were recruited from",
      sample_description,
      str_c(
        sex_text,
        numerical$age,
        "."
      ),
      sep = " "
    )

  if (all(numerical_names_raw %in% names(numerical))) {
    apa <-
      str_c(
        apa,
        "Participants reported an average of",
        numerical$business_exp,
        "working in a business setting, and an average of",
        numerical$business_edu,
        "of business education. The mean completion time of the task was",
        str_c(
          numerical$total_time,
          "."
        ),
        sep = " "
      )
  }

  descriptives <-
    lst(
      condition_allocation_table,
      total,
      total_apa,
      sex_female,
      numerical,
      apa
    )

  return(descriptives)
}
