source("./packages.R")

lapply(list.files("./R", full.names = TRUE), source)

paper_deps<-
read_yaml("_bookdown.yml") %>%
        pluck("rmd_files")

list(
  tar_map(
    values = get_values(),
    names = "experiment_number",
    tar_target(
      plot,
      get_plot(data)
    ),
    tar_target(
      results,
      get_results(data)
    ),
    tar_target(
      descriptives,
      get_descriptives(data, iv)
    ),
    tar_file(
      materials,
      system.file("materials", package = paste0("aggregation", experiment_number))
    )
  ),
  tar_target(
    samuelson_distribution,
    get_samuelson_distribution()
  ),
  tar_file(
    input,
    "index.Rmd"
  ),
  tar_file(
    config_file,
    "_bookdown.yml"
  ),
  tar_file(
    deps,
        paper_deps
  ),
  tar_file(
    paper,
    render_with_deps(
      input = input,
      config_file = config_file,
      deps = c(
        # To make `tar_load()` and `tar_read()` work with bookdown
        !!tar_knitr_deps_expr(paper_deps),
        # To track changes in the files
        deps
      )
    )
  )
)
