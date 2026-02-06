stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()
host <- "github.com"

studycase_path <-
  download_data(
    example_dirs = "study_case_1",
    stics_version = stics_version,
    raise_error = TRUE
  )

workspace_path <- file.path(studycase_path, "bou00t1")

sim <- get_sim(workspace_path)

test_that("two dev vars", {
  expected <- list(bou00t1 = data.frame(
    var = c("iamfs", "irecs"),
    day = c(
      max(sim$bou00t1$iamfs),
      max(sim$bou00t1$irecs)
    ),
    date = c(
      as.Date("2000-06-19"),
      as.Date("2000-10-31")
    )
  ))
  expected$bou00t1$var <- factor(expected$bou00t1$var,
    levels = c("iamfs", "irecs"),
    ordered = TRUE
  )
  expected$bou00t1$day <- as.numeric(expected$bou00t1$day)
  expected$bou00t1$Date <- as.POSIXct(expected$bou00t1$date)
  observed <- get_dev_stages(sim, c("iamfs", "irecs"))
  expect_equal(observed, expected)
})
