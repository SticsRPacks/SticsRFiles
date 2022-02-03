context("searching variables information")

# fixing version to latest standard one
stics_version <- "V9.1"

# creating an empty df
empty_df <- data.frame(name=character(0),
                       definition=character(0),
                       unit = character(0),
                       type=character(0),
                       stringsAsFactors = FALSE)

# Testing empty result
test_that("giving a unknown variable name returns a 0 row data", {

  empty_df_var <- get_var_info("myunknownvariable", stics_version = "V9.1")
  empty_df_keyword <- get_var_info(keyword = "myunknownvariable", stics_version = "V9.1")

  # Don't function, as expect_equal
  # Error message
  # Error in matrix(if (is.null(value)) logical() else value, nrow = nr, dimnames = list(rn,  :
  #   la longueur de 'dimnames' [2] n'est pas égale à l'étendue du tableau
  # testthat::expect_identical(empty_df_var, empty_df)
  # testthat::expect_identical(empty_df_keyword, empty_df)
  testthat::expect_true(dplyr::all_equal(empty_df, empty_df_var))
  testthat::expect_true(dplyr::all_equal(empty_df, empty_df_keyword))
})

var_lai_df <- data.frame(name=c("albedolai", "exolai"),
                         definition=c("albedo of the crop including soil and vegetation",
                                      "reduction factor on leaf growth due to water excess"),
                         unit = c("SD", "0-1"),
                         type=c("real", "real"),
                         stringsAsFactors = FALSE)

keyword_lai_df <- data.frame(name=c("albedolai", "diftemp1intercoupe"),
                             definition=c("albedo of the crop including soil and vegetation",
                                          "mean difference between crop and air temperatures during the vegetative phase (emergence - maximum LAI)"),
                             unit = c("SD", "degreeC"),
                             type=c("real", "real"),
                             stringsAsFactors = FALSE)

# Testing result for searching a variable name using lai
test_that("giving an existing partial variable name in var arg or keyword", {
  var_df <- get_var_info("lai", stics_version = stics_version)[1:2,]
  keyword_df <- get_var_info(keyword = "lai", stics_version = stics_version)[1:2,]
  testthat::expect_true(dplyr::all_equal(var_df, var_lai_df))
  testthat::expect_true(dplyr::all_equal(keyword_df, keyword_lai_df))
})

var_etmetr_df <- data.frame(name="etm_etr1moy",
                            definition="etm/etr ratio on the vegetative phase",
                            unit = "0-1",
                            type="real",
                            stringsAsFactors = FALSE)

# Testing with different versions
# Testing result for searching a variable name using etm_etr1moy
# or etm as keyword testing returned df dim
test_that("giving different versions", {
  existing_var_df <- get_var_info("etm_etr1moy", stics_version = "V9.1")
  missing_var_df <- get_var_info("etm_etr1moy", stics_version = "V8.5")

  testthat::expect_true(dplyr::all_equal(missing_var_df, empty_df))
  testthat::expect_true(dplyr::all_equal(existing_var_df, var_etmetr_df))

  var_df_9_1 <- get_var_info("etm", stics_version = "V9.1")
  var_df_8_5 <- get_var_info("etm", stics_version = "V8.5")

  all_eq <- dplyr::all_equal(var_df_9_1, var_df_8_5)
  if (is.character(all_eq)) all_eq <- FALSE

  testthat::expect_false(all_eq)
})



