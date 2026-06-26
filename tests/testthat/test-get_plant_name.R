example_ic <- download_data(
  example_dirs = "study_case_intercrop",
  raise_error = TRUE
)
usms_file <- file.path(example_ic, "usms.xml")
usms <- get_usms_list(usms_file)
plants_number_usms <- unlist(
  get_param_xml(usms_file, "nbplantes"),
  use.names = FALSE
)
names(plants_number_usms) <- usms
plant_names_per_usm <- SticsRFiles:::get_plant_name(example_ic, usms_file)
plants_number_names <- unlist(lapply(plant_names_per_usm, length))

test_that("Researching parameter bounds", {
  expect_identical(usms, names(plant_names_per_usm))
  expect_equal(plants_number_usms, plants_number_names)
})
