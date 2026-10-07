xml_path <- get_examples_path("xml")

usms_file <- file.path(xml_path, "usms.xml")
usms_list <- get_usms_list(usms_file)
sols_file <- file.path(xml_path, "sols.xml")
sols_list <- get_soils_list(sols_file)

context("Rewriting xml files")
test_that("usms.xml", {
  SticsRFiles:::rewrite_usms_file(
    usms_file = usms_file,
    out_dir = tempdir(),
    usm = usms_list[1:3]
  )
  expect_equal(get_usms_list(file.path(tempdir(), "usms.xml")), usms_list[1:3])
})

test_that("sols.xml", {
  SticsRFiles:::rewrite_sols_file(
    sols_file = sols_file,
    usms_file = usms_file,
    out_dir = tempdir(),
    usm = usms_list[1:3]
  )
  expect_equal(
    get_soils_list(file.path(tempdir(), "sols.xml")),
    unlist(
      get_param_xml(
        file = usms_file,
        param = "nomsol",
        select = "usm",
        select_value = usms_list[1:3]
      ),
      use.names = FALSE
    )
  )
})
