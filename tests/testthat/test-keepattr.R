workspace <- get_examples_path(file_type = "sti")
situations <- SticsRFiles::get_usms_list(file = file.path(
  workspace,
  "usms.xml"
))
sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)

test_that("cropr_simulation attribute is kept", {
  expect_s3_class(sim[1], "cropr_simulation")
})
