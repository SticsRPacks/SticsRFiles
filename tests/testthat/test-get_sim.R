context("get model outputs")

path <- get_examples_path(file_type = "sti")

# Testing with a JavaStics workspace like
test_that("output is always list", {
  outputs= get_sim(path,"banana")
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  # Should always returns a list, even for one usm
})

test_that("get output without usm argument", {
  outputs= get_sim(workspace = path)
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(is.data.frame(outputs$banana))
  expect_true(is.null(outputs$proto_rice))
})

# Testing without the usms.xml file
if (file.exists(file.path(path, "usms.xml"))) {
  file.rename(file.path(path, "usms.xml"), file.path(path, "usms.xml.ori"))
}

test_that("output is always list, without usms.xml", {
  outputs= get_sim(path,"banana")
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  # Should always returns a list, even for one usm
})

test_that("get output without usm argument, without usms.xml", {
  outputs= get_sim(workspace = path)
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(is.data.frame(outputs$banana))
})


# Testing a simple usm as path sub-directory
test_that("output is always list, without usms.xml, banana sub-dir", {
  if (!dir.exists(file.path(path,"banana"))) dir.create(file.path(path,"banana"))
  file.copy(file.path(path,"mod_sbanana.sti"), file.path(path,"banana"))
  outputs= get_sim(path,"banana")
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  # Should always returns a list, even for one usm
  unlink(file.path(path,"banana"))
})
# Restoring usms.xml
if (file.exists(file.path(path, "usms.xml.ori"))) {
  file.rename(file.path(path, "usms.xml.ori"), file.path(path, "usms.xml") )
}


example_IC = download_data(example_dirs = "study_case_intercrop")

test_that("get simulations with intercrops", {
  outputs= get_sim(workspace = example_IC)
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
                    c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0",
                      "SC_Wheat_2005-2006_N0")))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
               c("plant_1","plant_2"))
  expect_equal(unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
               c("Principal","Associated"))
  expect_equal(unique(outputs$`SC_Pea_2005-2006_N0`$Plant), "plant_1")
  expect_null(outputs$`SC_Pea_2005-2006_N0`$Dominance)
})

test_that("get simulations with intercrops, giving usms.xml file", {
  outputs= get_obs(workspace = example_IC, usms_filename = "usms.xml")
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
                    c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0",
                      "SC_Wheat_2005-2006_N0")))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
               c("ble","poi"))
  expect_equal(unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
               c("Principal","Associated"))
  expect_equal(unique(outputs$`SC_Pea_2005-2006_N0`$Plant), "poi")
  expect_null(outputs$`SC_Pea_2005-2006_N0`$Dominance)
})

