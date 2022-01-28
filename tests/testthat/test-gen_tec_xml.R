library(SticsRFiles)
library(dplyr)
# options(warn=-1)
xl_path= file.path(get_examples_path("xl"), "inputs_stics_example.xlsx")

tec_param <- read_params_table(file = xl_path, sheet_name = "Tec")

out_dir <- file.path(tempdir(), "gen_xml")
if(!dir.exists(out_dir)) dir.create(out_dir)

gen_tec_xml(param_df = tec_param[4, ], out_dir = out_dir)

tec_xml <- file.path(out_dir, tec_param[4,]$Tec_name)


# For residues
xml_res_vec <- unlist(get_param_xml(file = tec_xml, select = "formalisme", select_value = "supply of organic residus")[[1]])

xl_res_vec <- select(tec_param[4,], starts_with(names(xml_res_vec)))

# For irrigation
xml_irr_values <- get_param_xml(file = tec_xml, select = "formalisme", select_value = "irrigation")[[1]]
# renaming param according to table
names(xml_irr_values)[7] <- "julapI"
names(xml_irr_values)[8] <- "doseI"

xl_irr_values <- select(tec_param[4,], starts_with(sort(names(xml_irr_values))))
xml_irr_values <- unlist(xml_irr_values[sort(names(xml_irr_values))], use.names = FALSE)

# For N supply
xml_fert_values <- get_param_xml(file = tec_xml, select = "formalisme", select_value = "fertilisation")[[1]]
# renaming param according to table (TODO: use param dict)
names(xml_fert_values)[6] <- "julapN"
names(xml_fert_values)[7] <- "doseN"

xl_fert_values <- select(tec_param[4,], starts_with(sort(names(xml_fert_values))))
xml_fert_values <- unlist(xml_fert_values[sort(names(xml_fert_values))], use.names = FALSE)

context("Comparing table values vs xml tec file values")

test_that ("Testing values for interventions", {
  expect_true(all(xml_res_vec == xl_res_vec))
  expect_true(all(xml_irr_values == xml_irr_values))
  expect_true(all(as.character(xml_fert_values) == xml_fert_values))
})
