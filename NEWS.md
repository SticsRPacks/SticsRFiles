# SticsRFiles 0.2.0

## General changes

* DESCRIPTION
  * Suggests: formatR, readxl
  * added missing packages: curl, tidyr, tibble
  * license changed to License for CeCILL-C

* useless functions removed

* some functions have been renamed or generalized

* tests 
  * new ones added for : get_var_info, get_obs, get_daily_results, 
  force_param_values, 
  * test-get*: now using get_examples_path function, some fixes

* Documentation
  * vignettes and tutorial: anchor_sections set to FALSE, fixes on warnings and errors, using paged html tables
  * functions help: content fixes, completion: parameters, examples, setting examples to dontrun, ...


* fix: exported/unexported functions list

* fix: CRAN issues for checks


## Specific changes
* get_obs, get_daily_results: removed useless columns from return, simplification, 

* download_usm_xl, download_usm_csv: added verbose argument for masquing warnings, displays, overwritting case

* read_params_table : New function for getting parameters tables from csv files or excel files sheets, updates in vignettes and functions help

* get_param_txt: added varieties management, earlier return when param is null, catching names ending with numerical indices, get varietal parameter, reading now parameters attached to several layers or fertilizations, added examples files for Stics version 8.5

* Manipulating_Stics_XML_files.Rmd: using XML file for the last model version for displaying fragments as examples

* get_stics_versions_compat, manage_stics_versions.R: NA management in data

* get_file: added plant names in output (for obs and daily data), fixed issue and tests added for intercropping case

* get_daily_results: renamed class attribute to cropr_simulation, use of tibble::as_tibble(), mixed calculation using get_plants_nb, always returns a list, optional usm_name arg, uses get_plant_name instead of using its own code, added javastics_path argument if plant files are located in it, dual variables name syntax (i.e. lai_n or lai(n) for example), taking into account intercrops management

* functions refactoring fo getting observations and daily outputs

* all_out_var, get_var_info: now using partial matching on name column also

* static_help: new function for generating html files from package functions help, some fixes and updates for vignettes

* new function for downloading csv files from examples

* new functions for managing model versions information and examples

* get_obs* functions: fixes and optimization, added associated data and tests

* force_param_values: fix for one line tibble

* set_param_xml: output dir checking

* force_param_values: new function for forcing model parameters

* get_report_results: new function for extracting data from reports files

* get_param_info: added using of a specific XML file, attribute for Stics version, unique for parameters names list, building a filter before dplyr::filter 
* functions added to manage Stics versions data

* get_obs, get_plant_name: using usms.xml file outside of a Stics workspace, parsing mixed crops, file checking,usms_list renamed as usm_name

* gen_*_xml functions: fix for usms name column, 

* get_plant_name: getting now plant files list outside mapply call

* get_param_value: returns a named list even if only one parameter, fix for param_name nb > 1, without parameters names in args;returned  for each file

* xmlDocument class: fix for checking ids validity

* removed useless scripts for generating XML files

* get_plants_nb: is now an interface to get either plants nb from an xml file or a text file, returning an usms named vector

* download_data: fix select
 
* gen_usms_xml2txt: silent error when blanks in workspace_path, fix javastics workspace use,

* get_stics_versions_compat: all data as character, study_case_1 added 

* gen_usms_obs: added usms_list

* All functions code and help now using the get_examples_path function

* get_examples_path: new function for getting files examples according to their type (txt, XML, csv,...) 

* gen_varmod: forcing to add variable not in Stics output variables for a given version, now checks if a variable exists, dual variable syntax (i.e. lai_n or lai(n))

* set_param_txt: set value per soil layer, per variety

* get_usms_files: taking into account 2 plant files folders (without duplicates checks), taking into account varieties management, 

* get_usms_list: returns now a character vector of usms names

* get_param_xml: without parameters names in args;returned  for each file, 
name of the XML file added to the output














# SticsRFiles 0.1.0.9004

* Added a `NEWS.md` file to track changes to the package.
