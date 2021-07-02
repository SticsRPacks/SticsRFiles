# SticsRFiles 0.4.1 (2021-07-02)

## Changes

  * FIX: force_param_values() did not work for parameters which names included parenthesis

  * Added dependency to dplyr >= 1.0.0 due to use of relocate function

  * Typos in doc

# SticsRFiles 0.4.0 (2021-06-22)

## General changes

  * DESCRIPTION / NAMESPACE: updates linked to new functions
    
  * Tests 
    * updates, new ones added (get_obs, get_sim) 

  * Functions:
    * new: get_lai_forcing, check_usms_files
    * help updates
    * help reference site update (gen_obs function)

  * Files:
    * some useless files removed 
    * new Stics dirs added (needed by tests)
    * fix: example dirs names in inst
    
  * Tutorial: minor fix for use-xml-files (working dir creation)
    
## Specific changes
  * get_sim / get_obs / get_file / get_file_ 
    refactoring
    added some controls
	now taking into account a usms file path in functions
	fixed issues related to intercrops data loading
	added data.frame columns detection for filtering data on dates

  * functions for manipulating xml files 
    taking into account a new kind of Stics parameter dependency (for plant and soil layers dependant parameters)
    
  * set_param_txt: fix for setting values for varietal parameters
    
  * gen_obs: fixes 
    generating multiple files from a sheet
    generating files from a single observed variable
  
  * gen_usms_xml2txt
    now compatible with lai forcing
    added optional argument for checking files
    now removing files before each files generation



# SticsRFiles 0.3.0 (2021-04-15)

## General changes

  * DESCRIPTION
    * Imports: lifecycle

  * Tests 
    * new ones added for : gen_tec_xml, gen_ini_xml

  * Functions:
    * Now managing functions life cycle

  * Files:
    * useless XML template files have been removed  
    
## Specific changes
  * get_sim: new function replacing get_daily_results
  
  * get_daily_results: has been set to deprecated (a warning is displayed)
  
  * set_param_value: changed error output to warning if any parameter name does not exist
  
  * gen_tec_xml: now taking into account specific parameters linked to cut crops 
  management
  
  * gen_ini_xml: fix for crop identifiers management (plant 1 or 2)
  
  * get_usms_list: changed error output to warning  when the usms file does not exist
  
  * force_param_value: now uses either parameters names including () or _ for indexed ones 
  (for example lai(n) or lai_n)
  
  * get_param_xml, set_param_xml: checks have been added for detecting parameters 
  duplicates in tec files



# SticsRFiles 0.2.0 (2021-01-15)

## General changes

* DESCRIPTION
  * Suggests: formatR, readxl
  * added missing packages: curl, tidyr, tibble
  * license changed to License for CeCILL-C
  * Travis configuration removed

* README:
  * Travis badge removed
  * Github Actions R-CMD-check badge added
  
* Functions:
  * useless functions have been removed
  * some functions have been renamed or generalized

* Tests 
  * new ones added for : get_var_info, get_obs, get_daily_results, 
  force_param_values 
  * test-get*: now using get_examples_path function, some fixes

* Documentation
  * vignettes and tutorial: anchor_sections set to FALSE, fixes on warnings and errors, using paged html tables
  * functions help: content fixes and completion (parameters, examples, setting examples to dontrun, ...)

* Fixes
  * exported/unexported functions list
  * CRAN issues for checks

* Automatic testing: 
  * Github Actions added for checks
  * Travis checks removed
  * SticsRTest Travis tests triggering removed



## Specific changes
* get_obs, get_daily_results: removed useless columns from return, simplification 

* download_usm_xl, download_usm_csv: added verbose argument for masquing warnings/displays, added overwritting case

* read_params_table : new function for getting parameters tables either from csv files or excel files sheets

* get_param_txt: added varieties management, earlier return when param is null, catching names ending with numerical indices, get varietal parameter, reading now parameters attached to several layers or fertilizations, added examples files for Stics version 8.5

* Manipulating_Stics_XML_files.Rmd: using XML file for the last model version for displaying files fragments as examples

* get_stics_versions_compat, manage_stics_versions.R: added NA management in data

* get_file: added plant names in output (for obs and daily data), fixed issue and tests added for intercropping case

* get_daily_results: renamed class attribute to cropr_simulation, use of tibble::as_tibble(), mixed calculation using get_plants_nb, always returns a list, added optional usm_name arg, uses now get_plant_name instead, added javastics_path argument if plant files are located in it, dual variables name syntax (i.e. lai_n or lai(n) for example), taking into account intercrops management

* functions refactoring for getting observations and daily outputs

* get_var_info: now using partial matching on *name* column also

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

* gen_*_xml functions: fix for usms name column

* get_plant_name: getting now plant files list outside mapply call

* get_param_value: returns a named list even if only one parameter, fix for param_name nb > 1, without parameters names in args;returned  for each file

* xmlDocument class: fix for checking ids validity

* removed useless scripts for generating XML files

* get_plants_nb: is now an interface to get either plants number from an xml file or a text file, returning an usms named vector

* download_data: fix select
 
* gen_usms_xml2txt: silent error when blanks in workspace_path, fix javastics workspace use,

* get_stics_versions_compat: all data as character, study_case_1 added 

* gen_usms_obs: added usms_list

* All functions code and help now using the get_examples_path function

* get_examples_path: new function for getting files examples according to their type (txt, XML, csv,...) and embedded Stics versions data

* gen_varmod: forcing to add variable not in Stics output variables for a given version, now checks if a variable exists, dual variable syntax (i.e. lai_n or lai(n))

* set_param_txt: set value per soil layer, per variety

* get_usms_files: taking into account 2 plant files folders (but without duplicates checks), taking into account varieties management

* get_usms_list: returns now a character vector of usms names

* get_param_xml: without parameters names in args;returned for each file, 
name of the XML file added to the output



# SticsRFiles 0.1.0.9004 (2020-02-03)

* Added a `NEWS.md` file to track changes to the package.
