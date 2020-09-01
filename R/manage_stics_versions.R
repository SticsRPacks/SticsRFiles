#' Getting read only account identifiers for the Stics subversion repository
#'
#' @return A list with fields `username` and `password`
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_svn_identifiers()
#'
#' $username
#' [1] "sticsread"
#'
#' $password
#' [1] "sticsread2020"
#'
#' }
get_svn_identifiers <- function() {

  return(list(username="sticsread", password="sticsread2020"))
}



#' Dowloading inputs and/or outputs csv files from a repository branch/tag
#'
#' @param branch_url Address of the branch or tag
#' @param dest_dir Directory path where to store files
#' @param file File name (inputs.csv or ouputs.csv)
#' or keyword "all" for both files (default)
#' @param ids Connexion identifiers to the subversion server
#' @param overwrite Logical, TRUE for overwtiting files, FALSE otherwise
#'
# @return
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_csv_files(
#'   branch_url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10",
#'   dest_dir = system.file("extdata", package = "SticsRFiles"))
#' }
get_csv_files <- function(branch_url,
                          dest_dir,
                          file = "all",
                          ids = get_svn_identifiers(),
                          overwrite = FALSE) {

  files_list <- c("inputs.csv", "outputs.csv")

  if (file =="all") {
    file <- files_list
  }

  if (!all( file %in% files_list)) stop("Unknown given file !")


  # Initialization of authentication on the subversion server
  # with an authorized read only account
  h <- curl::new_handle()

  curl::handle_setopt(h, username = ids$username)

  curl::handle_setopt(h, password = ids$password)

  # Setting the files url and download
  file_url <- paste0(branch_url, "/doc/", file )

  for (f in 1:length(file_url)) {
    dest_file <- file.path(dest_dir, file[f])

    if (file.exists(dest_file) && !overwrite ) {
      warning("File ", dest_file,
              "already exists",
              "(consider set overwrite to TRUE for passing through)!")
    }

    curl::curl_download(file_url[f],
                        handle = h,
                        destfile = dest_file)
  }
}


#' Adding a new version in the SticsRFiles library or package
#'
#' @param version_name name (i.e. "VX.Y") of the version to set
#' in the csv file containing informations about versions
#' @param url Subversion repository address of the branch, tag to get
#' information from
#' @param dest The destination where to write information "install" for writing
#' things in the installed SticsRFiles library (default), "package" for writing
#' them in the package project (in RStudio)
#' @param overwrite A logical, TRUE to overwrite csv files,
#' FALSE otherwise (default)
#' @param verbose Logical, TRUE for displaying warnings, FALSE otherwise
#'
#' @return An invisible data.frame containing versions data
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::add_stics_version(version_name = "V10.0",
#'  url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10")
#'
#' }
add_stics_version <- function(version_name,
                              url,
                              dest = "install",
                              overwrite = FALSE,
                              verbose = TRUE) {

  # Taking only into account adding or overwriting csv files : inputs.csv, outouts.csv
  # and updating csv file stics_versions_info.csv gathering by version existing files examples
  # by type.
  # Only the csv col is automatically filled and files copied.
  # Other files examples must be added manuelly and corresponding information updated in
  # stics_versions_info.csv

  # url: modulostics branch or tag url, starting with
  # https://w3.avignon.inra.fr/svn/modulostics/branches/ or
  # https://w3.avignon.inra.fr/svn/modulostics/tags/ or
  # completed with the branch or tag name stord in the subversion reporitory.

  # dest : install or package ?
  if ( dest == "install") dest_dir <- system.file("extdata", package = "SticsRFiles")

  if (dest == "package" )  {
    proj_dir <- rstudioapi::getActiveProject()
    if (base::is.null(proj_dir)) stop("Load the project SticsRFiles before proceeding !")
    dest_dir <- file.path(proj_dir,"inst", "extdata")
  }

  # Creating new version dir in csv subdir
  # if (!base::is.null(SticsRFiles:::get_versions_info(version_name = version_name, versions_dir = dest_dir)) &&
  #     !overwrite) {
  #   warning("Information about ", version_name, " already exists in ", dest_dir, " directory")
  # }

  # Creating csv dir
  dir_path <- file.path(dest_dir, "csv")
  if (!dir.exists(dir_path)) dir.create(dir_path)

  # Creating version dir
  dir_path <- file.path(dir_path, version_name)
  if (!dir.exists(dir_path)) dir.create(path = dir_path)


  # Getting csv files from repos (branch or tag url) or overwriting them
  get_csv_files(url, dir_path, overwrite = overwrite)


  # Writing data updated with new version information (about csv files location)
  set_versions_info(version_name = version_name,
                    versions_dir = dest_dir,
                    verbose = verbose)

  cat(paste0(version_name, " successfully set in SticsRFiles ", dest, ".\n"))
}


#' Updating csv files for a given version
#'
#' @param version_name name (i.e. "VX.Y") of the version
#' @param url Subversion repository address of the branch, tag to get
#' information from
#' @param dest The destination where to write information "install" for writing
#' things in the installed SticsRFiles library (default), "package" for writing
#' them in the package project (in RStudio)
#' @param verbose Logical, TRUE for displaying warnings, FALSE otherwise
#'
#' @return An invisible data.frame containing versions data in the
#' SticsRFiles library (or package)
#' @export
#'
#' @examples
#' \dontrun{
#' SticsRFiles::update_stics_version(version_name = "V10.0",
#'  url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10")
#'
#' }
update_stics_version <-function(version_name,
                                url,
                                dest = "install",
                                verbose = FALSE) {

  # Forcing csv files overwriting
  add_stics_version(version_name,
                    url,
                    dest = dest,
                    overwrite = TRUE,
                    verbose = verbose)
}


#' Writing information about Stics versions and related example files
#' directories in the SticsRFiles library or package
#'
#' @param version_name name (i.e. "VX.Y") of the version to add in versions
#' information
#' @param versions_dir Optional, , either an `extdata` directory path
#' of the installed SticsRFiles library (default) or of the package project
#' @param verbose Logical, TRUE for displaying warnings, FALSE otherwise
#'
# @return
#' @keywords internal
#'
# @examples
set_versions_info <- function(version_name,
                              versions_dir = system.file("extdata",
                                                         package = "SticsRFiles"),
                              verbose = TRUE) {

  # Stopping if versions_dir does not exist
  if (!dir.exists(versions_dir)) stop(versions_dir, " does not exist!")

  # Creating versions sub directory, if needed
  dir_path <- file.path(versions_dir, "versions")
  if (!dir.exists(dir_path)) dir.create(dir_path)

  # Getting data.frame for the new version: with only the csv column filled
  version_info <- get_version_info_tmpl(version_name = version_name)
  versions_info_file <- file.path(dir_path, "stics_versions_info.csv")

  # Getting existing data about versions
  versions_info <- SticsRFiles::get_versions_info( versions_dir = versions_dir )

  # Setting data for a new file, only with csv column filled
  if (base::is.null(versions_info)) versions_info <- version_info

  # Adding data in the final data.frame to write, if version_name does not exist
  if (! version_name %in% versions_info$versions ) {
    versions_info <- rbind(versions_info, version_info)
  } else {
    if (verbose) {
      warning(version_name," already exists in ",versions_info_file,
              ", it is safer updating it by hand !")
    }
  }

  # Writing the csv file in the appriopriate folder
  utils::write.csv2(x = versions_info,
                    quote = FALSE,
                    file = versions_info_file,
                    row.names = FALSE)

  return(invisible(versions_info))
}


#' Get a basic data.frame for a version
#'
#' @param version_name name (i.e. "VX.Y") of the version to set
#' in the output data.frame
#'
#' @return a data.frame, with version set in `versions` and `csv` columns
#' (for existing inputs.csv and outputs.csv files in a versions VX.Y
#' sub-directory)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_versions_info(version_name = "V10.0")
#'
#'   versions   csv obs sti txt xml xml_tmpl xl study_case_1
#'   1    V10.0 V10.0
#'
#' }
get_version_info_tmpl <- function(version_name) {

  data.frame( versions = version_name,
              csv = version_name,
              obs ="",
              sti = "",
              txt = "",
              xml = "",
              xml_tmpl ="",
              xl = "",
              study_case_1 = ""
  )
}

