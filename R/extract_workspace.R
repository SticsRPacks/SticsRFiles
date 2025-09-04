#' Extract an XML JavaSTICS workspace from a source workspace
#'
#' @param from_workspace Source workspace path
#' @param to_workspace Target workspace path
#' @param javastics JavaStics directory path, needed for getting
#' plant files which are stored in JavaSTICS `plant` directory, or general
#' parameters files and *.mod files from the `config` directory
#' @param usm vector of situation names (i.e. USMs to extract.)
#' @param use_mod_files Logical: if TRUE, *.mod files are searched in the
#' from_workspace and copyed to the to_workspace if found. Default is FALSE.
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#'# Extract all usms from a source workspace to a target workspace
#' extract_workspace("/path/to/source/workspace",
#'                   "/path/to/target/workspace",
#'                   "/path/to/javastics/dir")
#'
#'# Extract only selected usms from a source workspace to a target workspace
#' extract_workspace("/path/to/source/workspace",
#'                   "/path/to/target/workspace",
#'                   "/path/to/javastics/dir",
#'                   usm = c("SugarCane", "potato"))
#'
#' }
extract_workspace <- function(
  from_workspace,
  to_workspace,
  javastics,
  usm = NULL,
  use_mod_files = FALSE
) {
  # Managing workspace directory creation
  if (!dir.exists(to_workspace)) {
    dir.create(to_workspace, recursive = TRUE)
  }

  # Getting usms list from the src workspace directory,
  # selecting only the existing usms provided in the usm argument
  usms_path <- file.path(from_workspace, "usms.xml")
  all_usms <- get_usms_list(file = usms_path)

  # checking usms
  if (!is.null(usm)) {
    usm_exist <- all_usms %in% usm
    if (!any(usm_exist)) stop("All usms are missing !")
    unknown_usms <- usm[!usm %in% all_usms]
    if (length(unknown_usms) > 0) {
      warning("Unknown usm(s) name(s): ", paste(unknown_usms, collapse = ";"))
    }

    sel_usms <- all_usms[usm_exist]

    # Rewriting the usms.xml file in the target workspace
    rewrite_usms_file(
      file.path(from_workspace, "usms.xml"),
      to_workspace,
      usm = sel_usms
    )
  } else {
    sel_usms <- all_usms
    # just copying the usms.xml file
    if (
      !file.copy(
        from = file.path(from_workspace, "usms.xml"),
        to = file.path(to_workspace, "usms.xml")
      )
    ) {
      stop("Error copying usms.xml file")
    }
  }

  # Rewriting the sols.xml file in the target workspace
  # in any case bc it depends on the usms, and the sols.xml file
  # may contain soils not related to the selected usms
  rewrite_sols_file(
    file.path(from_workspace, "usms.xml"),
    file.path(from_workspace, "sols.xml"),
    to_workspace,
    usm = sel_usms
  )

  # Getting usms files from the src workspace directory
  usms_files <- get_usms_files(
    workspace = from_workspace,
    usm = sel_usms,
    javastics = javastics,
    use_mod_files = use_mod_files
  )

  # get the vector of existing files paths
  usms_files_paths <- unlist(
    lapply(usms_files, function(x) {
      x$paths[x$exist]
    }),
    use.names = FALSE
  )

  # getting plant files paths
  plt_idx <- grep(pattern = "_plt\\.xml", usms_files_paths)
  plt_files_paths <- usms_files_paths[plt_idx]

  # manage plant files: create plant directory if not exist in the to_workspace
  if (!dir.exists(file.path(to_workspace, "plant"))) {
    print(dir.create(file.path(to_workspace, "plant")))
  }
  cp_status_plt <- file.copy(
    from = plt_files_paths,
    to = file.path(to_workspace, "plant")
  )

  # copy all other files to to_workspace
  other_files_paths <- usms_files_paths[-plt_idx]

  cp_status <- file.copy(from = other_files_paths, to = to_workspace)

  # TODO: add check of the status
}
