#' @title Get JavaSTICS command line executable name
#'
#' @description From a JavaSTICS path, searching for the JavaSTICS command
#'
#' @param javastics JavaSTICS installation root folder
#' @param verbose Logical value (optional), TRUE to display run infos, FALSE otherwise (default)
#'
#' @details If JavaStics version < 1.5, returns `JavaSticsCmd.exe`
#' otherwise, returns `JavaStics.exe`
#'
#' @return An executable name, with attached command option string as attribute
#'
#' @examples
#' \dontrun{
#'  get_javasticscmd_exe("path/to/JavaSTICS")
#' }
#'
#' @keywords internal
get_javasticscmd_exe <- function(javastics, verbose = TRUE) {

  if (!dir.exists(javastics))
    stop("Error, directory does not exist: ", javastics)

  javastics_cmd <- NULL
  option <- ""
  prev_cmd <- "JavaSticsCmd.exe"

  if (file.exists(file.path(javastics,prev_cmd))) {
    javastics_cmd <- prev_cmd
  } else {
    javastics_cmd <- "JavaStics.exe"
    if (verbose) option <- " --verbose"
  }

  if (is.null(javastics_cmd))
    stop("Error, no JavaSTICS command line found in", javastics)

  javastics_cmd <- paste0(javastics_cmd, option)

  return(javastics_cmd)


}


#' @title Get JavaSTICS command line string
#'
#' @description From a JavaSTICS path, constructing command lines for
#' files generation from XML and/or running simulation through
#' a system2 command execution
#'
#' @param javastics JavaSTICS installation root folder
#' @param java_cmd Name or path of the java virtual machine executable (optional)
#' @param type keywords for filtering in the returned list what kind of command
#' to be generated (default both: "generate" and "run")
#' @param workspace A JavaStics workspace path (optional)
#' @param verbose Logical value (optional), TRUE to display run infos, FALSE otherwise (default)
#'
#' @details According to a JavaStics version (i.e. javastics path content) and
#' the OS type, the function builds command line strings used from system2
#' calls. Commands may be filtered using type argument.
#'
#' @return A named list with fields : command (java command/path), cmd_generate
#' and/or cmd_run that are to be used as args in the system2 function call.
#'
#' @examples
#' \dontrun{
#'  get_javastics_cmd(javastics = "path/to/JavaSTICS",
#'    ,               workspace = "path/to/workspace")
#' }
#'
#' @keywords internal
get_javastics_cmd <- function(javastics,
                              java_cmd = "java",
                              type = c("generate", "run"),
                              workspace = NULL,
                              verbose = TRUE) {

  # detecting JavaStics command exe name from javastics path
  javastics_cmd <- get_javasticscmd_exe(javastics, verbose = verbose)

  # Base command string, without workspace
  if (user_os() != "win") {
    check_java_version(javastics_cmd = javastics_cmd, java_cmd = java_cmd)
    command <- java_cmd
    generate <- paste0('-jar ',javastics_cmd,' --generate-txt') #"',ws,'"')
    run <- paste0('-jar ',javastics_cmd,' --run') #"',ws,'"')
  } else {
    command <- javastics_cmd
    generate <- paste0(' --generate-txt') #"',ws,'"')
    run <- paste0(' --run') #"',ws,'"')
  }

  # adding workspace name if provided
  if (!is.null(workspace)) {
    generate <- paste(generate,workspace)
    run <- paste(run, workspace)
  }

  # All strings in a named list
  # content returned conditionally to type content
  list(command = command,
       cmd_generate = generate,
       cmd_run = run)[c("command", paste("cmd", type, sep = "_"))]

}

#' @title Checking if the java virtual machine is compatible with the given
#' JavaStics command executable
#'
#' @description For Unix like systems, the system java virtual machine is
#' detected and consistency for JavaStics executable used is checked
#'
#' @param javastics_cmd JavaSTICS command line executable name
#' @param java_cmd Name or path of the java virtual machine executable
#'
#' @details Errors are raised if the JavaStics executable cannot be run with the
#' given java virtual machine.
#'
#'
#' @examples
#' \dontrun{
#'  check_java_version(javastics_cmd = "JavaStics.exe")
#' }
#'
#' @keywords internal
check_java_version <- function(javastics_cmd = "JavaSticsCmd.exe",
                               java_cmd="java") {

  # Making difference between a command and an alias to the command
  # does not function for the moment, from R (from a linux bash )
  # So, java_cmd may contain a java executable path
  java_path <- system2("which", java_cmd, stdout = TRUE, stderr = TRUE)
  if(!length(java_path)) {
    #alias <- system2("alias", java_cmd, stdout = TRUE, stderr = TRUE)
    # getting from alias java path, may be later if
    stop("java not found !")
  }

  # Getting version string
  java_version <- system2(java_cmd, "-version", stdout = TRUE, stderr = TRUE)

  version_str <- gsub("[\"a-z\ ]", x = java_version[1], replacement = "")

  ver <- as.numeric(substr(1,3,x = version_str))

  if (javastics_cmd == "JavaStics.exe" && ver < 11)
    warning("The installed java version is not usable with that version of
            JavaSTICS, use at least java 11")

  if (javastics_cmd == "JavaSticsCmd.exe" && ver > 1.8)
    warning("The installed java version is not usable with that version of
            JavaSTICS, use at most java 1.8")

  return(invisible(version_str))

}
