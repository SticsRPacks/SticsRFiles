#' Generate html help from functions Rd help files
#'
#' @param pkg Package name
#' @param links_level integer, 0: inside the given package, 1: with base and
#' recommended packages, 2: other packages specified by .libPaths()
#' @param topic Optional, selecting specific topics with their names vector (functions names)
#' @param out_dir Optional, where to store html functions help files
#' @param overwrite Optional, logical TRUE for overwriting hmtl files (default), FALSE otherwise
#'
#' @return An invisible character vector of file paths
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   static_help("SticsRFiles")
#'   static_help(pkg = "SticsRFiles", out_dir = "/path/to/out/dir")
#' }
static_help = function(pkg,
                       links_level = 0,
                       topic = NULL,
                       out_dir = getwd(),
                       overwrite = TRUE) {

  if (!dir.exists(out_dir)) dir.create(out_dir)


  pkg_dir <- find.package(pkg)

  links <- tools::findHTMLlinks(pkgDir = pkg_dir, level = links_level)
  pkgRdDB = tools:::fetchRdDB(file.path(pkg_dir, 'help', pkg))
  force(links); topics = names(pkgRdDB)

  if (!base::is.null(topic)){
    topic_idx <- topics %in% topic
    topics <- topics[topic_idx]
  }

  files_path <- file.path(out_dir, paste(topics, 'html', sep = '.'))

  for (f in files_path) {

    if(file.exists(f) && ! overwrite) next
    p <- gsub(pattern = "\\.html$", x = basename(f), replacement = "")
    #cat(sprintf("topic : %s, file: %s\n",p,f))

    tools::Rd2HTML(pkgRdDB[[p]],f ,
                   package = pkg, Links = links, no_links = is.null(links))
  }
  return(invisible(files_path))
}


get_from_help <- function(html_file, tag1 = "Usage", tag2 = "Example") {
  l <- readLines( html_file )
  l1 <- grep(pattern = tag1,x = l)
  l2 <- grep(pattern = tag2,x = l) - 2
  return(l[l1:l2])
}
