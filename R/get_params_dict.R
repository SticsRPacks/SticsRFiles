#' Getting an intern dictionnary or merging it with an given dictionnary
#' given as argument
#'
#' @param in_dict a named list with parameters names as values
# @param javastics_dir a JavaStics path
#'
#' @return A named list with XML parameters names
#'
#@keywords internal
#'
#' @export
#' @examples
#' \dontrun{
#' # Getting internal dictionnary default content
#' # get_param_dict()
#'
#' # Giving a new dictionnary
#' in_dict <- list(name1 = "param_name1", name2 = "param_name2")
#'
#' #get_params_dict(in_dict)
#'
#' # Giving a new dictionnary with common values with the internal one
#' in_dict <- list(name1 = "amount", name2 = "julapI_or_sum_upvt")
#'
#' #get_params_dict(in_dict)
#'
#' }
get_params_dict <- function(in_dict = NULL) { #, javastics_dir = NULL) {

  # TODO: replace get_params_from_table with a call
  # to the future function that will load the correspondance table (inputs.csv ?)
  # file when it will contain the XML parameters names correspondence
  # or if specifying a dict type as files types i.e. tec, ini,...
  # that will allow to load parameters names from anb XML file
  # (regarding to the Stics version)
  #if ( base::is.null(in_dict)) return(get_params_from_table())

  base_dict <- list(julapI="julapI_or_sum_upvt",doseI="amount",
               julapN="julapN_or_sum_upvt",doseN="absolute_value/%")

  # Returning intern dict
  if (base::is.null(in_dict)) return(base_dict)

  checked <- check_dict(in_dict = in_dict)

  #checked <- TRUE
  if (!checked) stop("A least one XML parameter name does not match the reference list ! ")

  # Merging the in_dict and the intern dict
  #new_dict <- set_params_dict(in_dict = in_dict, javastics_dir = NULL )

  new_dict <- merge_dict(in_dict, base_dict)

  return(new_dict)
}


merge_dict <- function(in_dict, base_dict) {

  # We suppose here that all XML parameters names of the in_dict list
  # have been previously checked in set_param_dict

  # Calculate in_dict XML parameters names indices in base_dict
  in_names <- unlist(in_dict, use.names = FALSE)
  base_names <- unlist(base_dict, use.names = FALSE)

  in_names_idx <- base_names %in% in_names

  # Removing correponding fields
  base_dict[in_names_idx] <- NULL

  # Adding dict lists
  new_dict <- c(base_dict, in_dict)

  return(new_dict)

}

check_dict <- function(in_dict) { #, javastics_dir, file_name = "inputs.csv") {

  # TODO: will be usefull when in inputs.csv when correspondence will
  # be integrated between code names and param names in XML files
  # to be able to check param names in XML files !!!
  # params_file <- base::file.path(javastics_dir, "config", file_name)$V1
  #
  # stics_int_names <- read.table(params_file , header = F, strip.white = T, sep = ";", stringsAsFactors = F)$V1
  #
  # in_names <- names(in_dict, use.names = F)

  # checks if fields values are unique
  #in_names <- names(in_dict)

  if (base::is.null(in_dict)) return(TRUE)

  in_values <- unlist(in_dict, use.names = F)
  #uniq_names <- length(in_names) != length(unique(in_names))
  uniq_values <- length(in_values) == length(unique(in_values))
  checked <- uniq_values
  if (!checked) return(FALSE)


  return(TRUE)

}
