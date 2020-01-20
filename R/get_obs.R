#' Read STICS observation files *.obs and store data into a list
#'
#' @description Read STICS observation files for sole crops and store data into a list
#'
#' @param dirpath Directory path
#' @param obs_filenames A vector of observation file name(s). Optional, see details.
#' @param usms A vector of usm name(s) corresponding the the obs files. Optional, see details.
#' @param usms_filename name of the usm file. Optional, see details.
#'
#' @details If `filename` is not specified (or equal to `NULL`), the
#' function read all obs files in the `dirpath` folder. If there are no .obs files,
#' the function returns `NULL`
#' If usms names are not provided they are supposed to have the same name as the obs file.
#' If usms_filename is not provided, existence of usms are checked in the usms.xml file.
#'
#'
#' @return A list of STICS-formated observations. Return `NULL` if no files were found.
#'
#'
#'
#' @examples
#'
#' \dontrun{
#path <- system.file(file.path("extdata","obs","V9.0"), package = "SticsRFiles")
#Meas <- get_obs(path)
#' }
#'
#' @export
#'
get_obs= function(dirpath=getwd(), obs_filenames=NULL, usms=NULL, usms_filename="usms.xml"){
  .=NULL # to avoid CRAN note for pipe

  #
  # Does not work for mixed crops for the moment
  # To reconsider (revise get_obs_int) when the definitive data structure for storing obs will be defined.
  # get_obs_int searches obs files comparing with mod_s*** files if filename is not provided which means that Stics
  # must have been run before ... could this be changed and still work with mixed crops?
  #

  # For a list of folders recurive call
  if ( length(dirpath) > 1) {
    obs_list <- lapply(dirpath, function(x) get_obs(x,obs_filenames, usms, usms_filename))
    n <- unlist(lapply(obs_list, function(x) names(x[1])))
    obs_list <- lapply(obs_list, function(x) x[[1]])
    names(obs_list) <- n
    return(obs_list)
  }

  if (base::is.null(obs_filenames)) {
    obs_filenames=list.files(dirpath)%>%.[grep("\\.obs$",.)]
  }
  if (length(obs_filenames)==0) {
    warning("No observations files in the given directory ",dirpath)
    return(NULL)
  }
  obs_df=get_obs_int(dirpath, obs_filenames, mixed=FALSE)
  if (base::is.null(obs_df)) {
    return(NULL)
  }

  # if usms not provided, suppose that usms names are obs filenames prefix
  obs_filenames_prefix=sapply(obs_filenames,function (x) unlist(strsplit(x,"\\."))[1])
  if (base::is.null(usms)) {
    usms=obs_filenames_prefix
  } else if (length(usms)!=length(obs_filenames_prefix)) {
    stop("Size of usms list given in argument is different from size of obs files list => correspondance cannot be done.")
  }
  # check usms existence
  flag_exist=sapply(usms,function (x) is.element(x,get_usms_list(dirpath,usms_filename)[[1]]))
  if (any(!flag_exist)) {
    stop(paste0("USMs ",usms[!flag_exist]," are not listed in file ",usms_filename,", please modify or provide usms argument."))
  }

  obs_list=vector("list", length(usms))

  # obs_df is splitted per USM and only columns containing Stics variables and not only NA are extracted
  # not optimal of course ... will be done differently when get_obs_int will be rewritten
  obs_list=lapply(obs_filenames, function (x) within(obs_df[obs_df$Plant==x,],rm("ian","mo","jo","jul","Plant")))
  for (iusm in 1:length(obs_list)) {
    obs_list[[iusm]]=obs_list[[iusm]][,sapply(1:ncol(obs_list[[iusm]]),function (x) any(!is.na(obs_list[[iusm]][,x])))]
    obs_list[[iusm]]=obs_list[[iusm]][sapply(1:nrow(obs_list[[iusm]]),function (x) any(!is.na(obs_list[[iusm]][x,]))),]
  }
  names(obs_list)=usms

  return(obs_list)
}
