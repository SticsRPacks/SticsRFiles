#' Read STICS observation file (*.obs)
#'
#' @description Read STICS observation file(s) for sole and/or mixed crops.
#'
#' @param dirpath  Directory path
#' @param filename A vector of observation file name(s). Optional, see details.
#' @param mixed    (optional) Is the simulation on mixed species (boolean)
#'
#' @details If \code{filename} is not specified (or equal to \code{NULL}), the
#'          function tries first to match the \code{mod_s*} names for the same \*.obs
#'          names, and then to use the \code{.obs} file if there is one only
#'          (used for sole crops or for both mixed crops). If there are no .obs files,
#'          or two but not specified for reading, the function returns \code{NULL}
#'          If \code{mixed} is not specified (or equal to \code{NULL}), the function
#'          tries to read the number of species from the input files.
#'
#' @return A data.frameof the STICS-formated observations. Return \code{NULL} if no files were found,
#'         or more files than useable. If mixed crops (two `filename` provided), the function binds
#'         them and add a new column called "Plant" which corresponds to the name of each observation
#'         file.
#'
# @seealso \code{\link{read_output}}
#'
#' @importFrom data.table fread rbindlist
#'
#' @examples
#'\dontrun{
#'
#' obs_table = SticsRFiles:::get_obs_int()
#'
#' }
#'
#'
#' @keywords internal
#'
get_obs_int= function(dirpath=getwd(), filename=NULL, mixed= NULL){
  .=NULL # to avoid CRAN note for pipe
  if(is.null(mixed)){
    if(file.exists(file.path(dirpath,"new_travail.usm"))){
      nbplants=
        get_usm_txt(filepath = file.path(dirpath,"new_travail.usm"))$nbplantes%>%
        as.numeric
      if(nbplants>1){mixed= T}else{mixed= F}
    }else{
      if(length(list.files(dirpath)%>%.[grep("\\.obs$",.)])==1){
        # If there is only one .obs file, the value of mixed doesn't matter
        mixed=F
      }else{
        stop("mixed= NULL, there are several .obs files, and new_travail.usm",
             " cannot be found, please set the mixed parameter")
      }
    }
  }

  # If no filename is given, trying to:
  # (1) use the mod_s* names or
  # (2) use the *.obs file if there is only one
  if(is.null(filename)){
    if(mixed){
      plant_name=
        list.files(dirpath)%>%.[grep("mod_sp",.)]%>%gsub("mod_sp","",.)%>%
        strsplit(.,"\\.")%>%{if(length(.)>0){.[[1]]%>%.[1]}}
      plant_name=
        list.files(dirpath)%>%.[grep("mod_sa",.)]%>%gsub("mod_sa","",.)%>%
        strsplit(.,"\\.")%>%{if(length(.)>0){.[[1]]%>%.[1]}}%>%c(plant_name,.)
    }else{
      plant_name=
        list.files(dirpath)%>%.[grep("mod_s",.)]%>%gsub("mod_s","",.)%>%
        strsplit(.,"\\.")%>%{if(length(.)>0){.[[1]]%>%.[1]}}
    }

    # If the *.obs names are the same used for mod_s* files, read them accordingly...
    if(all(file.exists(file.path(dirpath,paste0(plant_name,".obs"))))){
      obs_table= lapply(plant_name, function(x){
        out= data.table::fread(file.path(dirpath,paste0(x,".obs")), data.table = F)
        out[out<=-999.00]= NA
        out$Plant= x
        colnames(out)= var_to_col_names(colnames(out))
        return(out)
      })
      obs_table= data.table::rbindlist(obs_table, fill=T)
      attrfiles= plant_name
      warning("Observation file names read from matching mod_s* file names.\nmod_s* names:",
              plant_name, "\n*.obs:",paste0(plant_name,".obs"))
    }else{
    # ...else try to read a single *.obs file (multiple .obs file are not allowed)
      obs_files= list.files(dirpath)%>%.[grep("\\.obs$",.)]
      if(length(obs_files)==1){
        obs_table= data.table::fread(file.path(dirpath,obs_files), data.table = F)
        colnames(obs_table)= var_to_col_names(colnames(obs_table))
        attrfiles= obs_files
        obs_table[obs_table<=-999.00]= NA
        obs_table$Plant= obs_files
        warning("Observation file guessed from the only '.obs' file in dirpath",
                plant_name, "\n*.obs:",paste0(plant_name,".obs"))
      }else{
        warning("\nObservation file names do not match mod_s* file names and several *.obs ",
             "file names are present. Please provide the *.obs file names using the ",
             "filename parameter")
        obs_table= NULL
      }
    }
  }else{
    obs_table= lapply(filename, function(x){
      out= data.table::fread(file.path(dirpath,x), data.table = F)
      out[out<=-999.00]= NA
      out$Plant= x
      colnames(out)= var_to_col_names(colnames(out))
      return(out)
    })
    obs_table= data.table::rbindlist(obs_table, fill=T)
    attrfiles= filename
  }

  if(!is.null(obs_table)){
    Date= data.frame(Date=as.POSIXct(x = paste(obs_table$ian,obs_table$mo,obs_table$jo, sep="-"),
                                     format = "%Y-%m-%d", tz="UTC"))
    obs_table= cbind(Date,obs_table)
    attr(obs_table, "file")= attrfiles
  }

  return(obs_table)
}
