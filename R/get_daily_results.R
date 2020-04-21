#' @title Loading Stics daily output file(s)
#' @description Reading daily file(s) (mod_s*.sti) as a tibble with possible selection
#' on variable names, cumulative DOY, dates list.
#' @param workspace Stics or JavaStics workspace path containing the `mod_s*.sti` files (see details)
#' @param usm_name vector of usm(s) names
#' @param var_list vector of output variables names (optional, see `get_var_info()` to get the names of the variables)
#' @param doy_list vector of cumulative DOYs (optional)
#' @param dates_list list of dates (optional)
#' @param mixed    value (recycled) or vector of. `TRUE`: intercrop, `FALSE`: sole crops (default), `NULL`: guess from XML files.
#' @param usms_file usms file path (e.g. "usms.xml") in case of `NULL` values in `mixed`.
#' @param javastics_path JavaStics installation path (Optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace)
#'
#' @details The function can guess if the usm(s) are mixed or not by reading the XML
#' input files. To do so, set the `mixed` argument to `NULL`.
#'
#' @return A tibble or a list of
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file(file.path("extdata","sti","V9.0"), package = "SticsRFiles")
#' get_daily_results(path,"banana")
#'
#' @export
#'
get_daily_results <- function(workspace,
                              usm_name,
                              var_list=NULL,
                              doy_list=NULL,
                              dates_list=NULL,
                              mixed= rep(FALSE,length(usm_name)),
                              usms_file= "usms.xml",
                              javastics_path = NULL) {
  .= NULL

  if(length(mixed)>1 & length(mixed)!=length(usm_name)){
    stop("The 'mixed' argument must either be of length one or length(usm_name)")
  }

  # TODO:
  # - manage filtering for multiple files : potentially filters may be different
  # for each file.


  # Getting outputs for multiple usms
  if(length(usm_name) > 1){

    if(is.null(mixed)){
      mixed= "NULL"
    }

    results_tbl_list <-
      mapply(function(x,y){
        get_daily_results(workspace,
                          x,
                          var_list = var_list,
                          doy_list = doy_list,
                          dates_list = dates_list,
                          mixed = if(y=="NULL"){NULL}else{y},
                          usms_file = usms_file)
      },
      x= usm_name, y= mixed,SIMPLIFY = FALSE)

    names(results_tbl_list) <- usm_name
    return(results_tbl_list)
  }

  # Checking usms file
  usms_files = c(suppressWarnings(normalizePath(file.path(workspace,usms_file), mustWork = FALSE)),
                 usms_file)
  usms_file_exists <- file.exists(usms_files)
  usms_file <- usms_files[usms_file_exists]

  if(is.null(mixed)){

    if ( !any(usms_file_exists) ) {
      stop("Unable to find an usms.xml file in the workspace directory.\n",
           "Please consider to set a valid usms.xml path in usms_file for getting information about intercrop usms !")
    }

    # Keeping usms.xml from the workspace (default)
    if ( all(usms_file_exists) ) usms_file <- usms_file[1]

    # Try to guess if it is a mixture or not
    nb_plant= try(get_plants_nb(usm_xml_path = usms_file, usms_list = usm_name))

    if(inherits(nb_plant,"try-error")){
      stop("Unable to guess if the usm is an intercrop. Please set mixed to TRUE or FALSE")
    }

    if(nb_plant==1){
      mixed= FALSE
    }else{
      mixed= TRUE
    }
  }

  if(mixed){

    plant_xml= try(get_param_xml(xml_file = usms_file[1], param_name = "fplt",
                                 select = "usm", usm_name)[[1]])

    if(inherits(plant_xml,"try-error")){
      plant_xml= c("plant_1","plant_2")
      warning("Error reading usms file, using dummy plant file names")
    }

    if(is.null(javastics_path)){
      plt_path <- file.path(workspace, "plant")
      if(!dir.exists(plt_path)){
        warning("plant folder not found in the workspace, please add javastics_path to use the plant folder",
                "from javaStics.")
      }
    }else{
      plt_path <- try(normalizePath(file.path(workspace, "plant")))
    }

    plant_names=
      try(
        lapply(plant_xml, function(x){
          get_param_xml(xml_file = normalizePath(file.path(plt_path,x)),
                        param_name = "codeplante")[[1]]
        })%>%unlist()
      )

    if(inherits(plant_names,"try-error")){
      plant_names= plant_xml
      warning("Error reading plant names, using plant file names for the output instead")
    }

    Table_1 =
      try(data.table::fread(file.path(workspace,paste0("mod_sp",usm_name,".sti")),
                            data.table = FALSE))

    if(inherits(Table_1,"try-error")){
      warning("Error reading output file :",
              file.path(workspace,paste0("mod_sp",usm_name,".sti")))
      return()
    }

    Table_2 =
      try(data.table::fread(file.path(workspace,paste0("mod_sa",usm_name,".sti")),
                            data.table = FALSE))

    if(inherits(Table_2,"try-error")){
      warning("Error reading output file :",
              file.path(workspace,paste0("mod_sa",usm_name,".sti")))
      return()
    }
    Table_1$Plant = plant_names[1]
    Table_2$Plant = plant_names[2]
    Table_1$Dominance = "Principal"
    Table_2$Dominance = "Associated"

    results_tbl =
      dplyr::bind_rows(Table_1, Table_2)%>%
      dplyr::group_by(.data$Dominance)%>%
      dplyr::mutate(cum_jul= compute_doy_cumul(.data$jul, .data$ian))%>%
      dplyr::ungroup()

  }else{
    results_tbl =
      try(data.table::fread(file.path(workspace,paste0("mod_s",usm_name,".sti")),
                            data.table = FALSE)%>%dplyr::as.tbl())
    if(inherits(results_tbl,"try-error")){
      warning("Error reading output file :",
              file.path(workspace,paste0("mod_s",usm_name,".sti")),
              "\n Perhaps usm ", usm_name,
              " is an intercropping one, try adding in function inputs mixed = TRUE")
      return()
    }

    # Add cum_jul to table (cumulative DOY)

    results_tbl <-
      results_tbl%>%
      dplyr::mutate(cum_jul= compute_doy_cumul(.data$jul, .data$ian))
  }

  # filtering dates
  # only on cum_jul
  if(!is.null(doy_list)){
    results_tbl <-
      results_tbl%>%
      dplyr::filter(.data$cum_jul %in% doy_list)
  }

  # Converting (n) to _n in variable names to be homogeneous with get_obs_int
  # output colnames
  colnames(results_tbl)= var_to_col_names(colnames(results_tbl))

  # selecting variables columns
  if(!is.null(var_list)){
    results_tbl <-
      results_tbl%>%
      dplyr::select(c("ian", "mo","jo", "jul"),dplyr::one_of(var_to_col_names(var_list)))
  }

  # Adding the Date  in the simulation results tibble
  results_tbl <-
    results_tbl%>%
    dplyr::mutate(Date=as.POSIXct(x = paste(.data$ian,.data$mo,.data$jo,sep="-"),
                                  format = "%Y-%m-%d",tz="UTC"))%>%
    dplyr::select(.data$Date, dplyr::everything())



  return(results_tbl)
}
