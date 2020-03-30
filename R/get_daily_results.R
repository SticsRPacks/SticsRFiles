#' @title Loading Stics daily output file(s)
#' @description Reading daily file(s) (mod_s*.sti) as a tibble with possible selection
#' on variable names, cumulative DOY, dates list.
#' @param workspace Stics or JavaStics workspace path containing the `mod_s*.sti` files (see details)
#' @param usm_name vector of usm(s) names
#' @param var_list vector of output variables names (optional)
#' @param doy_list vector of cumulative DOYs (optional)
#' @param dates_list list of dates (optional)
#' @param mixed    value (recycled) or vector of. `TRUE`: intercrop, `FALSE`: sole crops (default), `NULL`: guess from XML files.
#' @param usms_file The name of the usms file (e.g. "usms.xml") in case of `NULL` values in `mixed`.
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
                              usms_file= "usms.xml") {
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

  usms_file= normalizePath(file.path(workspace,usms_file), mustWork = FALSE)

  if(is.null(mixed)){
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

    plant_xml= try(get_param_xml(xml_file = usms_file, param_name = "fplt",
                                 select = "usm", usm_name)[[1]])

    if(inherits(plant_xml,"try-error")){
      plant_xml= c("plant_1","plant_2")
      warning("Error reading usms file, using dummy plant file names")
    }

    plant_names=
      try(
        lapply(plant_xml, function(x){
          get_param_xml(xml_file = normalizePath(file.path(workspace,"plant",x)),
                        param_name = "codeplante")[[1]]
        })%>%unlist()
      )

    if(inherits(plant_names,"try-error")){
      plant_names= plant_xml
      warning("Error reading plant names, using plant file names instead")
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
              file.path(workspace,paste0("mod_s",usm_name,".sti")))
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

  # selecting variables columns
  if(!is.null(var_list)){
    results_tbl <-
      results_tbl%>%
      dplyr::select(c("ian", "mo","jo", "jul"),dplyr::one_of(var_list))
  }

  # Adding the Date  in the simulation results tibble
  results_tbl <-
    results_tbl%>%
    dplyr::mutate(Date=as.POSIXct(x = paste(.data$ian,.data$mo,.data$jo,sep="-"),
                                  format = "%Y-%m-%d",tz="UTC"))%>%
    dplyr::select(.data$Date, dplyr::everything())

  # Converting .n. to _n in variable names to be homogeneous with get_obs_int
  # output colnames
  colnames(results_tbl)= var_to_col_names(colnames(results_tbl))

  return(results_tbl)
}
