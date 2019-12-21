#' Set (replace) STICS input file parameters
#'
#' @description Replace or set an input parameter from a pre-existing STICS input
#'              file.
#'
#' @param dirpath  USM directory path
#' @param filepath Path to the parameter file
#' @param param    Parameter name
#' @param value    New parameter value
#' @param plant    Plant index. Optional, only for plant or technical parameters
#' @param vars     Vector of variable names for STICS output requirements
#' @param add      Boolean. Append input to existing file (add to the list)
#' @param variety  Integer. The plant variety to get the parameter from.
#'
#' @details The \code{plant} parameter can be either equal to \code{1}, \code{2} for
#'          the associated plant in the case of intercrop, or \code{c(1,2)} for both
#'          Princiapl and associated plants.
#'          \code{\link{all_out_var}} is a helper function that returns all possible
#'          output variables.
#'
#' @note \code{set_out_var_txt} is not used by \code{set_param_txt}. To replace the output
#'       variables required from STICS, please directly call \code{set_out_var_txt}.
#'
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#'\dontrun{
#' # Replace the interrow distance parameter to 0.01:
#'
#' library(SticsRFiles)
#' set_param_txt(dirpath = "stics_usm/usm_1", param= "interrang", value= 0.01)
#'
#'}
#'
#' @export
set_param_txt= function(dirpath=getwd(),param,value,add=F,plant=1){
  param_val= get_param_txt(dirpath = dirpath, param = param)

  file_type=
    lapply(strsplit(names(param_val),"\\."), function(x){x[1]})%>%
    unlist%>%unique

  if(length(file_type)>1){
    stop("Parameter found in several files:",paste(file_type,collapse = ", "),
         "\nPlease use the set_* functions directly to set the parameter value.")
  }
  switch(file_type,
         ini= {
           set_ini_txt(filepath = file.path(dirpath,"ficini.txt"),
                   param = param, value = value, add= add)
         },
         general= {
           set_general_txt(filepath = file.path(dirpath,"tempopar.sti"),
                       param = param, value = value, add= add)
         },
         tmp= {
           set_tmp_txt(filepath = file.path(dirpath,"tempoparV6.sti"),
                   param = param, value = value, add= add)
         },
         soil= {
           set_soil_txt(filepath = file.path(dirpath,"param.sol"),
                    param = param, value = value)
         },
         usm= {
           set_usm_txt(filepath = file.path(dirpath,"new_travail.usm"),
                   param = param, value = value)
         },
         station= {
           set_station_txt(filepath = file.path(dirpath,"station.txt"),
                       param = param, value = value, add= add)
         },
         tec= {
           tmp= lapply(plant, function(x){
             set_tec_txt(filepath = file.path(dirpath,paste0("fictec",x,".txt")),
                     param = param, value = value, add= add)
           })
         },
         plant= {
           variety=
             get_param_txt(dirpath = dirpath, param = "variete")[plant]%>%
             as.numeric()

           tmp= lapply(plant, function(x){
             set_plant_txt(filepath = file.path(dirpath,paste0("ficplt",x,".txt")),
                       param = param, value = value, add= add, variety = variety[x])
           })
         },
         stop("Parameter not found")
  )

}


#' @rdname set_param_txt
#' @export
set_usm_txt= function(filepath="new_travail.usm",param,value,add=F){
  set_file_txt(filepath,param,value,add)
}

#' @rdname set_param_txt
#' @export
set_station_txt= function(filepath="station.txt",param,value,add=F){
  set_file_txt(filepath,param,value,add)
}


#' @rdname set_param_txt
#' @export
set_ini_txt= function(filepath= "ficini.txt",param,value,add=F){
  set_file_txt(filepath,param,value,add)
}


#' @rdname set_param_txt
#' @export
set_general_txt= function(filepath= "tempopar.sti",param,value,add=F){
  set_file_txt(filepath,param,value,add)
}

#' @rdname set_param_txt
#' @export
set_tmp_txt= function(filepath= "tempoparv6.sti",param,value,add=F){
  set_file_txt(filepath,param,value,add)
}

#' @rdname set_param_txt
#' @export
set_plant_txt= function(filepath="ficplt1.txt",param,value,add=F,variety= NULL){
  set_file_txt(filepath,param,value,add,variety)
}

#' @rdname set_param_txt
#' @export
set_tec_txt= function(filepath="fictec1.txt",param,value,add=F){
  set_file_txt(filepath,param,value,add)
}

#' @rdname set_param_txt
#' @export
set_soil_txt= function(filepath="param.sol",param,value){
  ref= get_soil_txt(filepath)
  param= paste0(param,"$")
  if(length(ref[[param]])!=length(value)){
    stop(paste("Length of input value different from parameter value length.\n",
               "Original values:\n",paste(ref[[param]],collapse= ", "),
               "\ninput:\n",paste(value,collapse= ", ")))
  }
  ref[[param]]= format(value, scientific=F)

  writeLines(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_typsol,
                   ref$P_argi,ref$P_Norg,ref$P_profhum,ref$P_calc,
                   ref$P_pH,ref$P_concseuil,ref$P_albedo,ref$P_q0,
                   ref$P_ruisolnu,ref$P_obstarac,ref$P_pluiebat,
                   ref$P_mulchbat,ref$P_zesx,ref$P_cfes,
                   ref$P_z0solnu ,ref$P_CsurNsol,ref$P_penterui),
             filepath)

  write(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_codecailloux,ref$P_codemacropor,
              ref$P_codefente,ref$P_codrainage,ref$P_coderemontcap,
              ref$P_codenitrif,ref$P_codedenit),
        filepath,append = T)

  write(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_profimper,ref$P_ecartdrain,ref$P_ksol,
              ref$P_profdrain,ref$P_capiljour,ref$P_humcapil,
              ref$P_profdenit,ref$P_vpotdenit),
        filepath,append = T)

  for(icou in 1:5){
    write(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_epc[icou],ref$P_hccf[icou],
                ref$P_hminf[icou],ref$P_DAF[icou],ref$P_cailloux[icou],
                ref$P_typecailloux[icou],ref$P_infil[icou],
                ref$P_epd[icou]),
          filepath,append = T)
  }
}

#' @rdname set_param_txt
#' @export
set_out_var_txt= function(filepath="var.mod",vars=c("lai(n)","masec(n)"),add= F){
  cat(vars,file=filepath, sep="\n",append = add)
}



#' Internal function to set some STICS input file parameters
#'
#' @description Replace or set an input parameter from a pre-existing STICS input
#'              file. This function is called by some of the generic \code{set_*}
#'              functions under the hood.
#'
#' @param filepath Path to the parameter file
#' @param param    Parameter name
#' @param value    New parameter value
#' @param add      Boolean. Append input to existing file (add to the list)
#'
#' @details The function uses \code{\link[base]{sys.call}} to know from which function
#'          of the \code{set_*} family it is called, so it won't work properly if called
#'          by the user directly. This is why this function is internal.
#'
#' @note This function is not used for \code{\link{set_soil_txt}}.
#'
#' @seealso \code{\link{set_param_txt}}.
#'
#' @keywords internal
#' @export
set_file_txt= function(filepath,param,value,add,variety= NULL){
  # access the function name from which set_file_txt was called
  type= strsplit(deparse(sys.call(-1)),split = "\\(")[[1]][1]
  params= readLines(filepath)
  param_= paste0(param,"$")
  switch(type,
         set_usm_txt = {
           ref= get_usm_txt(filepath)
           if(grep(param_,names(ref))<grep("P_fplt",names(ref))){
             ref_index= grep(param_,names(ref))*2
           }else{
             ref_index= grep(gsub("P_","",param_),params)+1
           }
         },
         set_station_txt= {
           ref= get_station_txt(filepath)
           ref_index= grep(param_,names(ref))*2
         },
         set_ini_txt= {
           ref= get_ini_txt(filepath)
           ref_index= grep(param_,names(ref))*2
         },
         set_plant_txt= {
           ref_index= grep(gsub('P_','',param_),params)+1
           if(!base::is.null(variety)){
             ref_index= ref_index[variety]
           }
         },
         # Default here
         {
           ref_index= grep(gsub('P_','',param_),params)+1
         }
  )

  if(!length(ref_index)>0){
    if(add){
      value= paste(value, collapse = " ")
      params= c(params,param,value)
      ref_index= grep(gsub('P_','',param_),params)+1
    }else{
      stop(paste(param,"parameter not found in:\n",filepath))
    }
  }else{
    if(add){
      stop(paste("Parameter",param,"already present in the file, try to replace its value",
                 "instead of adding the parameter"))
    }
  }

  if(length(ref_index)!=length(value)){
    stop(paste("Length of input value different from parameter value length.\n",
               "Original values:\n",paste(params[ref_index],collapse= ", "),
               "\ninput:\n",paste(value,collapse= ", ")))
  }
  params[ref_index]= format(value, scientific=F)
  writeLines(params,filepath)
}
