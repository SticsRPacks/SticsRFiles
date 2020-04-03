#' Read STICS input parameters from text files
#'
#' @description Read STICS model input parameters from a pre-existing STICS input
#'              file. Generally used after calling building a usm with `JavaStics`.
#'
#' @param dirpath      USM directory path
#' @param filepath     File path
#' @param param        Parameter name. Optional, if not provided, the function
#'                     return an object with all parameters
#' @param several_fert Is there several fertilization in the USM ?
#' @param several_thin Is there several thinning in the USM ?
#' @param is_pasture   Is the plant a pasture ?
#' @param variety      Integer. The plant variety to get the parameter from.
#' @param ...          Helper to pass arguments from \code{\link{get_param_txt}} to the
#'                     other functions
#'
#' @note Users generally only use \code{get_param_txt} that identify parameters for
#'       other functions and call them.
#'
#' @return A list of all parameters:
#'         \item{ini}{Initialization parameters}
#'         \item{general}{General parameters}
#'         \item{tec}{Technical parameters}
#'         \item{plant}{Plant parameters}
#'         \item{soil}{Soil parameters}
#'         \item{station}{Station parameters}
#'         \item{output}{The output variables the user require from STICS}
#' The function can return several sub-lists for each \code{plant} and \code{tec}
#'  if mixed crops, numbered by usage
#'
#' @seealso \code{\link{set_param_txt}}.
#'
#' @importFrom stats setNames
#'
#' @examples
#'\dontrun{
#' # Read the interrow distance parameter:
#'
#' library(SticsRFiles)
#' path = system.file("extdata/txt/V9.1", package = "SticsRFiles")
#' get_param_txt(path, param='interrang')
#'
#'}
#'
#' @export
get_param_txt= function(dirpath= getwd(),param= NULL,...){
  dot_args= list(...)
  if(isTRUE(dot_args$several_fert)){
    several_fert= dot_args$several_fert
  }else{
    several_fert= F
  }
  if(isTRUE(dot_args$several_thin)){
    several_thin= dot_args$several_thin
  }else{
    several_thin= F
  }
  if(isTRUE(dot_args$is_pasture)){
    is_pasture= dot_args$is_pasture
  }else{
    is_pasture= F
  }

  ini= get_ini_txt(file.path(dirpath,"ficini.txt"))
  general= get_general_txt(file.path(dirpath,"tempopar.sti"))
  soil= get_soil_txt(file.path(dirpath,"param.sol"))
  station= get_station_txt(file.path(dirpath,"station.txt"))
  usm= get_usm_txt(file.path(dirpath,"new_travail.usm"))
  output= get_out_var_txt(file.path(dirpath,"var.mod"))
  tmp= get_tmp_txt(file.path(dirpath,"tempoparv6.sti"))
  tec= plant= setNames(vector(mode = "list", length = ini$nbplantes),
                       paste0("plant",1:ini$nbplantes))

  for(i in seq_len(ini$nbplantes)){
    tec[paste0("plant",i)]=
      list(get_tec_txt(file.path(dirpath,paste0("fictec",i,".txt")),
                       several_fert = several_fert, several_thin = several_thin,
                       is_pasture = is_pasture))
    plant[paste0("plant",i)]=
      list(get_plant_txt(file.path(dirpath,paste0("ficplt",i,".txt")),
                         variety=
                           if(!is.null(param)){
                             tec[[paste0("plant",i)]]$P_variete
                           }else{
                             NULL
                           }))
  }

  parameters= list(usm= usm, ini= ini, general= general, tec= tec,
                   plant= plant, soil= soil, station= station,
                   output= output,tmp=tmp)

  if(!is.null(param)){
    parameters= unlist(parameters)
    parameters= parameters[grep(paste0(param,"$"),names(parameters))]
    if(length(parameters)==0){
      stop(param," parameter not found")
    }
  }

  return(parameters)
}


#' Read STICS input parameters files
#'
#' @description Read a specific STICS model input parameter file.
#' Users would generally use the wrapper `get_param_txt()` instead.
#'
#' @param filepath     File path
#' @param several_fert Is there several fertilization in the USM ?
#' @param several_thin Is there several thinning in the USM ?
#' @param is_pasture   Is the plant a pasture ?
#' @param variety      Integer. The plant variety to get the parameter from.
#'
#' @details he functions are compatible with intercrops.
#'
#' @note Users generally only use `get_param_txt()`, which is a wrapper for all
#' these functions.
#'
#' @return A list of parameters, depending on the file/function:
#'         \item{ini}{Initialization parameters}
#'         \item{general}{General parameters}
#'         \item{tec}{Technical parameters}
#'         \item{plant}{Plant parameters}
#'         \item{soil}{Soil parameters}
#'         \item{station}{Station parameters}
#'
#' @seealso `get_param_txt()`.
#'
#' @examples
#'\dontrun{
#' # Read the initialisation file (ficini.txt):
#' library(SticsRFiles)
#' path = system.file("extdata/txt/V9.1/ficini.txt", package = "SticsRFiles")
#' get_ini_txt(path)
#'}
#'
#' @export
get_ini_txt= function(filepath="ficini.txt"){
  params= get_txt_generic(filepath, names = FALSE)
  ini= list(nbplantes= params[1])
  ini= character_to_numeric_list(ini)

  return(ini)
}

#' @rdname get_ini_txt
#' @export
get_general_txt= function(filepath="tempopar.sti"){
  c(nbresidus= 21, get_txt_generic(filepath))
}


#' @rdname get_ini_txt
#' @export
get_tmp_txt= function(filepath="tempoparv6.sti"){
  get_txt_generic(filepath)
}

#' @rdname get_ini_txt
#' @export
get_plant_txt= function(filepath="ficplt1.txt", variety= NULL){
  x= get_txt_generic(filepath)

  index_codevar= which(names(x)=="codevar")
  varieties= x[[index_codevar]]

  x_1= c(x[1:(index_codevar-1)], nbVariete= length(varieties)) # add nbVariete
  x_2= x[index_codevar:length(x)]                              # variety-related parameters

  # Keep only the variety asked by the user:
  if(!is.null(variety)){
    # If variety is given with name(s), find the index
    if(is.character(variety)){
      variety= match(variety,varieties)
    }
    x_2= lapply(x_2, function(x){
      x[variety]
    })
  }

  c(x_1,x_2)
}


#' @rdname get_ini_txt
#' @export
get_tec_txt= function(filepath="fictec1.txt",several_fert=T,several_thin=T,is_pasture=F){

  params= readLines(filepath)
  itk= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  itk$P_nbjres= as.numeric(val())

  if(itk$P_nbjres > 0){
    for(i in 1:itk$P_nbjres){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$P_julres= c(itk$P_julres, vec[1])
      itk$P_coderes= c(itk$P_coderes,vec[2])
      itk$P_qres= c(itk$P_qres,vec[3])
      itk$P_Crespc= c(itk$P_Crespc,vec[4])
      itk$P_CsurNres= c(itk$P_CsurNres,vec[5])
      itk$P_Nminres= c(itk$P_Nminres,vec[6])
      itk$P_eaures= c(itk$P_eaures,vec[7])
    }
  }
  itk$P_nbjtrav= as.numeric(val())
  if(itk$P_nbjtrav > 0){
    for(i in 1:itk$P_nbjtrav){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$P_jultrav= c(itk$P_jultrav, vec[1])
      itk$P_profres= c(itk$P_profres, vec[2])
      itk$P_proftrav= c(itk$P_proftrav, vec[3])
    }
  }

  itk$P_iplt0= val()
  itk$P_profsem= val()
  itk$P_densitesem= val()
  itk$P_variete= val()
  itk$P_codetradtec= val()
  itk$P_interrang= val()
  itk$P_orientrang= val()
  itk$P_codedecisemis= val()
  itk$P_nbjmaxapressemis= val()
  itk$P_nbjseuiltempref= val()
  itk$P_codestade= val()
  itk$P_ilev= val()
  itk$P_iamf= val()
  itk$P_ilax= val()
  itk$P_isen= val()
  itk$P_ilan= val()
  itk$P_iflo= val()
  itk$P_idrp= val()
  itk$P_imat= val()
  itk$P_irec= val()
  itk$P_irecbutoir= val()
  itk$P_effirr= val()
  itk$P_codecalirrig= val()
  itk$P_ratiol= val()
  itk$P_dosimx= val()
  itk$P_doseirrigmin= val()
  itk$P_codedateappH2O= as.numeric(val())
  itk$nap= as.numeric(val())

  if(itk$nap > 0){
    for(i in 1:itk$nap){
      if(itk$P_codedateappH2O != 1) {
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_julapI= c(itk$P_julapI,vec[1])
        itk$P_doseI= c(itk$P_doseI,vec[2])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_upvttapI= c(itk$P_upvttapI,vec[1])
        itk$P_doseI= c(itk$P_doseI,vec[2])
      }
    }
  }

  itk$P_codlocirrig= val()
  itk$P_locirrig= val()
  itk$P_profmes= val()

  if(!several_fert){
    itk$P_engrais= val()
  }else{
    # val()
  }

  itk$P_concirr= val()
  itk$P_codedateappN= as.numeric(val())
  itk$P_codefracappN= as.numeric(val())
  itk$P_Qtot_N= val()
  itk$napN= as.numeric(val())

  if(itk$napN > 0){
    for(i in 1:itk$napN){
      if(itk$P_codedateappN != 1) {
        if(itk$P_codefracappN == 1) {
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
          }
        }else{
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
          }
        }
      }else{
        if (itk$P_codefracappN == 1) {
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
          }
        }else{
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
          }
        }
      }
    }
  }

  itk$P_codlocferti= val()
  itk$P_locferti= val()
  itk$P_ressuite= val()
  itk$P_codcueille= val()
  itk$P_nbcueille= val()
  itk$P_cadencerec= val()
  itk$P_codrecolte= val()
  itk$P_codeaumin= val()
  itk$P_h2ograinmin= val()
  itk$P_h2ograinmax= val()
  itk$P_sucrerec= val()
  itk$P_CNgrainrec= val()
  itk$P_huilerec= val()
  itk$P_coderecolteassoc= val()
  itk$P_codedecirecolte= val()
  itk$P_nbjmaxapresrecolte= val()
  itk$P_codefauche= val()
  itk$P_mscoupemini= val()
  itk$P_codemodfauche= as.numeric(val())

  if(itk$P_codemodfauche == 1) {
    itk$lecfauche= FALSE
  }else{
    itk$lecfauche= TRUE
  }

  itk$P_hautcoupedefaut= val()
  itk$P_stadecoupedf= val()
  nbcoupe2= as.numeric(val())

  if (itk$P_codemodfauche == 2) {
    for(i in 1:nbcoupe2){
      if(is_pasture){
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_julfauche= c(itk$P_julfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
        itk$P_restit= c(itk$P_restit, vec[6])
        itk$P_mscoupemini= c(itk$P_mscoupemini, vec[7])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_julfauche= c(itk$P_julfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
      }
    }
    itk$nbcoupe = nbcoupe2
  }else{
    for(i in 1:nbcoupe2){
      # val()
    }
  }

  nbcoupe3= as.numeric(val())

  if(itk$P_codemodfauche == 3) {
    for(i in 1:nbcoupe3){
      if(is_pasture){
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_tempfauche= c(itk$P_tempfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
        itk$P_restit= c(itk$P_restit, vec[6])
        itk$P_mscoupemini= c(itk$P_mscoupemini, vec[7])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_tempfauche= c(itk$P_tempfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
      }
    }
    itk$nbcoupe= nbcoupe3
  }else{
    for(i in 1:nbcoupe3){
      # val()
    }
  }

  itk$P_codepaillage= val()
  itk$P_couvermulchplastique= val()
  itk$P_albedomulchplastique= val()
  itk$P_codrognage= val()
  itk$P_largrogne= val()
  itk$P_hautrogne= val()
  itk$P_biorognem= val()
  itk$P_codcalrogne= val()
  itk$P_julrogne= val()
  itk$P_margerogne= val()
  itk$P_codeclaircie= val()

  if(several_thin){
    itk$P_nb_eclair= as.numeric(val())
    for(i in 1:itk$P_nb_eclair){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$P_juleclair=  c(itk$P_juleclair, vec[1])
      itk$P_nbinfloecl=  c(itk$P_nbinfloecl, vec[2])
    }
  }else{
    # vec= strsplit(x = val(),split = " ")[[1]]
    itk$P_nb_eclair= 1
    itk$P_juleclair=  val()
    itk$P_nbinfloecl=  val()
  }

  itk$P_codeffeuil= val()
  itk$P_codhauteff= val()
  itk$P_codcaleffeuil= val()
  itk$P_laidebeff= val()
  itk$P_effeuil= val()
  itk$P_juleffeuil= val()
  itk$P_laieffeuil= val()
  itk$P_codetaille= val()
  itk$P_jultaille= val()
  itk$P_codepalissage= val()
  itk$P_hautmaxtec= val()
  itk$P_largtec= val()
  itk$P_codabri= val()
  itk$P_transplastic= val()
  itk$P_surfouvre1= val()
  itk$P_julouvre2= val()
  itk$P_surfouvre2= val()
  itk$P_julouvre3= val()
  itk$P_surfouvre3= val()
  itk$P_codeDST= val()
  itk$P_dachisel= val()
  itk$P_dalabour= val()
  itk$P_rugochisel= val()
  itk$P_rugolabour= val()
  itk$P_codeDSTtass= val()
  itk$P_profhumsemoir= val()
  itk$P_dasemis= val()
  itk$P_profhumrecolteuse= val()
  itk$P_darecolte= val()
  itk$P_codeDSTnbcouche= val()

  # Transform into numeric:
  itk_out= character_to_numeric_list(itk)
  # Two parameters are not numeric, resetting them
  # to their original value:
  itk_out$P_stadecoupedf= itk$P_stadecoupedf
  itk_out$P_ressuite= itk$P_ressuite

  return(itk_out)
}


#' @rdname get_ini_txt
#' @export
get_soil_txt= function(filepath= "param.sol"){

  params= readLines(filepath,warn=F)
  soil= vector(mode='list', length = 0)

  index= 1
  val= function(){
    index<<- index+1
    vec= strsplit(x = params[index-1],split = " ")[[1]]
    vec= vec[vec!=""]
    return(vec)
  }

  soil$nbcouchessol_max= 1000

  soil[c("P_numsol","P_typsol","P_argi","P_Norg","P_profhum","P_calc","P_pH",
         "P_concseuil","P_albedo","P_q0","P_ruisolnu","P_obstarac","P_pluiebat",
         "P_mulchbat","P_zesx","P_cfes","P_z0solnu","P_CsurNsol", "P_penterui")]= val()

  soil[c("P_numsol","P_codecailloux","P_codemacropor","P_codefente",
         "P_codrainage","P_coderemontcap","P_codenitrif","P_codedenit")]= val()

  soil[c("P_numsol","P_profimper","P_ecartdrain","P_ksol","P_profdrain",
         "P_capiljour","P_humcapil","P_profdenit","P_vpotdenit")]= val()

  vec= matrix(data = NA,nrow = 9, ncol = 5)
  for(i in 1:5){
    vec[,i]= val()
  }
  vec= apply(vec,MARGIN = 1,FUN = list)

  soil[c("P_numsol","P_epc","P_hccf","P_hminf","P_DAF",
         "P_cailloux","P_typecailloux","P_infil","P_epd")]=
    lapply(vec, unlist)

  # Transform into numeric:
  soil= character_to_numeric_list(soil)

  return(soil)
}

#' @rdname get_ini_txt
#' @export
get_station_txt= function(filepath="station.txt"){
  get_txt_generic(filepath)
}


#' @rdname get_ini_txt
#' @export
get_usm_txt= function(filepath="new_travail.usm"){
  get_txt_generic(filepath)
}



#' Get desired STICS outputs
#'
#' @description Get the variables STICS will return ("var.mod" file)
#'
#' @param filepath The path to the "var.mod" file.
#'
#' @return The variables that will be returned by STICS
#' @export
#'
#' @seealso `gen_varmod`
#' @examples
#' get_out_var_txt(file.path(system.file("extdata/txt/V8.5", package = "SticsRFiles"),"var.mod"))
#'
get_out_var_txt= function(filepath="var.mod"){
  Vars= readLines(filepath)
  return(Vars)
}


#' Read parameter values from file
#'
#' @description Generic function to read STICS parameter files
#'
#' @param filepath The path to the parameter file
#' @param names    Boolean, read the parameter names ?
#'
#' @return A named (if names=TRUE) list of parameter values
#' @keywords internal
#'
#' @examples
#' get_txt_generic(file.path(system.file("extdata/txt/V8.5", package = "SticsRFiles"),"station.txt"))
#'
get_txt_generic= function(filepath, names=TRUE){
  params= readLines(filepath)

  x= as.list(params[!seq_along(params)%%2])
  if(names){
    names(x)= gsub(":","",params[!!seq_along(params)%%2])
  }

  is_dupli= duplicated(names(x))
  dupli_names= unique(names(x)[is_dupli])

  # Remove duplicated names if any, and put the values as a vector instead
  for(i in dupli_names){
    index_dupli= which(names(x)==i)
    x[[index_dupli[1]]]= unlist(x[index_dupli], use.names = FALSE)
    x= x[-index_dupli[-1]]
  }

  character_to_numeric_list(x)
}



#' Character list to numeric
#'
#' @description Tries to convert the values in a list into numeric values,
#' and if it fails, return as character.
#'
#' @param z A list with potential numeric values written a characters
#'
#' @return A list with numeric values when possible
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' test= list(a= "2", b="toto")
#' character_to_numeric_list(test)
#' }
#'
character_to_numeric_list= function(x){
  lapply(x, function(z){
    y= suppressWarnings(as.numeric(z))
    if(any(is.na(y))){z}else{y}
  })
}
