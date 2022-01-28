#' Read STICS input parameters from text files
#'
#' @description Read STICS model input parameters from a usm in text format (STICS input)
#' Generally used after calling building a usm with `JavaStics`.
#'
#' @param dirpath      USM directory path
#' @param param        Parameter name or partial name. Optional, if not provided, the function
#'                     returns an object with all parameters
#' @param variety      Either the variety name or index for plant parameters (optional, see details).
#' @param exact        Boolean indicating if the function must return results only for exact match.
#' @param ...          Further arguments to pass (for future-proofing only).
#'
#' @details If the `variety` is not given and a `param` is asked, the function will return the values
#' for the variety that is simulated in the USM by checking the `variete` parameter in the technical file.
#' If `param` is not provided by the user, the values from all varieties will be returned unless the user
#' ask for a given `variety`.
#'
#' @note Users would generally use `get_param_txt` to identify parameters names and values and pass
#' them to other functions.
#'
#' @return A parameter value, or if `param= NULL` a list of all parameters:
#'         \item{ini}{Initialization parameters}
#'         \item{general}{General parameters}
#'         \item{tec}{Technical parameters}
#'         \item{plant}{Plant parameters}
#'         \item{soil}{Soil parameters}
#'         \item{station}{Station parameters}
#'
#' @seealso `gen_varmod()`,
#'
#' @importFrom stats setNames
#'
#' @examples
#' # Read the interrow distance parameter:
#'
#' \dontrun{
#' library(SticsRFiles)
#' path = get_examples_path( file_type = "txt" )
#' get_param_txt(path, param='interrang')
#'
#' # Getting varietal values:
#'
#' # Get the leaf lifespan of the variety used in the usm:
#' get_param_txt(dirpath = path, param = "durvieF")
#' # Get the leaf lifespan of another variety available in the plant file:
#' get_param_txt(dirpath = path, param = "durvieF", variety = "Furio")
#' # To get the values for several (or all) varieties, either put all varieties:
#' varieties= c("Pactol", "Cherif", "Furio", "Dunia", "Volga", "Cecilia")
#' get_param_txt(dirpath = path, param = "durvieF", variety = varieties)
#' # Or get it from the output of the function returning all parameters:
#' get_param_txt(dirpath = path)$plant$plant1$durvieF
#' }
#'
#' @export
get_param_txt= function(dirpath= getwd(),param= NULL,variety= NULL, exact=FALSE, ...){

  dot_args= list(...) # Future-proof the function. We can add arguments now without
  # breaking it. I think for example to a "version argument" because the tec file is not
  # generic.

  ini= get_ini_txt(file.path(dirpath,"ficini.txt"))
  general= get_general_txt(file.path(dirpath,"tempopar.sti"))
  soil= get_soil_txt(file.path(dirpath,"param.sol"))
  station= get_station_txt(file.path(dirpath,"station.txt"))
  usm= get_usm_txt(file.path(dirpath,"new_travail.usm"))
  #output= get_var_mod(dirpath)
  tmp= get_tmp_txt(file.path(dirpath,"tempoparv6.sti"))

  several_fert= ifelse(tmp$option_engrais_multiple==1,TRUE,FALSE)
  several_thin= ifelse(tmp$option_thinning==1,TRUE,FALSE)
  is_pasture= ifelse(tmp$option_pature==1,TRUE,FALSE)

  tec= plant= setNames(vector(mode = "list", length = ini$nbplantes),
                       paste0("plant",1:ini$nbplantes))

  varieties=vector("list", ini$nbplantes)
  if (is.null(variety)) {
    variety=vector("list", ini$nbplantes)
  } else if (length(variety)==1) {
    variety=lapply(1:ini$nbplantes,function(x) variety)
  } else {
    variety=list(variety)
  }
  for(i in seq_len(ini$nbplantes)){
    tec[paste0("plant",i)]=
      list(get_tec_txt(filepath = file.path(dirpath,paste0("fictec",i,".txt")),
                       several_fert = several_fert, several_thin = several_thin,
                       is_pasture = is_pasture))

    varieties[[i]]= get_plant_txt(filepath = file.path(dirpath,paste0("ficplt",i,".txt")))$codevar
    tec_variety <- tec[[paste0("plant",i)]]$variete

    plant[paste0("plant",i)]=
      list(get_plant_txt(file.path(dirpath,paste0("ficplt",i,".txt")),
                         variety=
                           if(is.null(variety[[i]])){
                             if(!is.null(param)){
                               varieties[[i]][tec_variety]
                             }else{
                               NULL
                             }
                           }else{
                             #variety
                             if(is.character(variety[[i]])){
                               variety[[i]]= match(variety[[i]],varieties[[i]])
                               if(any(is.na(variety))) {
                                 cli::cli_alert_danger("Variety not found in plant file. Possible varieties are: {.val {varieties}}")
                                 return()
                               }
                               varieties[[i]][variety[[i]]]
                             } else {
                               varieties[[i]][variety[[i]]]
                             }
                           }
      ))

    # Fixes the current variety
    if (is.null(variety[[i]])) variety[[i]] <- tec_variety

  }

  parameters= list(usm= usm, ini= ini, general= general, tec= tec,
                   plant= plant, soil= soil, station= station,
                   #output= output,tmp=tmp)
                   tmp=tmp)

  # Returning the parameters list
  if(is.null(param)) return(parameters)

  parameters= unlist(parameters)
  # Fixes parameters names including indices (?)
  # For escaping braces and catching the right name
  param <- gsub(pattern = "\\(", x=param, replacement="\\\\(\\1")
  param <- gsub(pattern = "\\)", x=param, replacement="\\\\)\\1")
  if (exact) {
    parameters= parameters[grep(paste0("\\.",param,"[\\(\\)0-9]{0,}$"),names(parameters))]
  } else {
    parameters= parameters[grep(paste0(param,"[\\(\\)0-9]{0,}$"),names(parameters))]
  }

  if(length(parameters)==0){
    stop(param," parameter not found")
  }


  # Removing ending numbers
  param_names <- gsub(pattern = "[0-9]{0,}$", replacement = "", x = names(parameters))

  param_names <- gsub(pattern = ".*\\.(.*[0-9]{0,})", replacement = "\\1", x = param_names )

  idx_to_fix <- param_names %in% param
  idx_plant_to_fix <- idx_to_fix & grepl("^plant", names(parameters))

  if (any(idx_plant_to_fix)) {
    variety_names <- sapply(1:length(varieties), function(i) {varieties[[i]][variety[[i]]]})
    if(length(variety_names) == 1) variety_names <- rep(variety_names, length(idx_plant_to_fix))
    # names(parameters)[idx_plant_to_fix]= paste0(gsub("[1-999]","",names(parameters)[idx_plant_to_fix]),
    #                                       ".",
    #                                       variety_names[idx_plant_to_fix])
    names(parameters)[idx_plant_to_fix]= paste0(names(parameters)[idx_plant_to_fix],
                                                ".",
                                                variety_names[idx_plant_to_fix])
  }

  # TO BE FIXED if it makes sense ?
  # idx_tec_to_fix <- idx_to_fix & grepl("^tec", names(parameters))
  #
  # if (any(idx_tec_to_fix)) {
  #   names(parameters)[idx_tec_to_fix]= paste0(gsub("[1-999]","",names(parameters)),
  #                                               ".",
  #                                               varieties[tec_variety][idx_tec_to_fix])
  # }


  return(parameters)
}


#' Read STICS input parameters files
#'
#' @description Read a specific STICS model input parameter file.
#' Users would generally use the wrapper `get_param_txt()` instead.
#'
#' @param filepath     File path
#' @param several_fert Is there several fertilization in the USM ? See details.
#' @param several_thin Is there several thinning in the USM ? See details.
#' @param is_pasture   Is the plant a pasture ? See details.
#' @param variety      Integer. The plant variety to get the parameter from.
#' @param ...          Further arguments to pass (for future-proofing only)
#'
#' @details `several_fert`, `several_thin` and `is_pasture` are read from the tmp
#' file (`tempoparv6.sti`). `get_param_txt()` does it automatically. If you absolutely
#' need to use directly `get_tec_txt`, please see example.
#'
#'
#' @note The functions are compatible with intercrops. Users generally only use
#'  `get_param_txt()`, which is a wrapper for all these functions.
#'
#' @return A list of parameters, depending on the file/function:
#'         \item{ini}{Initialization parameters}
#'         \item{general}{General parameters}
#'         \item{tec}{Technical parameters}
#'         \item{plant}{Plant parameters}
#'         \item{soil}{Soil parameters}
#'         \item{station}{Station parameters}
#'         \item{tmp}{Temporary parameters}
#'
#' @seealso `get_param_txt()`.
#'
#' @examples
#'\dontrun{
#' # Read the initialisation file (ficini.txt):
#' library(SticsRFiles)
#' path = file.path(get_examples_path( file_type = "txt"), "ficini.txt")
#' get_ini_txt(path)
#'
#' # Read the tec file directly:
#'
#' # First, get the parameters from the tmp file:
#' tmp= get_tmp_txt(filepath = file.path(get_examples_path( file_type = "txt"), "tempoparv6.sti"))
#' several_fert= ifelse(tmp$option_engrais_multiple==1,TRUE,FALSE)
#' several_thin= ifelse(tmp$option_thinning==1,TRUE,FALSE)
#' is_pasture= ifelse(tmp$option_pature==1,TRUE,FALSE)
#'
#' # Then, get the technical parameters:
#' get_tec_txt(filepath= file.path(get_examples_path( file_type = "txt"), "fictec1.txt"),
#' several_fert = several_fert, several_thin = several_thin, is_pasture = is_pasture)
#'}
#'
#' @rdname get_param_txt
#' @export
get_ini_txt= function(filepath="ficini.txt"){
  params= get_txt_generic(filepath, names = FALSE)
  ini= list(nbplantes= params[1])
  ini= character_to_numeric_list(ini)

  return(ini)
}

#' @rdname get_param_txt
#' @export
get_general_txt= function(filepath="tempopar.sti"){
  c(nbresidus= 21, get_txt_generic(filepath))
}


#' @rdname get_param_txt
#' @export
get_tmp_txt= function(filepath="tempoparv6.sti"){
  get_txt_generic(filepath)
}

#' @rdname get_param_txt
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


#' @rdname get_param_txt
#' @export
get_tec_txt= function(filepath="fictec1.txt",several_fert,several_thin,is_pasture,...){

  dot_args= list(...) # Future-proofing the function. We can add arguments now without
  # breaking it. I think for example to a "version argument" because the tec file is not
  # generic.

  params= readLines(filepath)
  itk= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  itk$nbjres= as.numeric(val())

  if(itk$nbjres > 0){
    for(i in 1:itk$nbjres){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$julres= c(itk$julres, vec[1])
      itk$coderes= c(itk$coderes,vec[2])
      itk$qres= c(itk$qres,vec[3])
      itk$Crespc= c(itk$Crespc,vec[4])
      itk$CsurNres= c(itk$CsurNres,vec[5])
      itk$Nminres= c(itk$Nminres,vec[6])
      itk$eaures= c(itk$eaures,vec[7])
    }
  }
  itk$nbjtrav= as.numeric(val())
  if(itk$nbjtrav > 0){
    for(i in 1:itk$nbjtrav){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$jultrav= c(itk$jultrav, vec[1])
      itk$profres= c(itk$profres, vec[2])
      itk$proftrav= c(itk$proftrav, vec[3])
    }
  }

  itk$iplt0= val()
  itk$profsem= val()
  itk$densitesem= val()
  itk$variete= val()
  itk$codetradtec= val()
  itk$interrang= val()
  itk$orientrang= val()
  itk$codedecisemis= val()
  itk$nbjmaxapressemis= val()
  itk$nbjseuiltempref= val()
  itk$codestade= val()
  itk$ilev= val()
  itk$iamf= val()
  itk$ilax= val()
  itk$isen= val()
  itk$ilan= val()
  itk$iflo= val()
  itk$idrp= val()
  itk$imat= val()
  itk$irec= val()
  itk$irecbutoir= val()
  itk$effirr= val()
  itk$codecalirrig= val()
  itk$ratiol= val()
  itk$dosimx= val()
  itk$doseirrigmin= val()
  itk$codedateappH2O= as.numeric(val())
  itk$nap= as.numeric(val())

  if(itk$nap > 0){
    for(i in 1:itk$nap){
      if(itk$codedateappH2O != 1) {
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$julapI= c(itk$julapI,vec[1])
        itk$doseI= c(itk$doseI,vec[2])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$upvttapI= c(itk$upvttapI,vec[1])
        itk$doseI= c(itk$doseI,vec[2])
      }
    }
  }

  itk$codlocirrig= val()
  itk$locirrig= val()
  itk$profmes= val()

  if(!several_fert){
    itk$engrais= val()
  }else{
    # val()
  }

  itk$concirr= val()
  itk$codedateappN= as.numeric(val())
  itk$codefracappN= as.numeric(val())
  itk$Qtot_N= val()
  itk$napN= as.numeric(val())

  if(itk$napN > 0){
    for(i in 1:itk$napN){
      if(itk$codedateappN != 1) {
        if(itk$codefracappN == 1) {
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$julapN= c(itk$julapN, vec[1])
            itk$doseN= c(itk$doseN, vec[2])
            itk$engrais= c(itk$engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$julapN= c(itk$julapN, vec[1])
            itk$doseN= c(itk$doseN, vec[2])
          }
        }else{
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$julapN= c(itk$julapN, vec[1])
            itk$fracN= c(itk$fracN, vec[2])
            itk$engrais= c(itk$engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$julapN= c(itk$julapN, vec[1])
            itk$fracN= c(itk$fracN, vec[2])
          }
        }
      }else{
        if (itk$codefracappN == 1) {
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$upvttapN= c(itk$upvttapN, vec[1])
            itk$doseN= c(itk$doseN, vec[2])
            itk$engrais= c(itk$engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$upvttapN= c(itk$upvttapN, vec[1])
            itk$doseN= c(itk$doseN, vec[2])
          }
        }else{
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$upvttapN= c(itk$upvttapN, vec[1])
            itk$fracN= c(itk$fracN, vec[2])
            itk$engrais= c(itk$engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$upvttapN= c(itk$upvttapN, vec[1])
            itk$fracN= c(itk$fracN, vec[2])
          }
        }
      }
    }
  }

  itk$codlocferti= val()
  itk$locferti= val()
  itk$ressuite= val()
  itk$codcueille= val()
  itk$nbcueille= val()
  itk$cadencerec= val()
  itk$codrecolte= val()
  itk$codeaumin= val()
  itk$h2ograinmin= val()
  itk$h2ograinmax= val()
  itk$sucrerec= val()
  itk$CNgrainrec= val()
  itk$huilerec= val()
  itk$coderecolteassoc= val()
  itk$codedecirecolte= val()
  itk$nbjmaxapresrecolte= val()
  itk$codefauche= val()
  itk$mscoupemini= val()
  itk$codemodfauche= as.numeric(val())

  if(itk$codemodfauche == 1) {
    itk$lecfauche= FALSE
  }else{
    itk$lecfauche= TRUE
  }

  itk$hautcoupedefaut= val()
  itk$stadecoupedf= val()
  nbcoupe2= as.numeric(val())

  if(itk$codemodfauche == 2){
    for(i in 1:nbcoupe2){
      if(is_pasture){
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$julfauche= c(itk$julfauche, vec[1])
        itk$hautcoupe= c(itk$hautcoupe, vec[2])
        itk$lairesiduel= c(itk$lairesiduel, vec[3])
        itk$msresiduel= c(itk$msresiduel, vec[4])
        itk$anitcoupe= c(itk$anitcoupe, vec[5])
        itk$restit= c(itk$restit, vec[6])
        itk$mscoupemini= c(itk$mscoupemini, vec[7])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$julfauche= c(itk$julfauche, vec[1])
        itk$hautcoupe= c(itk$hautcoupe, vec[2])
        itk$lairesiduel= c(itk$lairesiduel, vec[3])
        itk$msresiduel= c(itk$msresiduel, vec[4])
        itk$anitcoupe= c(itk$anitcoupe, vec[5])
      }
    }
    itk$nbcoupe = nbcoupe2
  }else{
    for(i in 1:nbcoupe2){
      # val()
    }
  }

  nbcoupe3= as.numeric(val())

  if(itk$codemodfauche == 3) {
    for(i in 1:nbcoupe3){
      if(is_pasture){
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$tempfauche= c(itk$tempfauche, vec[1])
        itk$hautcoupe= c(itk$hautcoupe, vec[2])
        itk$lairesiduel= c(itk$lairesiduel, vec[3])
        itk$msresiduel= c(itk$msresiduel, vec[4])
        itk$anitcoupe= c(itk$anitcoupe, vec[5])
        itk$restit= c(itk$restit, vec[6])
        itk$mscoupemini= c(itk$mscoupemini, vec[7])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$tempfauche= c(itk$tempfauche, vec[1])
        itk$hautcoupe= c(itk$hautcoupe, vec[2])
        itk$lairesiduel= c(itk$lairesiduel, vec[3])
        itk$msresiduel= c(itk$msresiduel, vec[4])
        itk$anitcoupe= c(itk$anitcoupe, vec[5])
      }
    }
    itk$nbcoupe= nbcoupe3
  }else{
    for(i in 1:nbcoupe3){
      # val()
    }
  }

  itk$codepaillage= val()
  itk$couvermulchplastique= val()
  itk$albedomulchplastique= val()
  itk$codrognage= val()
  itk$largrogne= val()
  itk$hautrogne= val()
  itk$biorognem= val()
  itk$codcalrogne= val()
  itk$julrogne= val()
  itk$margerogne= val()
  itk$codeclaircie= val()

  if(several_thin){
    itk$nb_eclair= as.numeric(val())
    for(i in 1:itk$nb_eclair){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$juleclair=  c(itk$juleclair, vec[1])
      itk$nbinfloecl=  c(itk$nbinfloecl, vec[2])
    }
  }else{
    # vec= strsplit(x = val(),split = " ")[[1]]
    itk$nb_eclair= 1
    itk$juleclair=  val()
    itk$nbinfloecl=  val()
  }

  itk$codeffeuil= val()
  itk$codhauteff= val()
  itk$codcaleffeuil= val()
  itk$laidebeff= val()
  itk$effeuil= val()
  itk$juleffeuil= val()
  itk$laieffeuil= val()
  itk$codetaille= val()
  itk$jultaille= val()
  itk$codepalissage= val()
  itk$hautmaxtec= val()
  itk$largtec= val()
  itk$codabri= val()
  itk$transplastic= val()
  itk$surfouvre1= val()
  itk$julouvre2= val()
  itk$surfouvre2= val()
  itk$julouvre3= val()
  itk$surfouvre3= val()
  itk$codeDST= val()
  itk$dachisel= val()
  itk$dalabour= val()
  itk$rugochisel= val()
  itk$rugolabour= val()
  itk$codeDSTtass= val()
  itk$profhumsemoir= val()
  itk$dasemis= val()
  itk$profhumrecolteuse= val()
  itk$darecolte= val()
  itk$codeDSTnbcouche= val()

  # Transform into numeric:
  itk_out= character_to_numeric_list(itk)

  return(itk_out)
}


#' @rdname get_param_txt
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

  soil[c("numsol","typsol","argi","Norg","profhum","calc","pH",
         "concseuil","albedo","q0","ruisolnu","obstarac","pluiebat",
         "mulchbat","zesx","cfes","z0solnu","CsurNsol", "penterui")]= val()

  soil[c("numsol","codecailloux","codemacropor","codefente",
         "codrainage","coderemontcap","codenitrif","codedenit")]= val()

  soil[c("numsol","profimper","ecartdrain","ksol","profdrain",
         "capiljour","humcapil","profdenit","vpotdenit")]= val()

  vec= matrix(data = NA,nrow = 9, ncol = 5)
  for(i in 1:5){
    vec[,i]= val()
  }
  vec= apply(vec,MARGIN = 1,FUN = list)

  soil[c("numsol","epc","hccf","hminf","DAF",
         "cailloux","typecailloux","infil","epd")]=
    lapply(vec, unlist)

  # Transform into numeric:
  soil= character_to_numeric_list(soil)

  return(soil)
}

#' @rdname get_param_txt
#' @export
get_station_txt= function(filepath="station.txt"){
  get_txt_generic(filepath)
}


#' @rdname get_param_txt
#' @export
get_usm_txt= function(filepath="new_travail.usm"){
  get_txt_generic(filepath)
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
#' \dontrun{
#' path <- file.path(get_examples_path(file_type = "txt", version_name = "V8.5"), "station.txt")
#' get_txt_generic(path)
#' }
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
