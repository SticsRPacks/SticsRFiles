#' Getting the stics environment object
#'
#' @return stics environment
#@export
#'
# @examples
sticsenv <- function() {
  return(stics_env())
}


stics_env <- function(name = NULL, env = .GlobalEnv, create = TRUE) {

  exists_stics <- stics_exists()

  if (exists_stics && base::is.null(name)) return(get("stics",envir = globalenv(), inherits = FALSE))

  if (!exists_stics && create) {
    assign(
      x = "stics",
      value = new.env(parent = .GlobalEnv),
      pos = .GlobalEnv
    )

    stics_set_name(name = "stics", env = ".GlobalEnv")
  }


  if (base::is.null(name) || name == "stics") {
    return(get("stics", envir = .GlobalEnv))
  }


  exists_name <- exists(name, envir = stics, inherits = FALSE)

  if (exists_name) return(get(name, envir = stics, inherits = FALSE))

  if (create) {
    assign(
      x = name,
      value = new.env(parent = stics),
      pos = stics
    )

    stics_set_name(name = name, env = "stics")
  }

  return(get(name, envir = stics))
}


stics_set_name <- function(name, env = "stics", fix_name = NULL) {

  if (!stics_exists(name = name, env_name = env )) return(invisible(FALSE))

  envir <- get(env, envir = .GlobalEnv)

  env_str <- "stics"
  if (name != "stics") env_str <- "stics$name"

  # Getting name
  env_name <- eval(parse(text = paste0("environmentName(",env_str,")")))
  if ( ! env_name == "" ) {
    warning(name, "environment already named ", env_name)
    return(FALSE)
  }

  if ( base::is.null(fix_name)) {
    env_name <- paste0("R_", name)
  } else {
    env_name <- fix_name
  }

  xpression <- paste0("attr(", name, ",\"name\") <- \"", env_name, "\"")

  r <- eval(parse(text = xpression), envir = envir)
}


stics_get_name <- function(name = NULL, env = "stics") {

  if (!stics_exists(name = name, env_name = env )) return(invisible())

  return(environmentName(env = stics_get(name)))

}


stics_ls <- function(name = NULL, detail = FALSE) {

  envir <- stics_get(name)

  if( base::is.null(envir) ) return()

  if (detail) {
    return(utils::ls.str(envir))
  }

  return(ls(envir))
}


stics_exists <- function(name = NULL, env_name = "stics") {
  exists_stics <- exists(x = "stics", envir = .GlobalEnv, inherits = FALSE)

  if (base::is.null(name) || name =="stics") {
    return(exists_stics)
  }

  if (!exists_stics) {
    return(FALSE)
  }

  # name contains a env name
  val <- eval(parse( text = paste0("stics","$",name)))
  if (!base::is.null(val)) return(TRUE)

  # name contains a variable name
  val <- eval(parse( text = paste0("stics","$",env_name,"$", name)))
  if (!base::is.null(val)) return(TRUE)

  return(FALSE)
}



stics_get <- function(name = NULL, env_name = "stics") {

  if (base::is.null(name) || name == "stics") return(stics_env(create = FALSE))


  if (length(name) > 1) {
    warning("name contains not only one variable name, returning the first one content ! ")
    name <- name[1]
  }

  envir <- stics_env(name = env_name, create = FALSE)

  if (!stics_exists(name = name, env_name = env_name)) {
    warning(name, " does not exist in environment ", environmentName(envir))
    return()
  }

  elts <- stics_split_list(name)

  loc_var <- get(elts[1], envir = envir, inherits = FALSE)

  if (length(elts) == 1) {
    return(loc_var)
  }

  # for a list elt extraction
  xpression <- paste0("loc_var$", paste(elts[2:length(elts)], collapse = "$"))

  eval(parse(text = xpression))
}



stics_set <- function(name, value, env_name = "stics") {
  envir <- stics_env(name = env_name)

  # Creating a copy in the target environment
  envir[["value"]] <- value

  elts <- stics_split_list(name)


  # Simple variable, existing or not
  if (length(elts) == 1) {
    xpression <- paste0(name, " <- value")
    eval(parse(text = xpression), envir = envir)
    stics_rm(name = "value", env_name = env_name)
    return(invisible(TRUE))
  }

  # A named list variable
  # testing if exists
  if (!stics_exists(elts[1], env_name = env_name)) {
    warning(elts[1], ": unknown list variable in stics environment !")
    return(invisible(FALSE))
  }

  xpression <- paste0(paste(elts, collapse = "$"), "<- value")

  eval(parse(text = xpression), envir = envir)

  # deleting temporary value variable
  stics_rm(name = "value", env_name = env_name)

  return(invisible(TRUE))
}


stics_class <- function(name, env_name = "stics") {
  return(class(stics_get(name = name, env_name = env_name)))
}


stics_split_list <- function(name) {
  return(unlist(strsplit(x = name, split = "\\$")))
}


stics_rm <- function(name = NULL, env_name = "stics") {
  envir <- stics_env(name = env_name)

  if (base::is.null(name)) {
    envir <- parent.env(envir)
    name <- "stics"
  }

  if (!stics_exists(name = name, env_name = env_name)) {
    warning(name, " does not exist !")
    return()
  }

  rm(list = name, envir = envir)
}

stics_empty <- function() {
  names <- stics_ls()

  if (base::is.null(names) || !length(names)) return()

  stics_rm( name = names )
}

