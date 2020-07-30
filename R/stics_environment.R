sticsenv_name <- function() {
  return(".stics")
}

stics_env <- function(name = NULL, env_name = globalenv(), create = TRUE) {

  exists_env <- stics_exists()

  # Can't create env
  if (!exists_env) {
    if (!create) {
      # warning(sticsenv_name(),
      #      " environment does not exist, set create to TRUE to create it.")
      return(NULL)
    }

    # creating .stics env attached to .GlobalEnv
    local_stics <- sticsenv_create(name = sticsenv_name(), env_name = ".GlobalEnv")
  } else {
    # getting existing .stics env
    local_stics <- get(x = sticsenv_name(), envir = globalenv(), inherits = FALSE)
  }

  # Returning .stics env object
  if (base::is.null(name) || name == sticsenv_name()) {
    return(local_stics)
  }

  # Checking a sub-env
  exists_name <- exists(name, envir = local_stics, inherits = FALSE)

  # Returning .stics env object
  # see: local_stics[[name]]
  #if (exists_name) return(get(name, envir = local_stics, inherits = FALSE))
  if (exists_name) return(local_stics[[name]])


  if (!create) {
    # warning(name,
    #      " environment does not exist, set create to TRUE to create it.")
    return(NULL)
  }

  # Creating a new .stics sub-env
  local_env <- sticsenv_create(name = name, env_name = sticsenv_name())

  return(local_env)
}


#' @export
sticsenv <- stics_env


sticsenv_create <- function(name, env_name = ".GlobalEnv") {

  parent <- eval(parse(text = env_name))

  new_env <- new.env(parent = parent)

  assign(
    x = name,
    value = new_env,
    pos = parent
  )

  new_env <- sticsenv_set_name(name = name, env_name = env_name)

  return(new_env)
}


sticsenv_set_name <- function(name, env_name = sticsenv_name(), fix_name = NULL) {

  envir <- suppressWarnings(stics_get(name = name, env_name = env_name))

  if (base::is.null(envir)) return()

  if (base::is.null(fix_name)) {
    env_name <- paste0("R_", name)
  } else {
    env_name <- fix_name
  }

  attr(envir,"name") <- env_name

  return(envir)
}


sticsenv_get_name <- function(name = NULL, env_name = sticsenv_name()) {

  if (!stics_exists(name = name, env_name = env_name)) return(invisible())

  return(environmentName(env = stics_get(name)))

}


sticsenv_ls <- function(name = NULL, env_name = sticsenv_name(), detail = FALSE) {

  envir <- stics_get(name, env_name = env_name )

  if (base::is.null(envir)) return()

  if (detail) {
    return(utils::ls.str(envir))
  }

  return(ls(envir))
}

#' @export
sticsls <- sticsenv_ls


stics_exists <- function(name = NULL, env_name = sticsenv_name()) {


  exists_env <- exists(x = sticsenv_name(),
                       envir = globalenv(), inherits = FALSE)

  if (!exists_env) {
    return(FALSE)
  }

  if (base::is.null(name) || name == sticsenv_name()) {
    return(exists_env)
  }



  # name contains a env name
  val <- eval(parse(text = paste0(sticsenv_name(), "$", name)))
  if (!base::is.null(val)) return(TRUE)

  # name contains a variable name
  val <- eval(parse(text = paste0(sticsenv_name(), "$", env_name, "$", name)))
  if (!base::is.null(val)) return(TRUE)

  # For a variable containing a list
  name <- stics_split_list(name)[1]
  val <- eval(parse(text = paste0(sticsenv_name(), "$", env_name, "$", name)))
  if (!base::is.null(val)) return(TRUE)

  return(FALSE)
}

#' @export
sticsexists <- stics_exists




stics_get <- function(name = NULL, env_name = sticsenv_name()) {

  #browser()

  if (base::is.null(name) || name == sticsenv_name()) {
    return(stics_env(create = FALSE))
  }

  if (length(name) > 1) {
    # warning("name contains not only one variable name",
    #         "returning the first one content ! ")
    # name <- name[1]
    out <- lapply(name, function(x) stics_get(name = x, env_name = env_name ))

    names(out) <- name
    return(out)
  }


  envir <- stics_env(name = env_name, create = FALSE)

  elts <- stics_split_list(name)

  if (!stics_exists(name = elts[1], env_name = env_name)) {
    warning(elts[1],
            " does not exist in environment ",
            environmentName(envir))
    return(NA)
  }



  loc_var <- get(elts[1], envir = envir, inherits = FALSE)

  if (length(elts) == 1) {
    return(loc_var)
  }

  # for a list elt extraction
  xpression <- paste0("loc_var$", paste(elts[2:length(elts)], collapse = "$"))

  eval(parse(text = xpression))

  return(loc_var)
}

#' @export
sticsget <- stics_get



stics_set <- function(name, value, env_name = sticsenv_name()) {

  envir <- stics_env(name = env_name)

  # Creating a copy in the target environment
  envir[["value"]] <- value

  elts <- stics_split_list(name)


  # Simple variable, existing or not
  if (length(elts) == 1) {
    eval(parse(text = paste0(name, " <- value")), envir = envir)
    sticsenv_remove(name = "value", env_name = env_name)
    return(invisible(TRUE))
  }

  # A named list variable
  # testing if exists
  if (!stics_exists(elts[1], env_name = env_name)) {
    warning(elts[1],
            paste0(": unknown list variable in ",
                   sticsenv_name(),
                   " environment !")
    )

    return(invisible(FALSE))
  }

  xpression <- paste0(paste(elts, collapse = "$"), "<- value")

  eval(parse(text = xpression), envir = envir)

  # deleting temporary value variable
  sticsenv_remove(name = "value", env_name = env_name)

  return(invisible(TRUE))
}

#' @export
sticsset <- stics_set


stics_class <- function(name, env_name = sticsenv_name()) {
  return(class(stics_get(name = name, env_name = env_name)))
}


stics_split_list <- function(name) {
  return(unlist(strsplit(x = name, split = "\\$")))
}


sticsenv_remove <- function(name = NULL, env_name = sticsenv_name()) {
 # browser()
  envir <- stics_get(name = env_name)

  if (base::is.null(name)) {
    envir <- parent.env(envir)
    name <- sticsenv_name()
  }

  if (!stics_exists(name = name, env_name = env_name)) {
    warning(name, " does not exist !")
    return(FALSE)
  }

  # TODO: add if name is a part of a list variable
  # but not an env
  # a$b
  # splitter pour voir si name est une liste
  # si oui alors par rm mais evaluer a$b <- NULL
  #if ()


  # getting variable names
  splitted_names <- strsplit(name, split = "$", fixed = T)
  list_idx <- unlist(lapply(splitted_names, function(x) length(x) >1))

  list_var_names <- NULL
  var_names <- NULL

  if (any(list_idx)) list_var_names <- name[list_idx]

  if (any(!list_idx))  var_names <- name[!list_idx]


  ret <- FALSE
  if (length(list_var_names)) {
    # suppression elts de liste
    ret <- unlist(lapply(list_var_names,
                         function(x) eval(parse(text = paste0("base::is.null(",x," <- NULL)")), envir = envir)))
    ret <- all(ret)
  }

  if (!length(var_names)) return(ret)

  # removing other variables or env
  #if (any(!list_idx)) {
  if (base::is.null(rm(list = var_names, envir = envir))) return(ret & TRUE)
  #}


}

#' @export
sticsremove <- sticsenv_remove

sticsenv_clean <- function() {
  names <- sticsenv_ls()

  if (base::is.null(names) || !length(names)) return()

  sticsenv_remove(name = names)
}

#' @export
sticsclean <- sticsenv_clean()
