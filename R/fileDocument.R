#' @include global.R
#' @keywords internal

#' An S4 class to represent a file or dir.
#'
#' @slot type A file type ('file','dir')
#' @slot name A file name (with an extension or not)
#' @slot dir A file dir or path
#' @slot ext A file extension
#' @slot con A file connexion
#' @slot content A file content
#' @slot warn Logical, to display (TRUE) or not (FALSE) warnings (default)
#'
setClass(
  "file_document",
  representation(
    type = "character", name = "character", dir = "character",
    ext = "character", con = "ANY", content = "ANY",
    warn = "logical"
  ),
  prototype(
    type = character(length = 0), name = character(length = 0),
    dir = character(length = 0), ext = character(length = 0),
    con = NULL, content = "", warn = FALSE
  )
)

# constructor
setMethod(
  "filedocument", signature(file = "character", type = "character"),
  function(file = character(length = 0), type = character(length = 0)) {
    # print(file)
    return(methods::new("file_document", file, type))
  }
)


# file only
setMethod(
  "filedocument", signature(file = "character", type = "missing"),
  function(file = character(length = 0), type = character(length = 0)) {
    # print(file)
    return(methods::new("file_document", file))
  }
)


setMethod("initialize", "file_document", function(.Object,
                                                 file = character(length = 0),
                                                 type = character(length = 0)) {
  if (missing(file)) {
    stop("missing file name")
  }


  .Object@name <- basename(file)
  .Object@dir <- normalizePath(dirname(file))
  .Object@ext <- calcExt(.Object@name)

  .Object@type <- calcType(.Object)

  .Object@warn <- FALSE

  methods::validObject(.Object)
  return(.Object)
})



# setter method
# replace

# set
setReplaceMethod(
  "setWarn", signature(object = "file_document"),
  function(object, value) {
    object@warn <- value
    return(object)
  }
)


setReplaceMethod(
  "setName", signature(object = "file_document"),
  function(object, value) {
    object@name <- value
    return(object)
  }
)

setReplaceMethod(
  "setDir", signature(object = "file_document"),
  function(object, value) {
    object@dir <- normalizePath(value)
    return(object)
  }
)

setReplaceMethod(
  "setExt", signature(object = "file_document"),
  function(object, value) {
    object@ext <- value
    # add reconstruct file name !!!!!!!!!
    return(object)
  }
)

# set
setMethod(
  "setName", signature(object = "file_document"),
  function(object, value) {
    object@name <- value
    return(object)
  }
)


# getter methods
setMethod("getName", signature(object = "file_document"), function(object) {
  return(object@name)
})

#
setMethod("getDir", signature(object = "file_document"), function(object) {
  return(object@dir)
})

#
setMethod("getExt", signature(object = "file_document"), function(object) {
  # print(object)
  return(object@ext)
})

setMethod("getType", signature(object = "file_document"), function(object) {
  # print(object)
  return(object@type)
})

setMethod("getPath", signature(object = "file_document"), function(object) {
  return(file.path(object@dir, object@name))
})

setMethod("exist", signature(object = "file_document"), function(object) {
  message <- FALSE
  # TODO: make distinction between dir and file !!!
  p <- getPath(object)
  ret <- file.exists(p)

  if (ret) {
    if (isdir(object)) {
      ret <- ret & getType(object) == "dir"
    } else {
      ret <- ret & getType(object) == "file"
    }
  }
  if (!ret & message) {
    print(paste0("   File doesn't exist: ", p))
  }
  return(ret)
})

setMethod("show", "file_document", function(object) {
  # print("show de file_document")
  print(paste0("   name : ", object@name))
  print(paste0("   type : ", object@type))
  print(paste0("   dir : ", object@dir))
  print(paste0("   ext : ", object@ext))
})

#
setMethod("create", signature(object = "file_document"), function(object) {
  p <- getPath(object)
  if (!exist(object)) {
    if (object@type == "file") {
      file.create(p)
    }
    if (object@type == "dir") {
      dir.create(p)
    }
  } else {
    print(paste0("   File already exists : ", p))
  }
})

#
setMethod("move", signature(object = "file_document"), function(object, toFile) {
  # cas : rename, move
  if (exist(object)) {
    if (dir.exists(toFile)) {
      toFile <- file.path(toFile, object@name)
    }
    file.rename(getPath(object), toFile)

    # object@file <- toFile
    object <- methods::new(class(object)[[1]], toFile)
  } else {
    setName(object) <- toFile
  }
  return(object)
})

#
setMethod(
  "rename", signature(object = "file_document"),
  function(object, toFile) {
    move(object, toFile)
  }
)

#
setMethod("delete", signature(object = "file_document"), function(object) {
  if (exist(object)) {
    file.remove(getPath(object))
  } else {
    # print(paste0(' File already exists : ',object@file))
  }
})




# Methods with a static like behaviour TODO: see to extract these apart
# from this class ???

#
setMethod("infos", signature(object = "ANY"), function(object, type) {
  if (methods::is(object, "character")) {
    p <- object
  } else {
    p <- getPath(object)
  }
  # type: all,size,mtime,isdir
  ret <- file.info("")
  if (type == "all") {
    ret <- file.info(p)
  } else {
    ret <- file.info(p)[[type]]
  }
  return(ret)
})

#
setMethod("isdir", signature(object = "ANY"), function(object) {
  ret <- infos(object, "isdir")
  if (is.na(ret)) {
    stop("Unavailable information: file or dir doesn't exist !")
  }
  return(ret)
})

#
setMethod("isempty", signature(object = "ANY"), function(object) {
  # for files and dirs
  ret <- infos(object, "size") == 0
  if (is.na(ret)) {
    stop("Unavailable information: file doesn't exist !")
  }
  if (isdir(object)) {
    if (methods::is(object, "character")) {
      l <- list.files(object)
    } else {
      l <- list.files(getPath(object))
    }
    ret <- length(l) == 0
  }
  return(ret)
})

# setGeneric('getClass', function(object) standardGeneric('getClass'))
# setMethod('getClass',signature(object='ANY'),
# function(object){ return(class(object)[[1]]) } )

#
setMethod("calcExt", signature(object = "ANY"), function(object) {
  ext <- ""
  if (methods::is(object, "character")) {
    name <- object
  } else {
    name <- object@name
  }
  char_vec <- unlist(strsplit(name, "[.]"))
  n_char <- length(char_vec)
  if (n_char > 1) {
    ext <- char_vec[[n_char]]
  }

  return(ext)
})

setMethod("calcType", signature(object = "ANY"), function(object) {
  # default type
  type <- "file"
  if (methods::is(object, "character")) {
    name <- object
  } else {
    name <- getPath(object)
  }
  # keep this order for identifying file from dir
  if (file.exists(name)) type <- "file"
  if (dir.exists(name)) type <- "dir"
  return(type)
})
