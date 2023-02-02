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
  "setWarn", signature(doc_obj = "file_document"),
  function(doc_obj, value) {
    doc_obj@warn <- value
    return(doc_obj)
  }
)


setReplaceMethod(
  "setName", signature(doc_obj = "file_document"),
  function(doc_obj, value) {
    doc_obj@name <- value
    return(doc_obj)
  }
)

setReplaceMethod(
  "setDir", signature(doc_obj = "file_document"),
  function(doc_obj, value) {
    doc_obj@dir <- normalizePath(value)
    return(doc_obj)
  }
)

setReplaceMethod(
  "setExt", signature(doc_obj = "file_document"),
  function(doc_obj, value) {
    doc_obj@ext <- value
    # add reconstruct file name !!!!!!!!!
    return(doc_obj)
  }
)

# set
setMethod(
  "setName", signature(doc_obj = "file_document"),
  function(doc_obj, value) {
    doc_obj@name <- value
    return(doc_obj)
  }
)


# getter methods
setMethod("getName", signature(doc_obj = "file_document"), function(doc_obj) {
  return(doc_obj@name)
})

#
setMethod("getDir", signature(doc_obj = "file_document"), function(doc_obj) {
  return(doc_obj@dir)
})

#
setMethod("getExt", signature(doc_obj = "file_document"), function(doc_obj) {
  # print(doc_obj)
  return(doc_obj@ext)
})

setMethod("getType", signature(doc_obj = "file_document"), function(doc_obj) {
  # print(doc_obj)
  return(doc_obj@type)
})

setMethod("getPath", signature(doc_obj = "file_document"), function(doc_obj) {
  return(file.path(doc_obj@dir, doc_obj@name))
})

setMethod("exist", signature(doc_obj = "file_document"), function(doc_obj) {
  message <- FALSE
  # TODO: make distinction between dir and file !!!
  p <- getPath(doc_obj)
  ret <- file.exists(p)

  if (ret) {
    if (isdir(doc_obj)) {
      ret <- ret & getType(doc_obj) == "dir"
    } else {
      ret <- ret & getType(doc_obj) == "file"
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
setMethod("create", signature(doc_obj = "file_document"), function(doc_obj) {
  p <- getPath(doc_obj)
  if (!exist(doc_obj)) {
    if (doc_obj@type == "file") {
      file.create(p)
    }
    if (doc_obj@type == "dir") {
      dir.create(p)
    }
  } else {
    print(paste0("   File already exists : ", p))
  }
})

#
setMethod("move", signature(doc_obj = "file_document"), function(doc_obj, toFile) {
  # cas : rename, move
  if (exist(doc_obj)) {
    if (dir.exists(toFile)) {
      toFile <- file.path(toFile, doc_obj@name)
    }
    file.rename(getPath(doc_obj), toFile)

    # doc_obj@file <- toFile
    doc_obj <- methods::new(class(doc_obj)[[1]], toFile)
  } else {
    setName(doc_obj) <- toFile
  }
  return(doc_obj)
})

#
setMethod(
  "rename", signature(doc_obj = "file_document"),
  function(doc_obj, toFile) {
    move(doc_obj, toFile)
  }
)

#
setMethod("delete", signature(doc_obj = "file_document"), function(doc_obj) {
  if (exist(doc_obj)) {
    file.remove(getPath(doc_obj))
  } else {
    # print(paste0(' File already exists : ',doc_obj@file))
  }
})




# Methods with a static like behaviour TODO: see to extract these apart
# from this class ???

#
setMethod("infos", signature(doc_obj = "ANY"), function(doc_obj, type) {
  if (methods::is(doc_obj, "character")) {
    p <- doc_obj
  } else {
    p <- getPath(doc_obj)
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
setMethod("isdir", signature(doc_obj = "ANY"), function(doc_obj) {
  ret <- infos(doc_obj, "isdir")
  if (is.na(ret)) {
    stop("Unavailable information: file or dir doesn't exist !")
  }
  return(ret)
})

#
setMethod("isempty", signature(doc_obj = "ANY"), function(doc_obj) {
  # for files and dirs
  ret <- infos(doc_obj, "size") == 0
  if (is.na(ret)) {
    stop("Unavailable information: file doesn't exist !")
  }
  if (isdir(doc_obj)) {
    if (methods::is(doc_obj, "character")) {
      l <- list.files(doc_obj)
    } else {
      l <- list.files(getPath(doc_obj))
    }
    ret <- length(l) == 0
  }
  return(ret)
})

# setGeneric('getClass', function(doc_obj) standardGeneric('getClass'))
# setMethod('getClass',signature(doc_obj='ANY'),
# function(doc_obj){ return(class(doc_obj)[[1]]) } )

#
setMethod("calcExt", signature(doc_obj = "ANY"), function(doc_obj) {
  ext <- ""
  if (methods::is(doc_obj, "character")) {
    name <- doc_obj
  } else {
    name <- doc_obj@name
  }
  char_vec <- unlist(strsplit(name, "[.]"))
  n_char <- length(char_vec)
  if (n_char > 1) {
    ext <- char_vec[[n_char]]
  }

  return(ext)
})

setMethod("calcType", signature(doc_obj = "ANY"), function(doc_obj) {
  # default type
  type <- "file"
  if (methods::is(doc_obj, "character")) {
    name <- doc_obj
  } else {
    name <- getPath(doc_obj)
  }
  # keep this order for identifying file from dir
  if (file.exists(name)) type <- "file"
  if (dir.exists(name)) type <- "dir"
  return(type)
})
