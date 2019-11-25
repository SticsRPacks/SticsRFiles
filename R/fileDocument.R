#' @include global.R


#' An S4 class to represent a file or dir.
#'
#' @slot type A file type ('file','dir')
#' @slot name A file name (with an extension or not)
#' @slot dir A file dir or path
#' @slot ext A file extension
#' @slot con A file connexion
#' @slot content A file content
#'
setClass("fileDocument",
         representation(type = "character", name = "character", dir = "character", ext = "character", con = "ANY", content = "ANY"),
         prototype(type = character(length = 0), name = character(length = 0), dir = character(length = 0), ext = character(length = 0),
         con = NULL, content = ""))# ,
         #validity=validDoc)


# object validation function
# setMethod("validDoc", signature(object = "fileDocument"), function(object) {
#     # print("calling fileDocument validDoc")
#     # checking given type
#     if (!any(object@type == types_list)) {
#         return("Invalid type of file !")
#     }
#     TRUE
# })

# constructor
setMethod("filedocument", signature(file = "character", type = "character"), function(file = character(length = 0), type = character(length = 0)) {
    #print(file)
    return(methods::new("fileDocument", file, type))
})

# file only
setMethod("filedocument", signature(file = "character", type = "missing"), function(file = character(length = 0), type = character(length = 0)) {
    #print(file)
    return(methods::new("fileDocument", file))
})


setMethod("initialize", "fileDocument", function(.Object, file = character(length = 0), type = character(length = 0)) {
    # print("fileDocument initialization")
    if (missing(file)) {
        stop("missing file name")
    }
    # if (missing(type)) {
    #     type = "file"
    # }


    .Object@name <- basename(file)
    .Object@dir <- normalizePath(dirname(file))
    .Object@ext <- calcExt(.Object@name)

    .Object@type <- calcType(.Object)

    methods::validObject(.Object)
    return(.Object)
})



# setter method
# replace
setReplaceMethod("setName", signature(docObj = "fileDocument"), function(docObj, value) {
  docObj@name <- value
  return(docObj)
})

setReplaceMethod("setDir", signature(docObj = "fileDocument"), function(docObj, value) {
    docObj@dir <- normalizePath(value)
    return(docObj)
})

setReplaceMethod("setExt", signature(docObj = "fileDocument"), function(docObj, value) {
    docObj@ext <- value
    # add reconstruct file name !!!!!!!!!
    return(docObj)
})

# set
setMethod("setName", signature(docObj = "fileDocument"), function(docObj, value) {
  docObj@name <- value
  return(docObj)
})


# getter methods
setMethod("getName", signature(docObj = "fileDocument"), function(docObj) {
    return(docObj@name)
})

#
setMethod("getDir", signature(docObj = "fileDocument"), function(docObj) {
    return(docObj@dir)
})

#
setMethod("getExt", signature(docObj = "fileDocument"), function(docObj) {
    # print(docObj)
    return(docObj@ext)
})

setMethod("getType", signature(docObj = "fileDocument"), function(docObj) {
    # print(docObj)
    return(docObj@type)
})

setMethod("getPath", signature(docObj = "fileDocument"), function(docObj) {
    return(file.path(docObj@dir, docObj@name))
})

setMethod("exist", signature(docObj = "fileDocument"), function(docObj) {
  message=FALSE
    # TODO: make distinction between dir and file !!!
    p = getPath(docObj)
    ret = file.exists(p)

    if (ret) {
        if (isdir(docObj)) {
            ret = ret & getType(docObj) == "dir"
        } else {
            ret = ret & getType(docObj) == "file"
        }
    }
    if (!ret & message)
        print(paste0("   File doesn't exist: ", p))
    return(ret)
})

setMethod("show", "fileDocument", function(object) {
    # print("show de fileDocument")
    print(paste0("   name : ", object@name))
    print(paste0("   type : ", object@type))
    print(paste0("   dir : ", object@dir))
    print(paste0("   ext : ", object@ext))
})

#
setMethod("create", signature(docObj = "fileDocument"), function(docObj) {
    p = getPath(docObj)
    if (!exist(docObj)) {
        if (docObj@type == "file") {
            file.create(p)
        }
        if (docObj@type == "dir") {
            dir.create(p)
        }
    } else {
        print(paste0("   File already exists : ", p))
    }
})

#
setMethod("move", signature(docObj = "fileDocument"), function(docObj, toFile) {
    # cas : rename, move
    if (exist(docObj)) {
        if (dir.exists(toFile)) {
            toFile = file.path(toFile, docObj@name)
        }
        file.rename(getPath(docObj), toFile)

        # docObj@file <- toFile
        docObj <- methods::new(class(docObj)[[1]], toFile)
    } else {
      setName(docObj)<-toFile
    }
    return(docObj)
})

#
setMethod("rename", signature(docObj = "fileDocument"), function(docObj, toFile) {
  move(docObj,toFile)
})

#
setMethod("delete", signature(docObj = "fileDocument"), function(docObj) {
    if (exist(docObj)) {
        file.remove(getPath(docObj))
    } else {
        # print(paste0(' File already exists : ',docObj@file))
    }

})




# Methods with a static like behaviour TODO: see to extract these apart from this class ???

#
setMethod("infos", signature(docObj = "ANY"), function(docObj, type) {
    if (methods::is(docObj, "character")) {
        p = docObj
    } else {
        p = getPath(docObj)
    }
    # type: all,size,mtime,isdir
    ret = file.info("")
    if (type == "all") {
        ret = file.info(p)
    } else {
        ret = file.info(p)[[type]]
    }
    return(ret)
})

#
setMethod("isdir", signature(docObj = "ANY"), function(docObj) {
    ret = infos(docObj, "isdir")
    if (is.na(ret)) {
        stop("Unavailable information: file or dir doesn't exist !")
    }
    return(ret)
})

#
setMethod("isempty", signature(docObj = "ANY"), function(docObj) {
    # for files and dirs
    ret = infos(docObj, "size") == 0
    if (is.na(ret)) {
        stop("Unavailable information: file doesn't exist !")
    }
    if (isdir(docObj)) {
        if (methods::is(docObj, "character")) {
            l = list.files(docObj)
        } else {
            l = list.files(getPath(docObj))
        }
        ret = length(l) == 0
    }
    return(ret)
})

# setGeneric('getClass', function(docObj) standardGeneric('getClass')) setMethod('getClass',signature(docObj='ANY'),
# function(docObj){ return(class(docObj)[[1]]) } )

#
setMethod("calcExt", signature(docObj = "ANY"), function(docObj) {
    ext = ""
    if (methods::is(docObj, "character")) {
        name = docObj
    } else {
        name = docObj@name
    }
    char_vec = unlist(strsplit(name, "[.]"))
    n_char = length(char_vec)
    if (n_char > 1) {
        ext = char_vec[[n_char]]
    }

    return(ext)
})

setMethod("calcType", signature(docObj = "ANY"), function(docObj) {
  # default type
  type="file"
  if (methods::is(docObj, "character")) {
    name = docObj
  } else {
    name = getPath(docObj)
  }
  # keep this order for identifying file from dir
  if (file.exists(name)) type="file"
  if (dir.exists(name)) type="dir"
  return(type)
})



