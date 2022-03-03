#' @keywords internal

# xml class based on fileDocument class
setClass("xmlDocument",
  contains = c("fileDocument"),
  validity = validDoc
)


# object validation
setMethod("validDoc", signature(object = "xmlDocument"), function(object) {
  # methods::callNextMethod()
  # print("calling xmlDocument validDoc")
  ext <- getExt(object)
  # print(ext)
  # print(object@content)
  if (length(object@name) == 0 || object@name == "") {
    return("file name is empty !")
  }
  if (!length(ext) || !ext == "xml") {
    return("Error: no xml extension")
  }
  if (!exist(object)) {
    return(paste("Error: file doesn't exist:", object@name, "!"))
  }
  if (exist(object) && isempty(object)) {
    return(paste("Error: file is empty:", object@name, "!"))
  }
  TRUE
})


# constructor
setMethod(
  "xmldocument", signature(file = "character"),
  function(file = character(length = 0)) {
    # print(file)
    methods::new("xmlDocument", file)
  }
)

setMethod(
  "initialize", "xmlDocument",
  function(.Object, file = character(length = 0), warn = FALSE) {
    .Object <- methods::callNextMethod(.Object, file)
    # print('xmlDocument initialization')
    # print(file)
    methods::validObject(.Object)
    .Object <- loadContent(.Object)
    # .Object@warn <- warn
    return(.Object)
  }
)


setMethod("show", "xmlDocument", function(object) {
  methods::callNextMethod()
  # print("show de xmlDocument")
  if (isLoaded(object)) {
    print(paste0("   content : ", class(object@content)[[1]]))
  }
})

# setter methods
setReplaceMethod("setContent", signature(docObj = "xmlDocument"), function(docObj, value) {
  if (!methods::is(value, "XMLInternalDocument")) {
    stop("Input value is not a XMLInternalDocument class object")
  }
  docObj@content <- value
  return(docObj)
})

# getter methods
setMethod("getContent", signature(docObj = "xmlDocument"), function(docObj) {
  return(docObj@content)
})


setMethod("getNodeS", signature(docObj = "xmlDocument"), function(docObj, path = NULL) {
  node_set <- NULL
  if (!isLoaded(docObj)) {
    stop("xml file is not loaded in xmlDocument object")
  }

  if (base::is.null(path)) {
    # getting root node name to get corresponding node set
    node_set <- getNodeSet(docObj@content, paste0("/", xmlName(xmlRoot(docObj@content))))
  } else {
    node_set <- getNodeSet(docObj@content, path[1])
    # browser("test getNodeS")
    # For a path vector, a loop is necessary
    # to keep results according to the path order !
    path_nb <- length(path)
    if (path_nb > 1) {
      for (i in 2:path_nb) {
        node_set <- merge_nodesets(node_set, getNodeSet(docObj@content, path[i]))
      }
    }
  }

  if (!length(node_set)) {
    if (docObj@warn) warning("Node set is empty, check xml input path !")
    node_set <- NULL
  }
  return(node_set)
})

setMethod("getAttrs", signature(docObj = "xmlDocument"), function(docObj, path) {
  attr_list <- NULL
  node_set <- getNodeS(docObj, path)
  #

  if (base::is.null(node_set)) {
    return(attr_list)
  }

  # tranforming attributes to matrix (if only one attribute, a character
  # vector is returned)
  attr_list <- sapply(node_set, function(x) xmlAttrs(x))
  # not any attributes in nodeset
  if (base::is.null(unlist(attr_list))) {
    return()
  }

  if (is.character(attr_list) & !is.matrix(attr_list)) {
    new_list <- vector(mode = "list", length(attr_list))
    for (i in seq_along(attr_list)) {
      new_list[[i]] <- attr_list[i]
    }
    attr_list <- new_list
  }
  # attr_list=as.matrix(attr_list)
  if (is.list(attr_list)) {
    attr_list <- attributes_list2matrix(attr_list)
  } else {
    attr_list <- t(attr_list)
  }
  # # test to detect if there's only one attribute
  # # TODO: add a contition to test if rownames (dimnames[[1]]) are identical
  # # and dim()[[2]]==1
  # r_names = unique(rownames(attr_list))
  # if (base::is.null(r_names)){
  #
  # }
  # if (dim(attr_list)[[2]] ==1 & length(r_names) == 1) {
  #   colnames(attr_list) <- unique(rownames(attr_list))
  #   rownames(attr_list) <- c()
  # } else {
  #   # transposing the matrix to have attribute names as colnames !
  #   attr_list=t(attr_list)
  # }

  # testing if any node has not any attribute
  any_null <- any(sapply(attr_list, function(x) base::is.null(x)))
  # print(attr_list)
  if (any_null && docObj@warn) {
    warning(paste("Existing nodes without any attributes on xpath", path))
  }

  # browser("test getAttrs")

  # testing if all nodes have the same attributes !!
  if (!is.matrix(attr_list) && !is.matrix(attr_list[, ])) {
    if (docObj@warn) {
      print(class(attr_list))
      warning(paste(
        "Existing nodes with different attributes comparing to others on xpath, missing attributes ?",
        path
      ))
    }
  }

  return(attr_list)
})

# getAttrsNames
setMethod("getAttrsNames", signature(docObj = "xmlDocument"), function(docObj, path) {
  attr_names <- NULL
  attr_list <- getAttrs(docObj, path)


  # print(path)
  #  print(attr_list)
  #   if (base::is.null(attr_list)){
  #     return(attr_list)}

  # TODO: Normally USELESS, see getAttrs (as.matrix ...)
  # if (!is.matrix(attr_list)){
  #  print("attrs characters ")
  #  # attr_names=unique(names(attr_list))
  #  attr_names=lapply(attr_list,function(x) names(x))
  # } # else {
  # print("attrs matrix ")
  dim_names <- dimnames(attr_list)
  if (!base::is.null(dim_names[[1]])) {
    attr_names <- dim_names[[1]]
    # print(attr_names)
  } else {
    attr_names <- dim_names[[2]]
  }
  # attr_names=unlist(attr_names)
  # print(attr_names)
  # }
  return(attr_names)
})


# getAttrsValues
setMethod("getAttrsValues", signature(docObj = "xmlDocument"), function(docObj, path,
                                                                        attr_list = character(length = 0),
                                                                        nodes_ids = NULL) {
  # browser()
  sel_values <- NULL



  # getting attributes values from doc
  attr_values <- getAttrs(docObj, path)

  # no attributes for the query
  if (base::is.null(attr_values)) {
    return(sel_values)
  }

  # selecting outputs
  # empty attr_list
  if (length(attr_list) == 0) {
    return(attr_values)
  }


  # finding existing attr names in path
  sel <- is.element(colnames(attr_values), attr_list)

  if (!any(sel)) {
    # not any given attr_list names exist in path
    if (docObj@warn) {
      warning(paste("Not any given attribute name exist in ", path, "aborting !"))
      return()
    }
  }

  # browser()

  # getting existing names from attr_list in attr_values
  # and getting the original order in initial attr_list
  found_list <- intersect(colnames(attr_values)[sel], attr_list)
  sel_list <- attr_list[is.element(attr_list, found_list)]

  # selecting wanted attributes columns
  # by the col names
  sel_values <- attr_values[, sel_list]

  # If only one column selected, restoring matrix format
  if (length(sel_list) == 1) {
    sel_values <- as.matrix(sel_values)
    colnames(sel_values) <- sel_list
  }

  # browser()

  # keeping only lines specified by nodes_ids
  if (!base::is.null(nodes_ids)) {
    if (max(nodes_ids) <= dim(sel_values)[1]) {
      sel_values <- sel_values[nodes_ids, ]
    } else {
      stop("Subscript out of range, check ids !")
    }
  }

  return(sel_values)
})


# factoriser avec getAttrs!! + getNode(docObj,path,kind)
setMethod("getValues", signature(docObj = "xmlDocument"), function(docObj, path, nodes_ids = NULL) {
  node_set <- getNodeS(docObj, path)

  # getting nodes number
  nodes_nb <- length(node_set)

  if (nodes_nb == 0) {
    return(invisible())
  }

  # browser()

  if (!base::is.null(nodes_ids)) {
    if (max(nodes_ids) <= nodes_nb) {
      node_set <- node_set[nodes_ids]
    } else {
      stop("Subscript out of range, check ids !")
      # return(invisible())
    }
  }

  # Getting values from the node_set
  val_list <- unlist(lapply(node_set, function(x) xmlValue(x)))

  return(val_list)
})


# addAttrs
setMethod("addAttrs", signature(docObj = "xmlDocument"), function(docObj, path, named_vector) {
  # add not base::is.null node_set !!!!
  if (!base::is.null(names(named_vector))) {
    node_set <- getNodeS(docObj, path)
    invisible(sapply(node_set, function(x) xmlAttrs(x) <- named_vector))
  }
})


# delete attributes
# delAttrs
# TODO: to remove all attrs !!!!!!!!!
setMethod("removeAttrs", signature(docObj = "xmlDocument"), function(docObj, path, attr_names) {
  # add not base::is.null node_set !!!!
  if (!base::is.null(attr_names)) {
    node_set <- getNodeS(docObj, path)
    attr_nb <- length(attr_names)
    for (i in 1:attr_nb) {
      sapply(node_set, function(x) removeAttributes(x, attr_names[i]))
    }
  }
})


# Setters
#
# TODO : same code as setValues,
setMethod("setAttrValues", signature(docObj = "xmlDocument"), function(docObj, path, attr_name,
                                                                       values_list, nodes_ids = NULL) {
  node_set <- getNodeS(docObj, path)
  if (base::is.null(node_set)) {
    return(invisible())
  }

  if (!base::is.null(nodes_ids)) {
    node_set <- node_set[nodes_ids]
  }

  nodes_nb <- length(node_set)

  if (length(values_list) == 1) {
    values_list <- rep(values_list, nodes_nb)
  }
  values_nb <- length(values_list)
  if (values_nb != nodes_nb) {
    stop("Values number is not consistent with nodes number !")
  }
  for (i in 1:nodes_nb) {
    value <- values_list[[i]]
    xmlAttrs(node_set[[i]])[[attr_name]] <- value
  }
})


# setValues
setMethod(
  "setValues", signature(docObj = "xmlDocument"),
  function(docObj, path, values_list, nodes_ids = NULL) {
    node_set <- getNodeS(docObj, path)
    # browser("setValues")
    if (base::is.null(node_set)) {
      return(invisible())
    }

    if (!base::is.null(nodes_ids)) {
      node_set <- node_set[nodes_ids]
    }

    nodes_nb <- length(node_set)


    if (length(values_list) == 1) {
      values_list <- rep(values_list, nodes_nb)
    }
    values_nb <- length(values_list)

    if (values_nb != nodes_nb) {
      stop("Values number is not consistent with nodes number to be modified !")
    }

    for (i in 1:nodes_nb) {
      xmlValue(node_set[[i]]) <- values_list[[i]]
    }
  }
)

#

# insert after ?????

# addNodes
setMethod("addNodes", signature(docObj = "xmlDocument"), function(docObj, nodes_to_add, parent_path = NULL) {
  # parent node is root node
  if (base::is.null(parent_path)) {
    pnode <- xmlRoot(docObj@content)
    # getting parent node from given parent_path
  } else {
    node_set <- getNodeS(docObj, parent_path)
    # print(node_set[[1]])
    if (base::is.null(node_set)) {
      return()
    }
    pnode <- node_set[[1]]
  }
  # for a node set
  if (class(nodes_to_add)[[1]] == "XMLNodeSet") {
    for (n in seq_along(nodes_to_add)) {
      addChildren(pnode, nodes_to_add[[n]])
    }
    # for a single node
  } else {
    addChildren(pnode, nodes_to_add)
  }
})


# removeNodes
setMethod("delNodes", signature(docObj = "xmlDocument"), function(docObj, path) {
  node_set <- getNodeS(docObj, path)

  if (base::is.null(node_set)) {
    return()
  }

  removeNodes(node_set)
})



# replaceNodes ??

# delNode
# removeChildren
# removeNodes
# nodes=getNodeS(xnode,'//param[@nom="lvmax"]')
# removeChildren(xmlRoot(xnode@content),nodes[[1]])

# other methods
setMethod("loadContent", signature(docObj = "xmlDocument"), function(docObj) {
  # print("dans loadContent...")
  setContent(docObj) <- xmlParse(getPath(docObj))
  return(docObj)
})


setMethod("isLoaded", signature(docObj = "xmlDocument"), function(docObj) {
  return(methods::is(docObj@content, "XMLInternalDocument"))
})

setMethod("is.xmlDocument", signature(docObj = "ANY"), function(docObj) {
  if (methods::is(docObj, "xmlDocument")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})


# save method
setMethod("saveXmlDoc", signature(docObj = "xmlDocument"), function(docObj, xml_path) {
  if (isLoaded(docObj)) {
    write(saveXML(docObj@content), xml_path)
  }
})

# clone method
setMethod("cloneXmlDoc", signature(docObj = "xmlDocument"), function(docObj) {
  if (!isLoaded(docObj)) {
    return(NULL)
  }

  setContent(docObj) <- xmlClone(getContent(docObj))

  return(docObj)
})
