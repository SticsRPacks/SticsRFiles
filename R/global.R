#' @import XML
# for generic functions declarations for *Document classes
setGeneric("validDoc", function(object) standardGeneric("validDoc"))
# defined for fileDocument
types_list = c("file", "dir", "url", "link")
#' @export
setGeneric("filedocument", function(file, type) standardGeneric("filedocument"))
setGeneric("setName<-", function(docObj, value) standardGeneric("setName<-"))
setGeneric("setName", function(docObj, value) standardGeneric("setName"))
setGeneric("setDir<-", function(docObj, value) standardGeneric("setDir<-"))
setGeneric("setExt<-", function(docObj, value) standardGeneric("setExt<-"))
setGeneric("getName", function(docObj) standardGeneric("getName"))
#' @export
setGeneric("getDir", function(docObj) standardGeneric("getDir"))
setGeneric("getExt", function(docObj) standardGeneric("getExt"))
setGeneric("getType", function(docObj) standardGeneric("getType"))
#' @export
setGeneric("getPath", function(docObj) standardGeneric("getPath"))
#' @export
setGeneric("exist", function(docObj) standardGeneric("exist"))
#' @export
setGeneric("create", function(docObj) standardGeneric("create"))
#' @export
setGeneric("move", function(docObj, toFile) standardGeneric("move"))
#' @export
setGeneric("rename", function(docObj, toFile) standardGeneric("rename"))
#' @export
setGeneric("delete", function(docObj) standardGeneric("delete"))
setGeneric("infos", function(docObj, type = "all") standardGeneric("infos"))
#' @export
setGeneric("isdir", function(docObj) standardGeneric("isdir"))
setGeneric("isempty", function(docObj) standardGeneric("isempty"))
setGeneric("calcExt", function(docObj) standardGeneric("calcExt"))
setGeneric("calcType", function(docObj) standardGeneric("calcType"))

# defined for xmlDocument
#' @export
setGeneric("xmldocument", function(file) standardGeneric("xmldocument"))
setGeneric("setContent<-", function(docObj, value) standardGeneric("setContent<-"))
#' @export
setGeneric("getContent", function(docObj) standardGeneric("getContent"))
#' @export
setGeneric("loadContent", function(docObj) standardGeneric("loadContent"))
setGeneric("isLoaded", function(docObj) standardGeneric("isLoaded"))
#' @export
setGeneric("is.xmlDocument", function(docObj) standardGeneric("is.xmlDocument"))

# xml manipulations
#' @export
setGeneric("getNodeS", function(docObj,path=NULL) standardGeneric("getNodeS"))
#' @export
setGeneric("getAttrs", function(docObj,path) standardGeneric("getAttrs"))
#' @export
setGeneric("getAttrsNames", function(docObj,path) standardGeneric("getAttrsNames"))
#' @export
setGeneric("getAttrsValues", function(docObj,path,attr_list,nodes_ids=NULL) standardGeneric("getAttrsValues"))
#' @export
setGeneric("addAttrs", function(docObj,path,named_vector) standardGeneric("addAttrs"))
#' @export
setGeneric("removeAttrs", function(docObj,path,attr_names) standardGeneric("removeAttrs"))
#' @export
setGeneric("setAttrValues", function(docObj,path,attr_name,values_list,nodes_ids = NULL) standardGeneric("setAttrValues"))
#' @export
setGeneric("setValues", function(docObj,path,values_list, nodes_ids=NULL) standardGeneric("setValues"))
#' @export
setGeneric("getValues", function(docObj,path,nodes_ids=NULL) standardGeneric("getValues"))
#' @export
setGeneric("saveXmlDoc", function(docObj,xml_path) standardGeneric("saveXmlDoc"))
#' @export
setGeneric("cloneXmlDoc", function(docObj) standardGeneric("cloneXmlDoc"))

# adding and removing nodes to doc
#' @export
setGeneric("addNodes", function(docObj,nodes_to_add,parent_path=NULL) standardGeneric("addNodes"))
# removing nodes
#' @export
setGeneric("delNodes", function(docObj,path) standardGeneric("delNodes"))






