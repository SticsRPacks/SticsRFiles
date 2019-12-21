#' @import XML
# for generic functions declarations for *Document classes
setGeneric("validDoc", function(object) standardGeneric("validDoc"))
# defined for fileDocument
types_list = c("file", "dir", "url", "link")

setGeneric("filedocument", function(file, type) standardGeneric("filedocument"))
setGeneric("setName<-", function(docObj, value) standardGeneric("setName<-"))
setGeneric("setName", function(docObj, value) standardGeneric("setName"))
setGeneric("setWarn<-", function(docObj, value) standardGeneric("setWarn<-"))
setGeneric("setDir<-", function(docObj, value) standardGeneric("setDir<-"))
setGeneric("setExt<-", function(docObj, value) standardGeneric("setExt<-"))
setGeneric("getName", function(docObj) standardGeneric("getName"))

setGeneric("getDir", function(docObj) standardGeneric("getDir"))
setGeneric("getExt", function(docObj) standardGeneric("getExt"))
setGeneric("getType", function(docObj) standardGeneric("getType"))

setGeneric("getPath", function(docObj) standardGeneric("getPath"))

setGeneric("exist", function(docObj) standardGeneric("exist"))

setGeneric("create", function(docObj) standardGeneric("create"))

setGeneric("move", function(docObj, toFile) standardGeneric("move"))

setGeneric("rename", function(docObj, toFile) standardGeneric("rename"))

setGeneric("delete", function(docObj) standardGeneric("delete"))
setGeneric("infos", function(docObj, type = "all") standardGeneric("infos"))

setGeneric("isdir", function(docObj) standardGeneric("isdir"))
setGeneric("isempty", function(docObj) standardGeneric("isempty"))
setGeneric("calcExt", function(docObj) standardGeneric("calcExt"))
setGeneric("calcType", function(docObj) standardGeneric("calcType"))

# defined for xmlDocument

setGeneric("xmldocument", function(file) standardGeneric("xmldocument"))
setGeneric("setContent<-", function(docObj, value) standardGeneric("setContent<-"))

setGeneric("getContent", function(docObj) standardGeneric("getContent"))

setGeneric("loadContent", function(docObj) standardGeneric("loadContent"))
setGeneric("isLoaded", function(docObj) standardGeneric("isLoaded"))

setGeneric("is.xmlDocument", function(docObj) standardGeneric("is.xmlDocument"))

# xml manipulations

setGeneric("getNodeS", function(docObj,path=NULL) standardGeneric("getNodeS"))

setGeneric("getAttrs", function(docObj,path) standardGeneric("getAttrs"))

setGeneric("getAttrsNames", function(docObj,path) standardGeneric("getAttrsNames"))

setGeneric("getAttrsValues", function(docObj,path,attr_list,nodes_ids=NULL) standardGeneric("getAttrsValues"))

setGeneric("addAttrs", function(docObj,path,named_vector) standardGeneric("addAttrs"))

setGeneric("removeAttrs", function(docObj,path,attr_names) standardGeneric("removeAttrs"))

setGeneric("setAttrValues", function(docObj,path,attr_name,values_list,nodes_ids = NULL) standardGeneric("setAttrValues"))

setGeneric("setValues", function(docObj,path,values_list, nodes_ids=NULL) standardGeneric("setValues"))

setGeneric("getValues", function(docObj,path,nodes_ids=NULL) standardGeneric("getValues"))

setGeneric("saveXmlDoc", function(docObj,xml_path) standardGeneric("saveXmlDoc"))

setGeneric("cloneXmlDoc", function(docObj) standardGeneric("cloneXmlDoc"))

# adding and removing nodes to doc

setGeneric("addNodes", function(docObj,nodes_to_add,parent_path=NULL) standardGeneric("addNodes"))
# removing nodes

setGeneric("delNodes", function(docObj,path) standardGeneric("delNodes"))






