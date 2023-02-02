#' @import XML
# for generic functions declarations for *Document classes
setGeneric("validDoc", function(object) standardGeneric("validDoc"))
# defined for fileDocument
types_list <- c("file", "dir", "url", "link")

setGeneric("filedocument", function(file, type) standardGeneric("filedocument"))
setGeneric("setName<-", function(doc_obj, value) standardGeneric("setName<-"))
setGeneric("setName", function(doc_obj, value) standardGeneric("setName"))
setGeneric("setWarn<-", function(doc_obj, value) standardGeneric("setWarn<-"))
setGeneric("setDir<-", function(doc_obj, value) standardGeneric("setDir<-"))
setGeneric("setExt<-", function(doc_obj, value) standardGeneric("setExt<-"))
setGeneric("getName", function(doc_obj) standardGeneric("getName"))

setGeneric("getDir", function(doc_obj) standardGeneric("getDir"))
setGeneric("getExt", function(doc_obj) standardGeneric("getExt"))
setGeneric("getType", function(doc_obj) standardGeneric("getType"))

setGeneric("getPath", function(doc_obj) standardGeneric("getPath"))

setGeneric("exist", function(doc_obj) standardGeneric("exist"))

setGeneric("create", function(doc_obj) standardGeneric("create"))

setGeneric("move", function(doc_obj, toFile) standardGeneric("move"))

setGeneric("rename", function(doc_obj, toFile) standardGeneric("rename"))

setGeneric("delete", function(doc_obj) standardGeneric("delete"))
setGeneric("infos", function(doc_obj, type = "all") standardGeneric("infos"))

setGeneric("isdir", function(doc_obj) standardGeneric("isdir"))
setGeneric("isempty", function(doc_obj) standardGeneric("isempty"))
setGeneric("calcExt", function(doc_obj) standardGeneric("calcExt"))
setGeneric("calcType", function(doc_obj) standardGeneric("calcType"))

# defined for xml_document

setGeneric("xmldocument", function(file) standardGeneric("xmldocument"))
setGeneric("setContent<-", function(doc_obj, value) standardGeneric("setContent<-"))

setGeneric("getContent", function(doc_obj) standardGeneric("getContent"))

setGeneric("loadContent", function(doc_obj) standardGeneric("loadContent"))
setGeneric("isLoaded", function(doc_obj) standardGeneric("isLoaded"))

setGeneric("is.xml_document", function(doc_obj) standardGeneric("is.xml_document"))

# xml manipulations

setGeneric("getNodeS", function(doc_obj, path = NULL) standardGeneric("getNodeS"))

setGeneric("getAttrs", function(doc_obj, path) standardGeneric("getAttrs"))

setGeneric("getAttrsNames", function(doc_obj, path) standardGeneric("getAttrsNames"))

setGeneric("getAttrsValues", function(doc_obj, path, attr_list, nodes_ids = NULL) standardGeneric("getAttrsValues"))

setGeneric("addAttrs", function(doc_obj, path, named_vector) standardGeneric("addAttrs"))

setGeneric("removeAttrs", function(doc_obj, path, attr_names) standardGeneric("removeAttrs"))

setGeneric("setAttrValues", function(doc_obj, path, attr_name, values_list, nodes_ids = NULL) standardGeneric("setAttrValues"))

setGeneric("setValues", function(doc_obj, path, values_list, nodes_ids = NULL) standardGeneric("setValues"))

setGeneric("getValues", function(doc_obj, path, nodes_ids = NULL) standardGeneric("getValues"))

setGeneric("saveXmlDoc", function(doc_obj, xml_path) standardGeneric("saveXmlDoc"))

setGeneric("cloneXmlDoc", function(doc_obj) standardGeneric("cloneXmlDoc"))

# adding and removing nodes to doc

setGeneric("addNodes", function(doc_obj, nodes_to_add, parent_path = NULL) standardGeneric("addNodes"))
# removing nodes

setGeneric("delNodes", function(doc_obj, path) standardGeneric("delNodes"))
