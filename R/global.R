#' @import XML
# for generic functions declarations for *Document classes
setGeneric("valid_doc", function(object) standardGeneric("valid_doc"))
# defined for fileDocument
types_list <- c("file", "dir", "url", "link")

setGeneric("filedocument", function(file, type) standardGeneric("filedocument"))
setGeneric("setName<-", function(object, value) standardGeneric("setName<-"))
setGeneric("setName", function(object, value) standardGeneric("setName"))
setGeneric("setWarn<-", function(object, value) standardGeneric("setWarn<-"))
setGeneric("setDir<-", function(object, value) standardGeneric("setDir<-"))
setGeneric("setExt<-", function(object, value) standardGeneric("setExt<-"))
setGeneric("getName", function(object) standardGeneric("getName"))

setGeneric("getDir", function(object) standardGeneric("getDir"))
setGeneric("getExt", function(object) standardGeneric("getExt"))
setGeneric("getType", function(object) standardGeneric("getType"))

setGeneric("getPath", function(object) standardGeneric("getPath"))

setGeneric("exist", function(object) standardGeneric("exist"))

setGeneric("create", function(object) standardGeneric("create"))

setGeneric("move", function(object, toFile) standardGeneric("move"))

setGeneric("rename", function(object, toFile) standardGeneric("rename"))

setGeneric("delete", function(object) standardGeneric("delete"))
setGeneric("infos", function(object, type = "all") standardGeneric("infos"))

setGeneric("isdir", function(object) standardGeneric("isdir"))
setGeneric("isempty", function(object) standardGeneric("isempty"))
setGeneric("calcExt", function(object) standardGeneric("calcExt"))
setGeneric("calcType", function(object) standardGeneric("calcType"))

#setGeneric("show", function(object) standardGeneric("show"))

# defined for xml_document

setGeneric("xmldocument", function(file) standardGeneric("xmldocument"))
setGeneric("set_content<-", function(object, value) standardGeneric("set_content<-"))

setGeneric("get_content", function(object) standardGeneric("get_content"))

setGeneric("load_content", function(object) standardGeneric("load_content"))
setGeneric("is_loaded", function(object) standardGeneric("is_loaded"))

setGeneric("is.xml_document", function(object) standardGeneric("is.xml_document"))

# xml manipulations

setGeneric("get_nodes", function(object, path = NULL) standardGeneric("get_nodes"))

setGeneric("get_attrs", function(object, path) standardGeneric("get_attrs"))

setGeneric("get_attrs_names", function(object, path) standardGeneric("get_attrs_names"))

setGeneric("get_attrs_values", function(object, path, attr_list, nodes_ids = NULL) standardGeneric("get_attrs_values"))

setGeneric("add_attrs", function(object, path, named_vector) standardGeneric("add_attrs"))

setGeneric("remove_attrs", function(object, path, attr_names) standardGeneric("remove_attrs"))

setGeneric("set_attrs_values", function(object, path, attr_name, values_list, nodes_ids = NULL) standardGeneric("set_attrs_values"))

setGeneric("set_values", function(object, path, values_list, nodes_ids = NULL) standardGeneric("set_values"))

setGeneric("get_values", function(object, path, nodes_ids = NULL) standardGeneric("get_values"))

setGeneric("save_xml_doc", function(object, xml_path) standardGeneric("save_xml_doc"))

setGeneric("clone_xml_doc", function(object) standardGeneric("clone_xml_doc"))

# adding and removing nodes to doc

setGeneric("add_nodes", function(object, nodes_to_add, parent_path = NULL) standardGeneric("add_nodes"))
# removing nodes

setGeneric("del_nodes", function(object, path) standardGeneric("del_nodes"))
