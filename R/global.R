# for generic functions declarations for *document classes
setGeneric("valid_doc", function(object) standardGeneric("valid_doc"))
# defined for file_document
setGeneric("filedocument", function(file) standardGeneric("filedocument"))
setGeneric("set_name<-", function(object, value) standardGeneric("set_name<-"))
setGeneric("set_name", function(object, value) standardGeneric("set_name"))
setGeneric("set_warn<-", function(object, value) standardGeneric("set_warn<-"))
setGeneric("set_dir<-", function(object, value) standardGeneric("set_dir<-"))
setGeneric("set_ext<-", function(object, value) standardGeneric("set_ext<-"))
setGeneric("get_name", function(object) standardGeneric("get_name"))

setGeneric("get_dir", function(object) standardGeneric("get_dir"))
setGeneric("get_ext", function(object) standardGeneric("get_ext"))

setGeneric("get_path", function(object) standardGeneric("get_path"))

setGeneric("exist", function(object) standardGeneric("exist"))

setGeneric("move", function(object, to_file) standardGeneric("move"))

setGeneric("rename", function(object, to_file) standardGeneric("rename"))

setGeneric("delete", function(object) standardGeneric("delete"))
setGeneric("infos", function(object, field = "all") standardGeneric("infos"))

setGeneric("isdir", function(object) standardGeneric("isdir"))
setGeneric("isempty", function(object) standardGeneric("isempty"))
setGeneric("calc_ext", function(object) standardGeneric("calc_ext"))

# defined for xml_document

setGeneric("xmldocument", function(file) standardGeneric("xmldocument"))
setGeneric(
  "set_content<-",
  function(object, value) standardGeneric("set_content<-")
)

setGeneric("get_content", function(object) standardGeneric("get_content"))

setGeneric("load_content", function(object) standardGeneric("load_content"))
setGeneric("is_loaded", function(object) standardGeneric("is_loaded"))

setGeneric(
  "is.xml_document",
  function(object) standardGeneric("is.xml_document")
)

# xml manipulations

setGeneric(
  "get_nodes",
  function(object, path = NULL) standardGeneric("get_nodes")
)

setGeneric("get_attrs", function(object, path) standardGeneric("get_attrs"))

setGeneric(
  "get_attrs_names",
  function(object, path) standardGeneric("get_attrs_names")
)

setGeneric(
  "get_attrs_values",
  function(object, path, attr_list, nodes_ids = NULL) {
    standardGeneric("get_attrs_values")
  }
)

setGeneric(
  "add_attrs",
  function(object, path, named_vector) standardGeneric("add_attrs")
)

setGeneric(
  "remove_attrs",
  function(object, path, attr_names) standardGeneric("remove_attrs")
)

setGeneric(
  "set_attrs_values",
  function(object, path, attr_name, values_list, nodes_ids = NULL) {
    standardGeneric("set_attrs_values")
  }
)

setGeneric(
  "set_values",
  function(object, path, values_list, nodes_ids = NULL) {
    standardGeneric("set_values")
  }
)

setGeneric(
  "get_values",
  function(object, path, nodes_ids = NULL) {
    standardGeneric("get_values")
  }
)

setGeneric(
  "save_xml_doc",
  function(object, xml_path) standardGeneric("save_xml_doc")
)

setGeneric("clone_xml_doc", function(object) standardGeneric("clone_xml_doc"))

# adding and removing nodes to doc

setGeneric(
  "add_nodes",
  function(object, nodes_to_add, parent_path = NULL) {
    standardGeneric("add_nodes")
  }
)

# removing nodes
setGeneric("del_nodes", function(object, path) standardGeneric("del_nodes"))


# freeing memory of an object
setGeneric("delete", function(object) standardGeneric("delete"))
