attributes_list2matrix <- function(attr_list) {
  # attr_list : list of named vectors
  col_names = unique(unlist(lapply(attr_list,names)))
  attr_mat = matrix(NA,length(attr_list),length(col_names))
  colnames(attr_mat) <- col_names
  
  for (l in 1:length(attr_list)) {
    col_idx=sapply(names(attr_list[[l]]), function(x) which(is.element(col_names,x)))
    attr_mat[l,col_idx] <- attr_list[[l]]
  }
  return(attr_mat)
}