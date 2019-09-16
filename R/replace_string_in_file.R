replace_string_in_file <- function(file_path,target_string,replace_string,new_file_path=NULL) {
  #' @title Replacing a string in a file
  #' @description Replacing occurrences of a string by another one, in place or creating a new file
  #' @export

    if (!file.exists(file_path)){
    warning(paste(file_path,"doesn't exist, aborting !"))
    return()
  }
  in_place="i"
  redirect=""
  if (!is.null(new_file_path)) {
    in_place=""
    redirect=paste(" >",new_file_path)
  }
  cmd=paste0("perl -p",in_place," -e 's/",target_string,"/",replace_string,"/g ' ",file_path, redirect)
  # print(cmd)
  system(cmd)
}
