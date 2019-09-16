gen_param_sti <- function(workspace, par_names, par_values, file_name = "param.sti") {
  #' @title Generating a param.sti type file
  #' @description Generating a parameters forcing file from parameters names
  #' and parameters values vectors
  #' @param workspace Stics or JavaStics workspace path
  #' @param par_names vector of parameters names
  #' @param par_values vector of parameters values
  #' @param file_name file name to generate (default value: param.sti)
  #' @examples
  #' gen_param_sti(".", c("par1","par2"), c(1,2))
  #' gen_param_sti("/path/to/stics/workspace", c("par1","par2"), c(1,2))
  #' @export
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2019-06-03 11:56:53 +0200 (lun. 03 juin 2019) $
  #  $Author: plecharpent $
  #  $Revision: 1429 $
  # ----------------------------------------------------------------------
  #'

  file_path <- file.path(workspace, file_name)

  if (file.exists(file_path)) {
    file.remove(file_path)
  }

  # checking par consistency
  nb_par <- unique(c(length(par_names), length(par_values)))

  if (length(nb_par) > 1) {
    return(FALSE)
  }

  # TODO: checking if par_names exist ?

  # Writing file content
  con <- file(file_path, method = "w+")
  write(paste0(nb_par,"\n",paste0(sprintf("%s\n%f",par_names,par_values), collapse="\n")), con)
  close(con)


}
