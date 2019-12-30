#' Expand stics names from parameters/variables simple names,
#' with indices (i.e. par(1), par(2),...regarding to parameter/variable dimensions)
#'
#' @param in_csv_file Input csv file path
#' @param out_csv_file Output csv file path
#' @param header_vec An optional header vector
#'
#'
#' @examples
#' \dontrun{
#'
#' csv_file <- "/path/to/javastics/dir/config/inputs.csv"
#' out_csv_file <- "/path/to/out/dir/config/inputs_xpanded.csv"
#'
#' SticsRFiles:::expand_stics_names(csv_file, out_csv_file)
#'
#' }
#'
#' @keywords internal
#'
expand_stics_names <- function(in_csv_file, out_csv_file, header_vec = NULL) {
  in_data <- utils::read.table(in_csv_file,sep=";",stringsAsFactors = F, header = F)
  if (base::is.null(header_vec)) {
    names(in_data) <- c("name","def","unit","param","dim","type","min","max","optim","n")
  }

  # getting param names to duplicate
  par_to_expand <- in_data[in_data$dim > 1,]
  out_df <- data.frame()

  # duplicating rows and concatenation
  for ( p in 1:dim(par_to_expand)[1] ) {
    par <- par_to_expand$name[p]
    #print(par)
    par_dim <- par_to_expand$dim[p]
    tmp <- par_to_expand[rep(p,par_dim), ]
    tmp$name <- paste0(par,sprintf("(%i)",1:par_dim))
    out_df <- rbind(out_df,tmp)
  }

  # treating parameters ending with 0
  par0 <- in_data[grep("[^0-9]0$",in_data$name),]
  new_names <- gsub("0$","",par0$name)
  existing <- new_names %in% in_data$name
  if (any(existing)) {
    par0 <- par0[!existing,]
    par0$name <- gsub("0$","",par0$name)
  }

  out_df <- rbind(in_data, out_df,par0)
  out_df <- out_df[order(out_df$name),]


  utils::write.table(out_df,file = out_csv_file,
                     sep=";", col.names = F, row.names = F,
                     quote = F, na = "")

  return(invisible(out_df))
}
