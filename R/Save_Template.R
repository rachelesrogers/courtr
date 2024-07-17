#' Save RShiny Courtroom Study Template
#'
#' @param file_path path where template should be saved
#' @param overwrite TRUE to overwrite existing files
#'
#' @return RShiny app template will be saved in designated location
#' @export
#'
#' @examples
#' \dontrun{
#' Save_Template("your/file/path")
#' }
Save_Template <- function(file_path, overwrite=FALSE){
  fs::dir_copy(path=system.file("Generic_App", package="courtr"),
           new_path=paste0(file_path,"/Generic_App"), overwrite = overwrite)
}
