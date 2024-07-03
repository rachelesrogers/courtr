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
  fs::dir_copy(path="inst/Generic_App",
           new_path=file_path, overwrite = overwrite)
}
