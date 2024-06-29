#' Format Testimony
#'
#' @param testimony_file_path path to testimony csv file
#'
#' @return a formatted csv file with appropriate div classes for speech bubbles
#' @export
#'
#' @examples

Format_Testimony <- function(testimony_file_path){
  `%>%` <- magrittr::`%>%`

combined_testimony <- utils::read.csv(testimony_file_path)

combined_testimony$before <- NA
combined_testimony$after <- NA

right_testimony <- combined_testimony %>% dplyr::filter(Bubble == "Right")

right_testimony$before = paste0("<div class='speech-bubble ",right_testimony$Speaker,
                                "-right'><div class='left-text'>")
right_testimony$after = paste0("</div><div class='",right_testimony$Speaker,"-image-right'></div></div><br/>")

left_testimony <- combined_testimony %>% dplyr::filter(Bubble == "Left")

left_testimony$before = paste0("<div class='speech-bubble ",
                         left_testimony$Speaker,"-left'><div class='",
                         left_testimony$Speaker,"-image-left'></div><div class='right-text'>")
left_testimony$after = "</div></div> "

center_testimony <- combined_testimony %>% dplyr::filter(Bubble == "Center")

center_testimony$before = paste0("<div class='speech-bubble ",
                               center_testimony$Speaker,"-center'><div class='",
                               center_testimony$Speaker,"-image-left'></div><div class='right-text'>")
center_testimony$after = "</div></div> "

narrator_testimony <- combined_testimony %>% dplyr::filter(Bubble == "None") %>%
  dplyr::mutate(before ="", after = "")

combined_testimony_final <- rbind(narrator_testimony, left_testimony, center_testimony, right_testimony)
combined_testimony_final <- combined_testimony_final[order(combined_testimony_final$Count),]

min_max <- combined_testimony_final %>% dplyr::group_by(Page) %>%
  dplyr::summarise(min_count=min(Count), max_count=max(Count)) %>% dplyr::ungroup()

combined_testimony_trial <- dplyr::left_join(combined_testimony_final,min_max)

combined_testimony_trial <- combined_testimony_trial %>%
  dplyr::mutate(before=ifelse(Count==min_count,paste("<div style='display:grid'>",before,sep=""), before)) %>%
  dplyr::mutate(after=ifelse(Count==max_count,paste(after,"</div>",sep=""), after))

combined_testimony_trial$combined <- paste(combined_testimony_trial$before, combined_testimony_trial$Text, sep=" ")
combined_testimony_trial$combined <- paste(combined_testimony_trial$combined, combined_testimony_trial$after, sep=" ")

return(combined_testimony_trial)
}
