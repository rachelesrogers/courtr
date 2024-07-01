#' Edit Color of Cartoon Figures
#'
#' @return A Shiny app allowing for character editing and downloading
#' @export
#'
#' @examples
#' Edit_Figures()
Edit_Figures <- function(){
  `%>%` <- magrittr::`%>%`
change_fill <- function(file_contents, new_fill = "#aaaaff") {
  stringr::str_replace_all(file_contents, "fill:#[0-f]{6};", sprintf("fill:%s;", new_fill))
}

fig_info <- figure_information

ui <- shiny::fluidPage(
  shiny::titlePanel("Character Customization"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("clothes_choice", "Select Outfit:",
                  choices= unique(fig_info[fig_info$Part=="clothes",]$Label)),
      shiny::selectInput("head_choice", "Select Head:",
                  choices= unique(fig_info[fig_info$Part=="head",]$Label)),
      shiny::uiOutput('skinselect'),
      shiny::uiOutput('hairselect'),
      shiny::conditionalPanel(condition= "output.vis_hair2",
                       shiny::uiOutput('hair2select')),
      shiny::conditionalPanel(condition= "output.vis_hair3",
                       shiny::uiOutput('hair3select')),
      shiny::uiOutput('eyeselect'),
      shiny::conditionalPanel(condition= "output.vis_glasses",
                              shiny::uiOutput('glassesselect')),
      shiny::conditionalPanel(condition= "output.vis_shirt",
                              shiny::uiOutput('shirtselect')),
      shiny::conditionalPanel(condition= "output.vis_pants",
                              shiny::uiOutput('pantsselect')),
      shiny::conditionalPanel(condition= "output.vis_suit",
                              shiny::uiOutput('suitselect')),
      shiny::conditionalPanel(condition= "output.vis_tie",
                       shiny::uiOutput('tieselect')),
      shiny::conditionalPanel(condition= "output.vis_shoes",
                              shiny::uiOutput('shoesselect')),
      shiny::downloadButton("download", "Download Character")
    ),

    shiny::mainPanel(
      shiny::imageOutput("characterPlot")
    )
  )
)

server <- function(input, output) {

  head_path <- shiny::reactive({
    paste0("inst/www/head",input$head_choice,".svg")
  })

  body_path <- shiny::reactive({
    paste0("inst/www/",input$clothes_choice,".svg")
  })



  clothes_selection <- shiny::reactive({
    return(fig_info %>% dplyr::filter(Part == "clothes", Label == input$clothes_choice))
  })

  output$vis_shirt <- shiny::reactive({'shirt' %in% clothes_selection()$Item})

  shiny::outputOptions(output, "vis_shirt", suspendWhenHidden = FALSE)

  output$vis_pants <- shiny::reactive({'pants' %in% clothes_selection()$Item})

  shiny::outputOptions(output, "vis_pants", suspendWhenHidden = FALSE)

  output$vis_suit <- shiny::reactive({'suit' %in% clothes_selection()$Item})

  shiny::outputOptions(output, "vis_suit", suspendWhenHidden = FALSE)

  output$vis_tie <- shiny::reactive({'tie' %in% clothes_selection()$Item})

  shiny::outputOptions(output, "vis_tie", suspendWhenHidden = FALSE)

  output$vis_shoes <- shiny::reactive({'shoes' %in% clothes_selection()$Item})

  shiny::outputOptions(output, "vis_shoes", suspendWhenHidden = FALSE)

  head_selection <- shiny::reactive({
    return(fig_info %>% dplyr::filter(Part == "head", Label == input$head_choice))
  })

  output$vis_hair2 <- shiny::reactive({'hair2' %in% head_selection()$Item})

  shiny::outputOptions(output, "vis_hair2", suspendWhenHidden = FALSE)

  output$vis_hair3 <- shiny::reactive({'hair3' %in% head_selection()$Item})

  shiny::outputOptions(output, "vis_hair3", suspendWhenHidden = FALSE)

  output$vis_glasses <- shiny::reactive({'glasses' %in% head_selection()$Item})

  shiny::outputOptions(output, "vis_glasses", suspendWhenHidden = FALSE)

  possible_colors <- shiny::reactive({
    return(unique(c(head_selection()$Item, clothes_selection()$Item)))
  })

  default_eye <- shiny::reactive(head_selection()[head_selection()$Item=="eye",]$Color)

  output$eyeselect <- shiny::renderUI({colourpicker::colourInput("eye",
                                                          "Eye Color:",
                                                          default_eye())
  })

  default_hair <- shiny::reactive(head_selection()[head_selection()$Item=="hair",]$Color)

  output$hairselect <- shiny::renderUI({colourpicker::colourInput("hair",
                                                           "Hair Color:",
                                                           default_hair())
  })

  default_hair2 <- shiny::reactive(head_selection()[head_selection()$Item=="hair2",]$Color)

  output$hair2select <- shiny::renderUI({
    colourpicker::colourInput("hair2", "Secondary Hair Color:", default_hair2())
  })

  default_hair3 <- shiny::reactive(head_selection()[head_selection()$Item=="hair3",]$Color)

  output$hair3select <- shiny::renderUI({
    colourpicker::colourInput("hair3", "Hair Line Color:", default_hair3())
  })

  default_glasses <- shiny::reactive(head_selection()[head_selection()$Item=="glasses",]$Color)

  output$glassesselect <- shiny::renderUI({
    colourpicker::colourInput("glasses", "Glasses Color:", default_glasses())
  })

  default_skin <- shiny::reactive(head_selection()[head_selection()$Item=="skin",]$Color)

  output$skinselect <- shiny::renderUI({colourpicker::colourInput("skin",
                                                           "Skin Color:",
                                                           default_skin())
  })

  default_shirt <- shiny::reactive(clothes_selection()[clothes_selection()$Item=="shirt",]$Color)

  output$shirtselect <- shiny::renderUI({colourpicker::colourInput("shirt",
                                                            "Shirt Color:",
                                                            default_shirt())
  })

  default_pants <- shiny::reactive(clothes_selection()[clothes_selection()$Item=="pants",]$Color)

  output$pantsselect <- shiny::renderUI({colourpicker::colourInput("pants",
                                                            "Pants Color:",
                                                            default_pants())
  })

  default_suit <- shiny::reactive(clothes_selection()[clothes_selection()$Item=="suit",]$Color)

  output$suitselect <- shiny::renderUI({
    colourpicker::colourInput("suit", "Suit Color:", default_suit())
  })

  default_tie <- shiny::reactive(clothes_selection()[clothes_selection()$Item=="tie",]$Color)

  output$tieselect <- shiny::renderUI({
    colourpicker::colourInput("tie", "Tie Color:", default_tie())
  })

  default_shoes <- shiny::reactive(clothes_selection()[clothes_selection()$Item=="shoes",]$Color)

  output$shoesselect <- shiny::renderUI({
    colourpicker::colourInput("shoes", "Shoe Color:", default_shoes())
  })

  image_processing <- shiny::reactive({

    file_head <- as.data.frame(paste(gsub("'","",readLines(head_path())), collapse = ""))

    head_split <-file_head %>% stringr::str_split(">") %>%
      as.data.frame(col.names="svg_file") %>% dplyr::filter(svg_file !="")

    head_split$svg_file <- paste0(head_split$svg_file, ">")

    file_body <- as.data.frame(paste(gsub("'","",readLines(body_path())), collapse = ""))

    body_split <-file_body %>% stringr::str_split(">") %>%
      as.data.frame(col.names="svg_file") %>% dplyr::filter(svg_file !="")

    body_split$svg_file <- paste0(body_split$svg_file, ">")

    row_remove <- tail(head_split, -3)

    body_short <- head(body_split, -1)
    combined_split <- rbind(body_short, row_remove)

    finding_row<-mapply(grepl, "skin",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$skin)

    finding_row<-mapply(grepl, "hair1",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$hair)

    finding_row<-mapply(grepl, "hair2",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$hair2)

    finding_row<-mapply(grepl, "hair_lines",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$hair3)

    finding_row<-mapply(grepl, "glasses",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$glasses)

    finding_row<-mapply(grepl, "eye",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$eye)

    finding_row<-mapply(grepl, "skin",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$skin)

    finding_row<-mapply(grepl, "shirt",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$shirt)

    finding_row<-mapply(grepl, "pants",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$pants)

    finding_row<-mapply(grepl, "suit",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$suit)

    finding_row<-mapply(grepl, "tie",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$tie)

    finding_row<-mapply(grepl, "shoes",combined_split)

    combined_split[finding_row,] <- change_fill(combined_split[finding_row,], input$shoes)

    file_final_combined<- apply(combined_split,2,paste, collapse="")


    combined_magic <- magick::image_read_svg(file_final_combined, width=400)

    combined <- magick::image_fill(combined_magic, color = "transparent",
                           refcolor = "white",
                           fuzz=30,
                           point = "+1+1")


    tmpfile <- magick::image_write(combined, tempfile(fileext='png'), format="png")

    list(src = tmpfile, contentType = "image/png", width="50%")
    })



  output$characterPlot <- shiny::renderImage({image_processing()}, deleteFile = FALSE)

  output$download <- shiny::downloadHandler(
    filename = "Character.png",
    content = function(file) {
      img <- image_processing()$src
      file.copy(img, file)
    })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
}
