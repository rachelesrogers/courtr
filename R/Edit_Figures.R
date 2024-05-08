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
# input_info <- read.csv("input_information.csv")

ui <- shiny::fluidPage(

  # Application title
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
      shiny::downloadButton("download", "Download Character")
    ),

    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::imageOutput("characterPlot")
    )
  )
)

# Define server logic required to draw a histogram
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

  # output$color_inputs <- renderUI({
  #   list(paste(t(input_info$Question),collapse = ",br(),"))
  #   # list(input_info$Question)[[1]]
  # })


  image_processing <- shiny::reactive({

    file_head <- as.data.frame(paste(gsub("'","",readLines(head_path())), collapse = ""))

    head_split <-file_head %>% stringr::str_split(">") %>%
      as.data.frame(col.names="svg_file") %>% dplyr::filter(svg_file !="")

    head_split$svg_file <- paste0(head_split$svg_file, ">")

    finding_row_head<-mapply(grepl, "skin",head_split)

    head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$skin)

    finding_row_head<-mapply(grepl, "hair1",head_split)

    head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$hair)

    finding_row_head<-mapply(grepl, "hair2",head_split)

    head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$hair2)

    finding_row_head<-mapply(grepl, "hair_lines",head_split)

    head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$hair3)

    finding_row_head<-mapply(grepl, "glasses",head_split)

    head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$glasses)

    finding_row_head<-mapply(grepl, "eye",head_split)

    head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$eye)

    file_final_head <- apply(head_split,2,paste, collapse="")

    head_magic <- magick::image_read_svg(file_final_head, width=400)

    file_body <- as.data.frame(paste(gsub("'","",readLines(body_path())), collapse = ""))

    body_split <-file_body %>% stringr::str_split(">") %>%
      as.data.frame(col.names="svg_file") %>% dplyr::filter(svg_file !="")

    body_split$svg_file <- paste0(body_split$svg_file, ">")


    finding_row_body<-mapply(grepl, "skin",body_split)

    body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,], input$skin)

    finding_row_body<-mapply(grepl, "shirt",body_split)

    body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,], input$shirt)

    finding_row_body<-mapply(grepl, "pants",body_split)

    body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,], input$pants)

    finding_row_body<-mapply(grepl, "suit",body_split)

    body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,], input$suit)

    finding_row_body<-mapply(grepl, "tie",body_split)

    body_split[finding_row_body,] <- change_fill(body_split[finding_row_body,], input$tie)

    file_final_body <- apply(body_split,2,paste, collapse="")


    body_magic <- magick::image_read_svg(file_final_body, width=400)


    img <- c(body_magic, head_magic)

    combined <- magick::image_flatten(img)


    tmpfile <- magick::image_write(combined, tempfile(fileext='png'), format="png")

    list(src = tmpfile, contentType = "image/png", width="50%")})



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
