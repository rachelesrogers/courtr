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

fig_info <- utils::read.csv("figure_information.csv")
# input_info <- read.csv("input_information.csv")

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(

  # Application title
  shiny::titlePanel("Character Customization"),

  # Sidebar with a slider input for number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("clothes_choice", "Select Outfit:",
                  choices= unique(fig_info[fig_info$Part=="clothes",]$Label)),
      shiny::selectInput("head_choice", "Select Head:",
                  choices= unique(fig_info[fig_info$Part=="head",]$Label)),
      shiny::uiOutput('skinselect'),
      shiny::uiOutput('hairselect'),
      shiny::uiOutput('eyeselect'),
      shiny::conditionalPanel(condition= "output.vis_shirt",
                              shiny::uiOutput('shirtselect')),
      shiny::conditionalPanel(condition= "output.vis_pants",
                              shiny::uiOutput('pantsselect')),
      shiny::conditionalPanel(condition= "output.vis_suit",
                              shiny::uiOutput('suitselect')),
      shiny::downloadButton("download", "Download Character")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      shiny::imageOutput("characterPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  head_path <- shiny::reactive({
    paste0("www/head",input$head_choice,".svg")
  })

  body_path <- shiny::reactive({
    paste0("www/",input$clothes_choice,".svg")
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

  head_selection <- shiny::reactive({
    return(fig_info %>% filter(Part == "head", Label == input$head_choice))
  })

  possible_colors <- shiny::reactive({
    return(unique(c(head_selection()$Item, clothes_selection()$Item)))
  })

  default_eye <- shiny::reactive(head_selection()[head_selection()$Item=="eye",]$Color)

  output$eyeselect <- renderUI({colourpicker::colourInput("eye",
                                                          "Eye Color:",
                                                          default_eye())
  })

  default_hair <- shiny::reactive(head_selection()[head_selection()$Item=="hair",]$Color)

  output$hairselect <- renderUI({colourpicker::colourInput("hair",
                                                           "Hair Color:",
                                                           default_hair())
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

    finding_row_head<-mapply(grepl, "hair",head_split)

    head_split[finding_row_head,] <- change_fill(head_split[finding_row_head,], input$hair)

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
