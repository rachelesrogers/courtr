library(shiny)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(DBI)
library(RSQLite)
library(pool)

# A list of questions for the beginning of the survey. Separate pages by lists.
source("Demographics.R")

# A list of questions for the end of the survey, presented as one question per page
source("Questions.R")

# Completion Code for Survey Websites such as Prolific
completionCode <- "CODE"

# IRB/Survey Information document - shown on first page of app
consenttxt <- read.table("Informed_Consent.txt")

#Formatted document of testimony
testtxt<- read.csv("Combined_Testimony_Formatted.csv")

# --- Define UI -------
ui <- fluidPage(
  title = "Study Title",
  style="font-size:20px",
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(45%);
             left: calc(45%);
             }
             "
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),

  div(splitLayout(titlePanel(h1("STUDY TITLE", align="center",
  )),
  column(10,style="padding:30px;",
         progressBar(id="progress", value=0, display_pct = TRUE))), #Progress Bar as Percent
  style = "position:fixed; width: 100%; background-color: white;z-index: 1"),

  # Informed Consent/Informational Page
  conditionalPanel(condition= "input.informed==0",
                   column(width=8, offset=2,
                          wellPanel(style="margin-top:70px", p(uiOutput("informed_consent")),
                                    column(12, actionButton("informed", "I Agree"), align="center"),
                                    br()))
  ),

  # Notepad, included after demographics questions
  conditionalPanel(condition="input.demopage==output.demo_length & input.questionpage < output.num_quest",
                   wellPanel(style="background:url(Notebook.jpg);
                             position: fixed; width: 98%; z-index:1; margin-top:70px",
                             textAreaInput("notepad","Take Notes Here", rows=5, value=""),)),
  # Demographic Questions
  conditionalPanel(condition="input.informed==1 & input.demopage < output.demo_length",
                   column(width=8, offset=2,
                          wellPanel(style="margin-top:70px",uiOutput("demoquest"),
                                    column(12, actionButton("demopage", "Next"), align="center"),
                                    br()))),
  # Study Transcript Pages
  conditionalPanel(condition="input.demopage==output.demo_length &
                   input.testimonypage < output.testpages",
                   column(width=8, offset=2,
                          wellPanel(style="margin-top:290px", p(uiOutput("testimony"))),
                          column(12, actionButton("testimonypage", "Next"), align="center"))),
  # Question Pages - one question per page
  conditionalPanel(condition="input.testimonypage == output.testpages &
                   input.questionpage < output.num_quest",
                   column(width=8, offset=2,
                          wellPanel(style="margin-top:290px", uiOutput("finalquest"),
                                    column(12, actionButton("questionpage", "Next"), align="center"),
                                    br()))),
  # Completion Code Page
  conditionalPanel(condition="input.questionpage == output.num_quest",
                   wellPanel(style="margin-top:100px",sprintf("Completion Code: %s", completionCode)))

)

# --- server ------

# This is used to connect to the database - currently commented out to avoid recording testing procedures
# pool <- dbPool(drv = RSQLite::SQLite(), dbname = "demo.sqlite")

server <- function(input, output, session) {

  id <- NULL

  # Sampling between experimental conditions - as indicated in the Combined_Testimony.csv
  condition1 <- sample(c("A", "B", "C"),1, prob=c(1/3, 1/3, 1/3))
  condition2 <- sample(c("shorter", "longer"),1, prob=c(0.5, 0.5))
  condition3 <- sample(c("Yes", "No"),1, prob=c(0.5, 0.5))

  # List of questions for two of the experimental conditions - randomized by 'sample()' in the middle
  if (condition2 == "shorter"){
    questorder <- c(c("fixed_question1","fixed_question2","fixed_question3"),
                    sample(c("hidden_slider", "visible_slider", "hidden_wlabels",
                               "numeric_chance")),
                    c("comments"))
  } else if (condition2 == "longer"){
    questorder <- c(c("fixed_question1","fixed_question2","fixed_question3"),
                    sample(c("hidden_slider", "visible_slider", "hidden_wlabels",
                             "numeric_chance", "longer_question")),
                    c("comments"))
  }


  numquest <- length(questorder) #Number of questions based on each condition
  output$num_quest <- reactive(length(questorder))
  outputOptions(output, "num_quest", suspendWhenHidden = FALSE)

  output$demo_length <- reactive(length(demo)) # Number of demographic pages
  outputOptions(output, "demo_length", suspendWhenHidden = FALSE)

  # Assigning a non-identifiable random number to match participants across databases
  random_number <- runif(1,0,100)
  start_time <- Sys.time() #Time participant starts the survey
  answer <- reactiveVal() # variable to record study answers
  question <- reactiveVal() # variable to record study questions
  question_verification <- reactiveVal(0) # used to verify that participants answer questions
  prob_counter <- reactiveVal(0) # used to verify that participants move marker on continuous scales

  # Subsetting transcript based on experimental conditions
  subset_testimony<-testtxt %>%
    subset((Condition_1=="All"|Condition_1==condition1) &
             (Condition_2=="All"|Condition_2==condition2) &
             (Condition_3=="All"|Condition_3==condition3)) %>%
    aggregate(combined ~ Page, paste, collapse=" ")

  # Determining the number of pages in the testimony
  output$testpages <- reactive(length(unique(subset_testimony$Page)))
  servpages <- reactive(length(unique(subset_testimony$Page)))

  outputOptions(output, "testpages", suspendWhenHidden = FALSE)

  #https://stackoverflow.com/questions/65030433/adding-a-count-button-to-a-shiny-app-that-interacts-with-updatenumericinput

  # Page counter used for progress bar
  counter <- reactiveVal(0)
  observeEvent(input$informed | input$demopage | input$testimonypage | input$questionpage, {
    shinyjs::runjs("window.scrollTo(0, 0)") # Scroll to top of each new page
    newcount <- counter() + 1
    counter(newcount)
  })

  #Update progress bar
  observe({
    updateProgressBar(session = session, id = "progress",
                      value = (counter()/(servpages() + 4 + numquest))*100)
  })

  # Rendering study pages
  output$informed_consent <- renderUI(HTML(consenttxt[1,]))

  output$testimony <- renderUI(HTML(subset_testimony[input$testimonypage+1,]$combined))

  output$demoquest <- renderUI(demo[input$demopage+1])

  output$finalquest <- renderUI(questions[[questorder[[input$questionpage+1]]]])

  # Creating dataframe for data gathered on informed consent page
  consentans <- reactive({
    return(data.frame(
      start_time = start_time,
      time = Sys.time(),
      page = counter(),
      randomnumber = random_number,
      condition1 = condition1,
      condition2 = condition2,
      condition3 = condition3
    ))
  })

  # Recording Consent Information - include when database is connected and active
  # observeEvent(input$informed,{
  #              con <- localCheckout(pool, env = parent.frame())
  #              dbAppendTable(con, "consent_page", consentans())
  #              })

  # Creating dataframe for data gathered on first demographics page
  # Values correspond to questions in demographics file
  demo1ans <- reactive({
    return(data.frame(
      uniqueid = input$uniqueID,
      race = paste(input$race, collapse = ", "),
      gender = input$gender,
      age = input$age,
      income = input$income,
      gunown = input$gunown,
      guncomfort = input$guncomfort,
      page = counter(),
      time = Sys.time(),
      randomnumber = random_number,
      start_time = start_time,
      condition1 = condition1,
      condition2 = condition2,
      condition3 = condition3
    ))
  })

  # Creating dataframe for data gathered on second demographics page
  demo2ans <- reactive({
    return(data.frame(
      uniqueid = input$uniqueID,
      education = input$educ,
      vote = input$vote,
      political = input$poli,
      arrest = input$arrest,
      state = input$state,
      jury = input$jury,
      crimejury = input$jurycrim,
      page = counter(),
      time = Sys.time(),
      randomnumber = random_number,
      start_time = start_time,
      condition1 = condition1,
      condition2 = condition2,
      condition3 = condition3
    ))
  })

  # Functions for ensuring participants answer questions
  validate_input_list <- function(x, vars) {
    list_of_needs <- purrr::map_lgl(vars, ~ !is.null(x[[.]]))
    all(list_of_needs)
  }

  btn_status <- function(vars) {
    observe({
      page_filled <- validate_input_list(input, vars)
      if (!page_filled) {
        question_verification(0)
      } else {
        question_verification(1)
      }
    })
  }
  ###### Button Status - Validating Questions ##########
  btn_status( c(
    "uniqueID", "race", "gender", "age", "income",
    "gunown", "guncomfort"
  ))
  btn_status(c(
    "educ", "vote", "poli", "arrest", "state",
    "jury", "jurycrim"
  ))

  # Writing demographic answers to database - include when database is connected
  observeEvent(input$demopage,{
    # con <- localCheckout(pool, env = parent.frame())
    # if (input$demopage ==1){
    # dbAppendTable(con, "demographics1", demo1ans())} else if (input$demopage ==2){
    #   dbAppendTable(con, "demographics2", demo2ans())
    # }
    question_verification(0) # Resetting Question Verification
  })

  # Creating dataframe with notepad information
  noteans <- reactive({
    return(data.frame(
      uniqueid = input$uniqueID,
      page = counter(),
      time = Sys.time(),
      randomnumber = random_number,
      start_time = start_time,
      notes = input$notepad,
      condition1 = condition1,
      condition2 = condition2,
      condition3 = condition3
    ))
  })


  # Recording notepad information to database - include when database is active
  # observeEvent(input$testimonypage,{
  #   con <- localCheckout(pool, env = parent.frame())
  #   dbAppendTable(con, "notepad", noteans())
  # })

  # Adding a two second delay on the testimony page button
  observeEvent(input$testimonypage,{
    shinyjs::disable("testimonypage")
    # Initialize the timer, 10 seconds, not active.
    timer <- reactiveVal(2)
    active <- reactiveVal(TRUE)
    shinyjs::logjs("2s timer start")
    shinyjs::delay(2000, {
      shinyjs::logjs("timer up, enable next btn")
      shinyjs::enable("testimonypage")
    })
  })

# Record questions and answers for each question page at the end of the testimony
  observeEvent(input$fixed_question1, {
    ans_temp <- input$fixed_question1
    answer(ans_temp)
    question("fixed_question1")
  })
  btn_status(c("fixed_question1")) # only activate next button when something is selected

  observeEvent(input$fixed_question2, {
    ans_temp <- input$fixed_question2
    answer(ans_temp)
    question("fixed_question2")
  })
  btn_status(c("fixed_question2"))

  observeEvent(input$fixed_question3, {
    ans_temp <- input$fixed_question3
    answer(ans_temp)
    question("fixed_question3")
  })
  btn_status(c("fixed_question3"))

  observeEvent(input$hidden_slider, {
    ans_temp <- input$hidden_slider
    answer(ans_temp)
    question("hidden_slider")

    newcount <- prob_counter() + 1
    prob_counter(newcount)
    if (prob_counter()==1){
      question_verification(0)
    } else if (prob_counter() > 1){
      question_verification(1)
    }
  })

  observeEvent(input$visible_slider, {
    ans_temp <- input$visible_slider
    answer(ans_temp)
    question("visible_slider")

    newcount <- prob_counter() + 1
    prob_counter(newcount)
    if (prob_counter()==1){
      question_verification(0)
    } else if (prob_counter() > 1){
      question_verification(1)
    }
  })

  observeEvent(input$hidden_wlabels, {
    ans_temp <- input$hidden_wlabels
    answer(ans_temp)
    question("hidden_wlabels")

    newcount <- prob_counter() + 1
    prob_counter(newcount)
    if (prob_counter()==1){
      question_verification(0)
    } else if (prob_counter() > 1){
      question_verification(1)
    }
  })

  observeEvent(input$comments, {
    ans_temp <- input$comments
    answer(ans_temp)
    question("comments")
  })
  btn_status(c("comments"))

  observeEvent(input$longer_question, {
    ans_temp <- input$longer_question
    answer(ans_temp)
    question("longer_question")
  })
  btn_status(c("longer_question"))

    observeEvent(input$chance_num | input$chance_denom,{
    ans_temp <- paste0(input$chance_num,",", input$chance_denom)
    answer(ans_temp)
    question("numeric_chance")
    if (isTruthy(input$chance_num & input$chance_denom)){
      if (input$chance_num > input$chance_denom){
        question_verification(0)
        id <<- showNotification(
          "First number must be less than or equal to second number",
          duration = 10,
          closeButton = TRUE,
          type = "error"
        )
      } else{
        question_verification(1)
      }
    }
  })


  # Creating data frame for each response
  responseans <- reactive({
    return(data.frame(
      uniqueid = input$uniqueID,
      page = counter(),
      time = Sys.time(),
      randomnumber = random_number,
      start_time = start_time,
      question = question(),
      answer = answer(),
      condition1 = condition1,
      condition2 = condition2,
      condition3 = condition3
    ))
  })
  # Recording response information to database - include when database is active
  observeEvent(input$questionpage,{
    # con <- localCheckout(pool, env = parent.frame())
    # dbAppendTable(con, "survey_responses", responseans())
    question_verification(0)
    prob_counter(0)
  })

  observe({
    shinyjs::toggleState("questionpage", question_verification() == 1)
  })

  observe({
    shinyjs::toggleState("demopage", question_verification() == 1)
  })


}

# Run the application
shinyApp(ui = ui, server = server)
