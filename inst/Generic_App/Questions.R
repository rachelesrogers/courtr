questions <- list(
 fixed_question1 = list(
    radioButtons(
      "fixed_question1",
      "This question is fixed to always appear first after the testimony.
      Questions can be found in the Questions.R file.",
      c("Awesome!", "Great.", "Not Great."), selected=character(0)
    )
  ),

  fixed_question2 = list(
    textInput("fixed_question2", "This question is fixed to be second.
              Note that the name of the question in the Questions.R file appears twice -
              as the name of the list and as the reference to the question itself.", value="")
  ),

 fixed_question3 = list(
    radioButtons(
      "fixed_question3", "This is the third fixed question. All other questions are randomized.",
      c("Click to continue"), selected=character(0)
    )
    ),

hidden_slider = list(
  tags$head(tags$style('.irs-from, .irs-to, .irs-min, .irs-max {
            visibility: visible !important;
    } .irs-single {
            visibility: hidden !important;
    }')),
  sliderInput("hidden_slider",
              label = "This is a slider with hidden tick marks and values.",
              min = 0, max = 100, value = 50,
              ticks=FALSE,
              animate=FALSE
  )
),

visible_slider = list(
  tags$head(tags$style('.irs-single {
            visibility: visible !important;
    }')),
  sliderInput("visible_slider",
              label = "This slider has visible inputs. Note it must be moved to continue.",
              min = 0, max = 100, value = 50
  )
),

hidden_wlabels = list(
  tags$head(tags$style('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
            visibility: hidden !important;
    }')),
  fluidRow(column(2, align="center", p("minimum",
                                       style="padding:20px; margin-top:80px;")),
           column(8,    sliderInput("hidden_wlabels",
                                    label = "This slider features end point labels, as well as hidden values.",
                                    min = 0, max = 100, value = 50,
                                    ticks=FALSE,
                                    animate=FALSE)),
           column(2, align="center", p("maximum",
                                       style="padding:20px; margin-top:80px;")))
),

numeric_chance = list(
                   fluidRow(
                     column(4,align="center", p("There is about", style="padding:20px;")),
                     column(2,
                   shinyWidgets::autonumericInput("chance_num", "",
                                                  value=NA, minimumValue=1,
                                                  maximumValue=1000000000000)),
                   column(3, align="center", p("chance(s) in", style="padding:20px;")),
                   column(2,
                   shinyWidgets::autonumericInput("chance_denom",
                                                  "", value=NA, minimumValue=1,
                                                  maximumValue=1000000000000))),
  fluidRow(column(7, align="center", p("that this question features multiple numeric inputs.",
                                       style="padding:20px;")))
),

comments = list(
  textInput("comments", "Do you have any thoughts about the study
                              that you would like to share? This is fixed as
            the final question.", value = "")
),

#################### Algorithm Questions #############################

longer_question = list(
  radioButtons("longer_question",
               "Note this question only appears when Condition_2 is set as longer",
               choices = c("Option 1",
                           "Option 2",
                           "Option 3",
                           "Option 4",
                           "Option 5"),
               selected = character(0))
)

)




