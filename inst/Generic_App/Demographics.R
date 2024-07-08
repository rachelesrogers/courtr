demo <- list(list(
  h1("Demographic Information"),
  br(),
  textInput("uniqueID", "Unique ID"),
  br(),
  checkboxGroupInput(
    "race", "Please specify your race or ethnicity (select all that apply):",
    c(
      "White", "Black or African American",
      "American Indian or Alaska Native", "Asian",
      "Native Hawaiian or Pacific Islander", "Hispanic", "Other"
    )
  ),
  br(),
  radioButtons("gender", "Please specify your gender:", c("Man", "Woman", "Other/non-binary"), selected=character(0)),
  br(),
  radioButtons("age", "Please specify your age:",
               c("18 - 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "Over 65"), selected=character(0)),
  br(),
  radioButtons(
    "income", "Please specify your total family income before taxes:",
    c(
      "Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999",
      "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999",
      "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $89,999",
      "$90,000 - $99,999", "$100,000 - $149,999", "More than $150,000"
    ), selected=character(0)
  ),
  br(),
  radioButtons("gunown", "Do you own a firearm?", c("Yes", "No"), selected=character(0)),
  br(),
  radioButtons(
    "guncomfort", "How comfortable are you with firearms?",
    c(
      "Extremely Comfortable", "Moderately Comfortable",
      "Slightly Comfortable", "Neither Comfortable nor Uncomfortable",
      "Slightly Uncomfortable", "Moderately Uncomfortable", "Extremely Uncomfortable"
    ), selected=character(0)
  ),
  br()
),
list(
  h1("Demographic Information"),
  radioButtons(
    "educ", "Please specify your education:",
    c(
      "Less than high school", "High school graduate",
      "Some college", "2 year degree", "4 year degree",
      "Professional degree", "Doctorate"
    ), selected=character(0)
  ),
  br(),
  radioButtons("vote", "I tend to vote:", c(
    "Republican", "Democratic",
    "No consistent preference", "Other"
  ), selected=character(0)),
  br(),
  radioButtons(
    "poli", "My political views tend to be:",
    c(
      "Very Liberal", "Somewhat Liberal", "Middle of the Road",
      "Somewhat Conservative", "Very Conservative", "Other"
    ), selected=character(0)
  ),
  br(),
  radioButtons(
    "arrest",
    "Have you or a member of your immediate family ever been arrested by the police?",
    c("Yes", "No"), selected=character(0)
  ),
  br(),
  selectInput("state", "In which state/territory do you currently reside?",
              c(
                "","AL", "AK", "AZ", "AR", "AS", "CA", "CM", "CO", "CT", "DE", "DC", "FL",
                "GA", "GU", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                "MD", "MA", "MI", "MN","MO", "MS", "MT", "NE", "NV", "NH", "NJ",
                "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI",
                "SC", "SD", "TN", "TT", "TX", "UT", "VA", "VT", "VI", "WA", "WV", "WI",
                "WY"
              ),
              width = "300px"
  ),
  br(),
  radioButtons("jury", "Have you ever served on a jury before?", c("Yes", "No"), selected=character(0)),
  br(),
  radioButtons(
    "jurycrim",
    "If you have served on a jury, did the trial involve criminal charges against the defendant?",
    c("Yes", "No", "N/A"), selected="N/A"
  ),
  br()
)

)
