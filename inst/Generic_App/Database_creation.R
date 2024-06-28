library(DBI)
library(RSQLite)

demo_dataset <- dbConnect(RSQLite::SQLite(), "demo.sqlite")

consent <- data.frame(
  start_time = character(),
  time = character(),
  page = numeric(),
  randomnumber = numeric(),
  condition1 = character(),
  condition2 = character(),
  condition3 = character()
)

dbWriteTable(demo_dataset, "consent_page", consent)

demographics1 <- data.frame(
  uniqueid = character(),
  race = character(),
  gender = character(),
  age = character(),
  income = character(),
  gunown = character(),
  guncomfort = character(),
  page = numeric(),
  time = character(),
  randomnumber = numeric(),
  start_time = character(),
  condition1 = character(),
  condition2 = character(),
  condition3 = character()
)

dbWriteTable(demo_dataset, "demographics1", demographics1)

demographics2 <- data.frame(
  uniqueid = character(),
  education = character(),
  vote = character(),
  political = character(),
  arrest = character(),
  state = character(),
  jury = character(),
  crimejury = character(),
  page = numeric(),
  time = character(),
  randomnumber = numeric(),
  start_time = character(),
  condition1 = character(),
  condition2 = character(),
  condition3 = character()
)

dbWriteTable(demo_dataset, "demographics2", demographics2)

notepad <- data.frame(
  uniqueid = character(),
  page = numeric(),
  time = character(),
  randomnumber = numeric(),
  start_time = character(),
  notes = character(),
  condition1 = character(),
  condition2 = character(),
  condition3 = character()
)

dbWriteTable(demo_dataset, "notepad", notepad)

survey_responses <- data.frame(
  uniqueid = character(),
  page = numeric(),
  time = character(),
  randomnumber = numeric(),
  start_time = character(),
  question = character(),
  answer = character(),
  condition1 = character(),
  condition2 = character(),
  condition3 = character()
)

dbWriteTable(demo_dataset, "survey_responses", survey_responses)

dbDisconnect(demo_dataset)
