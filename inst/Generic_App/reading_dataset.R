library(DBI)
library(RSQLite)
library(tidyverse)

demo_dataset <- dbConnect(RSQLite::SQLite(), "demo.sqlite")

responses <- dbGetQuery(demo_dataset, "SELECT * FROM survey_responses")

wide_response <- responses %>%
  select(!c(time, page)) %>%
  pivot_wider(names_from = question, values_from = answer)

View(wide_response)

dbListTables(redo_dataset)

consent <- dbGetQuery(demo_dataset, "SELECT * FROM consent_page")

demo1 <- dbGetQuery(demo_dataset, "SELECT * FROM demographics1")

demo2 <- dbGetQuery(demo_dataset, "SELECT * FROM demographics2")

notes <- dbGetQuery(demo_dataset, "SELECT * FROM notepad")

View(consent)

View(demo1)

View(demo2)

View(notes)

dbDisconnect(demo_dataset)
