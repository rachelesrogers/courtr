test_that("template saves appropriately", {
  temp_location <- tempdir()

  Save_Template(temp_location, overwrite = TRUE)

  expect_true("Generic_App" %in% list.files(temp_location))

  expect_true(all(c("app.R", "Combined_Testimony.csv", "Combined_Testimony_Formatted.csv",
              "Database_creation.R", "Demographics.R", "Generic_App.Rproj",
              "Informed_Consent.txt", "Questions.R", "reading_dataset.R", "README",
              "www") %in% list.files(paste0(temp_location,"/Generic_App"))))

  unlink(paste0(temp_location,"/Generic_App"), recursive=TRUE)


})
