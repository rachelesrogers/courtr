test_that("template saves appropriately", {
  temp_location <- tempdir()

  Save_Template(temp_location, overwrite = TRUE)

  exists("Generic_App", list.files(temp_location))


})
