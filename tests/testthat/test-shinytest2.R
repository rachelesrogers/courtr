library(shinytest2)

test_that("{shinytest2} recording: Generic_App_start", {
  skip_on_cran()
  appdir <- system.file(package = "courtr", "Generic_App")
  app <- AppDriver$new(variant=platform_variant(),appdir, name = "Generic_App")

  app$expect_screenshot()
})
