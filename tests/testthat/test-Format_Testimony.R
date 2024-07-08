test_that("unknown speaker works", {
  expected_result <-
    data.frame(Count = 1, Page = 1, Speaker = "jimmy", Bubble = "Right",
               Text = "testing unknown speaker",
               before = "<div style='display:grid'><div class='speech-bubble jimmy-right'><div class='left-text'>",
               after = "</div><div class='jimmy-image-right'></div></div><br/></div>",
               combined = "<div style='display:grid'><div class='speech-bubble jimmy-right'><div class='left-text'> testing unknown speaker </div><div class='jimmy-image-right'></div></div><br/></div>")
  testing <- data.frame(Count = 1, Page = 1, Speaker = "jimmy",
                        Bubble = "Right", Text = "testing unknown speaker")
  test_result <- Format_Testimony(testing)

  expect_identical(test_result, expected_result)
})

test_that("order is based on count", {
  testing <- data.frame(Count = 1:5, Page = rep(1,5), Speaker =
                          c("narrator","defense", "prosecution", "narrator", "defense"),
                        Bubble = c("None", "Right", "Left", "None", "Right"),
                        Text = c("A", "B", "C", "D", "E"))
  test_result <- Format_Testimony(testing)

  expect_identical(test_result$Count, 1:5)

  expect_identical(test_result$Text, c("A", "B", "C", "D", "E"))
})
