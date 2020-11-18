test_that("box_interfejs1 is shiny tag", {
  expect_type(box_interfejs1("box_unical_id", "text"), "list")
  expect_true(class(box_interfejs1("box_unical_id", "text")) == "shiny.tag")
})
