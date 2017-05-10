context("pollyvote_object")

test_that("pollyvote objects can be created", {
  # create empty pollyvote container
  pv = create_pollyvote(id = "test_pv")
  assert_class(pv, "pollyvote")
})




