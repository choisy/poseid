library(gdpm)
context("Test if `breaks` returns correct output")

test_that("`breaks` returns the correct error message", {

  dengue <- getid(dengue, from = 1990, to = 1991)

  expect_error(breaks(dengue, style = "fail"))
  expect_error(breaks(dengue, n = 2,
                      fixedBreaks = c("its", "going", "to", "fail")))
  expect_error(breaks(dengue, n = 4, pal = heat.colors(3),
                      fixedBreaks = c("its", "going", "to", "fail", "again")))

})

test_that("`breaks` returns the correct output", {

  dengue <- getid(dengue, from = 1990, to = 1991)
  dengue$incidence_dengue <- 1
  dengue$mortality_dengue <- 0
  test1 <- breaks(dengue, "incidence_dengue")

  expect_equal(attr(test1, "breaks"), c(0, 1))

})
