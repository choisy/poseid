context("Test if `ll2incidence` returns the correct output")

test_that("ll2incidence` returns the correct incidence with a vector of Date", {

  date_year <- c(rep(as.Date("2011-01-01"), 2),
                 rep(as.Date("2012-01-01"), 1),
                 rep(as.Date("2013-01-01"), 4))

  expect_equal(ll2incidence(date_year, "year")$incidence,
               c(2, 1, 4))

  expect_equal(ll2incidence(date_year, "year")$middle,
               as.POSIXct(c("2011-07-02 12:00:00 UTC",
                            "2012-07-02 00:00:00 UTC",
                            "2013-07-02 12:00:00 UTC"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "month")$incidence,
               c(2, rep(0, 11), 1, rep(0, 11), 4))

  expect_equal(ll2incidence(date_year, "quarter")$incidence,
               c(2, rep(0, 3), 1, rep(0, 3), 4))

  expect_equal(ll2incidence(date_year, "quarter")$middle,
               as.POSIXct(c("2011-02-15 00:00:00", "2011-05-16 12:00:00",
                            "2011-08-16 00:00:00", "2011-11-16 00:00:00",
                            "2012-02-15 12:00:00", "2012-05-16 12:00:00",
                            "2012-08-16 00:00:00", "2012-11-16 00:00:00",
                            "2013-02-15 00:00:00"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "week")$incidence,
               c(2, rep(0, 52), 1, rep(0, 51), 4))

  expect_equal(ll2incidence(date_year, "day")$incidence,
               c(2, rep(0, 364), 1, rep(0, 365), 4))

})

test_that("ll2incidence` returns the correct incidence with a vector of
          format POSIXct", {

  date_year <- as.POSIXct(c(rep("2011-01-01 UTC", 2), rep("2012-01-01 UTC", 1),
                 rep("2013-01-01 UTC", 4)), tz = "UTC")

  expect_equal(ll2incidence(date_year, "year")$incidence,
               c(2, 1, 4))

  expect_equal(ll2incidence(date_year, "year")$middle,
               as.POSIXct(c("2011-07-02 12:00:00 UTC",
                            "2012-07-02 00:00:00 UTC",
                            "2013-07-02 12:00:00 UTC"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "month")$incidence,
               c(2, rep(0, 11), 1, rep(0, 11), 4))

  expect_equal(ll2incidence(date_year, "quarter")$incidence,
               c(2, rep(0, 3), 1, rep(0, 3), 4))

  expect_equal(ll2incidence(date_year, "quarter")$middle,
               as.POSIXct(c("2011-02-15 00:00:00", "2011-05-16 12:00:00",
                            "2011-08-16 00:00:00", "2011-11-16 00:00:00",
                            "2012-02-15 12:00:00", "2012-05-16 12:00:00",
                            "2012-08-16 00:00:00", "2012-11-16 00:00:00",
                            "2013-02-15 00:00:00"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "week")$incidence,
               c(2, rep(0, 52), 1, rep(0, 51), 4))

  expect_equal(ll2incidence(date_year, "day")$incidence,
               c(2, rep(0, 364), 1, rep(0, 365), 4))

})


test_that("ll2incidence` returns the correct incidence with a data frame of
          format POSIXct", {

  date_year <- as.POSIXct(c(rep("2011-01-01 UTC", 2), rep("2012-01-01 UTC", 1),
                            rep("2013-01-01 UTC", 4)), tz = "UTC")
  date_year <- data.frame(date_year = date_year)

  expect_equal(ll2incidence(date_year, "year")$incidence,
                         c(2, 1, 4))

  expect_equal(ll2incidence(date_year, "year")$middle,
                         as.POSIXct(c("2011-07-02 12:00:00 UTC",
                                      "2012-07-02 00:00:00 UTC",
                                      "2013-07-02 12:00:00 UTC"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "month")$incidence,
                         c(2, rep(0, 11), 1, rep(0, 11), 4))

  expect_equal(ll2incidence(date_year, "quarter")$incidence,
                         c(2, rep(0, 3), 1, rep(0, 3), 4))

  expect_equal(ll2incidence(date_year, "quarter")$middle,
               as.POSIXct(c("2011-02-15 00:00:00", "2011-05-16 12:00:00",
                            "2011-08-16 00:00:00", "2011-11-16 00:00:00",
                            "2012-02-15 12:00:00", "2012-05-16 12:00:00",
                            "2012-08-16 00:00:00", "2012-11-16 00:00:00",
                            "2013-02-15 00:00:00"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "week")$incidence,
                         c(2, rep(0, 52), 1, rep(0, 51), 4))

  expect_equal(ll2incidence(date_year, "day")$incidence,
                         c(2, rep(0, 364), 1, rep(0, 365), 4))

})

test_that("ll2incidence` returns the correct incidence with a data frame of
          format Date", {

  date_year <- c(rep(as.Date("2011-01-01"), 2),
                 rep(as.Date("2012-01-01"), 1),
                 rep(as.Date("2013-01-01"), 4))
  date_year <- data.frame(date_year = date_year)

  expect_equal(ll2incidence(date_year, "year")$incidence,
               c(2, 1, 4))

  expect_equal(ll2incidence(date_year, "year")$middle,
               as.POSIXct(c("2011-07-02 12:00:00 UTC",
                            "2012-07-02 00:00:00 UTC",
                            "2013-07-02 12:00:00 UTC"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "month")$incidence,
               c(2, rep(0, 11), 1, rep(0, 11), 4))

  expect_equal(ll2incidence(date_year, "quarter")$incidence,
               c(2, rep(0, 3), 1, rep(0, 3), 4))

  expect_equal(ll2incidence(date_year, "quarter")$middle,
               as.POSIXct(c("2011-02-15 00:00:00", "2011-05-16 12:00:00",
                            "2011-08-16 00:00:00", "2011-11-16 00:00:00",
                            "2012-02-15 12:00:00", "2012-05-16 12:00:00",
                            "2012-08-16 00:00:00", "2012-11-16 00:00:00",
                            "2013-02-15 00:00:00"), tz = "UTC"))

  expect_equal(ll2incidence(date_year, "week")$incidence,
               c(2, rep(0, 52), 1, rep(0, 51), 4))

  expect_equal(ll2incidence(date_year, "day")$incidence,
               c(2, rep(0, 364), 1, rep(0, 365), 4))

})


test_that("ll2incidence` returns the correct error", {

  date_year <- c(rep(as.Date("2011-01-01"), 2),
                   rep(as.Date("2012-01-01"), 1),
                   rep(as.Date("2013-01-01"), 4))

  date_year_p <- as.POSIXct(c(rep("2011-01-01 UTC", 2),
                              rep("2012-01-01 UTC", 1),
                              rep("2013-01-01 UTC", 4)), tz = "UTC")
  date_year_df <- data.frame(date_year = date_year, date_year_p = date_year_p)


  expect_error(ll2incidence(date_year_df, "day"), regexp = NULL)

  expect_error(ll2incidence(as.numeric(date_year), "day"), regexp = NULL)

  expect_error(ll2incidence(as.character(date_year), "day"), regexp = NULL)

  expect_error(ll2incidence(date_year, "dqy"), regexp = NULL)

  expect_error(ll2incidence(date_year_p, c("day", "month")), regexp = NULL)

})
