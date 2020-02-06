context("fft and periodogram estimators")

library(tidyverse)
library(testthat)
library(magrittr)

testthat::test_that("non-regular ts are detected", {

  sr <- 400
  ts <- tibble(
    t_c = seq(3, 8, by = 1/sr),
    t_na = replace(t_c, sample(length(t_c), size = 20), NA_real_),
    sig_c = rnorm(length(t_c)),
    sig_na = replace(t_c, sample(length(t_c), size = 10), NA_real_)
  )

  expect_warning(
    regular_ts(
      ts %>% transmute(t = t_na),
      signal = "sig_c",
      sr = 400
      ),
    regexp = "NA values in t"
    )
  #expect_false(regular_ts(ts %>% transmute(t = t_na), sr = 400))

  expect_warning(regular_ts(tibble(t = c(1,2,4), s = 3), signal = "s", sr = 400),
                 regexp = "t_i are not equidistant or inconsistent with samplerate")

  expect_warning(regular_ts(ts %>% transmute(t = t_c, s = 3), signal = "s", sr = 401),
                 regexp = "t_i are not equidistant or inconsistent with samplerate")

  expect_warning(regular_ts(ts %>% transmute(t = t_c, signal = sig_na),
                            sr = 400, signal = "signal"),
                 regexp = "NA values in signal")

  expect_true(regular_ts(ts %>% transmute(t = t_c, sig_c),
                         sr = sr, signal = "sig_c"))
})


test_that("fft runs", {
  sr <- 400
  data <- tibble(t = seq(0, 2, by = 1/sr), s = cos(2*pi*30*t))

})
