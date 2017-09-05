test_that("bandwidth_of_sinewave_is_zero", {
  testsound <- tuneR::normalize(tuneR::sine(4000))
  #Even though sine wave has bandwidth=0 meanspec() will have a small bandwidth
  a <- orthophonia::frequencySpectrumPowerQuartiles(testsound, 0, FALSE)
  expect_lt(a[[2]]-a[[1]], 0.05)
})

#Test that min_freq > sine freq fails cleanly

#Test bandwidth of multiple freq

