library(jsonlite)

context("Bandwidth")
tests <- jsonlite::fromJSON("https://raw.githubusercontent.com/orthoptera-aao/orthophonia-tests/master/bandwidth.json")
for (i in 1:length(tests)) {
  test_that(tests[[i]]$desc_en, {
    if (tests[[i]]$r_testsound[[1]] == "func") {
      testsound <- eval(parse(text=tests[[i]]$r_testsound[[2]]))
    }
    if (class(testsound)[[1]] == "Wave") {
      args <- vector("list", length(tests[[i]]$other_args)+1)
      args[[1]] <- testsound
      for (j in 2:length(tests[[i]]$other_args)+1) {
        args[[j]] <- tests[[i]]$other_args[[j-1]]
      }
      result <- do.call(tests[[i]]$orthophonia_func, args = args)
      for (j in 1:length(tests[[i]]$tests)) {
        test_type = names(tests[[i]]$tests[j])
        do.call(test_type, list(result, eval(parse(text=tests[[i]]$tests[[j]][[1]]))))
      }
    } else {
      warning("Could not create a test file: ", tests[[i]]$desc_en)
    }
  })
}