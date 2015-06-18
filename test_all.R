library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(acotsp)
}

test_dir("tests/testthat")
