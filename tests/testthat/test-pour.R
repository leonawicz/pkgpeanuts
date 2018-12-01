context("pour")


test_that("functions return as expected", {
  x <- .pkg_author("firstname", "lastname", "emailaddress", c("aut", "cre"),
                  comment = c(ORCID = "0000-1111-2222-3333"))
  expect_is(x, "list")
  expect_identical(names(x), "Authors@R")
})
