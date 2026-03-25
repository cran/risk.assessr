test_that("returns NULL early when get_package_tarfile() returns NULL", {
  mockery::stub(assess_pkg_r_package, "get_package_tarfile", function(package_name, version = NA, repos = NULL) NULL)
  
  # guards: these should NOT be called if tarball is NULL
  called_modify <- FALSE
  mockery::stub(assess_pkg_r_package, "modify_description_file", function(x) { called_modify <<- TRUE; x })
  called_setup <- FALSE
  mockery::stub(assess_pkg_r_package, "set_up_pkg", function(x) { called_setup <<- TRUE; list() })
  
  expect_null(assess_pkg_r_package("ghostpkg"))
  expect_false(called_modify)
  expect_false(called_setup)
})

test_that("successful end-to-end flow calls assess_pkg and returns its value", {
  tar_path <- fake_tar()
  mockery::stub(assess_pkg_r_package, "get_package_tarfile", function(...) tar_path)
  
  # keep description step simple
  mockery::stub(assess_pkg_r_package, "modify_description_file", function(x) x)
  
  # fake setup bundle
  pkg_src <- tempdir()
  mockery::stub(assess_pkg_r_package, "set_up_pkg", function(x) {
    list(
      build_vignettes   = FALSE,
      package_installed = FALSE,           
      pkg_source_path   = pkg_src,
      rcmdcheck_args    = list()           
    )
  })
  
  # install succeeds
  mockery::stub(assess_pkg_r_package, "install_package_local", function(path) TRUE)
  
  # capture args passed to assess_pkg
  captured <- new.env(parent = emptyenv())
  mockery::stub(assess_pkg_r_package, "assess_pkg", function(pkg_source_path, rcmdcheck_args) {
    captured$path <- pkg_source_path
    captured$args <- rcmdcheck_args
    "assessment-ok"
  })
  
  res <- assess_pkg_r_package("somepkg", version = "1.2.3")
  expect_equal(res, "assessment-ok")
  expect_equal(captured$path, pkg_src)
  expect_type(captured$args, "list")
  expect_true(!is.null(captured$args$path))
  expect_equal(captured$args$path, pkg_src)
})

test_that("install failure yields NULL and shows message", {
  tar_path <- fake_tar()
  mockery::stub(assess_pkg_r_package, "get_package_tarfile", function(...) tar_path)
  mockery::stub(assess_pkg_r_package, "modify_description_file", function(x) x)
  mockery::stub(assess_pkg_r_package, "set_up_pkg", function(x) {
    list(
      build_vignettes   = FALSE,
      package_installed = FALSE,
      pkg_source_path   = tempdir(),
      rcmdcheck_args    = list()
    )
  })
  mockery::stub(assess_pkg_r_package, "install_package_local", function(path) FALSE)
  
  # make sure assess_pkg is NOT called on failure
  called_assess <- FALSE
  mockery::stub(assess_pkg_r_package, "assess_pkg", function(...) { called_assess <<- TRUE; "should-not-be-called" })
  
  expect_message(
    res <- assess_pkg_r_package("badpkg"),
    "Package installation failed\\."
  )
  expect_null(res)
  expect_false(called_assess)
})

test_that("passes version and repos down to get_package_tarfile()", {
  seen <- new.env(parent = emptyenv())
  mockery::stub(assess_pkg_r_package, "get_package_tarfile", function(package_name, version = NA, repos = NULL) {
    seen$package_name <- package_name
    seen$version <- version
    seen$repos <- repos
    fake_tar()
  })
  mockery::stub(assess_pkg_r_package, "modify_description_file", identity)
  mockery::stub(assess_pkg_r_package, "set_up_pkg", function(x) {
    list(build_vignettes = FALSE, package_installed = FALSE, pkg_source_path = tempdir(), rcmdcheck_args = list())
  })
  mockery::stub(assess_pkg_r_package, "install_package_local", function(path) TRUE)
  mockery::stub(assess_pkg_r_package, "assess_pkg", function(...) "ok")
  
  custom_repos <- c(CRAN = "https://my.custom.repo")
  res <- assess_pkg_r_package("passdownpkg", version = "9.9.9", repos = custom_repos)
  expect_equal(res, "ok")
  expect_equal(seen$package_name, "passdownpkg")
  expect_equal(seen$version, "9.9.9")
  expect_equal(seen$repos, custom_repos)
})

test_that("cleans up tar file (unlink is still safe to call)", {
  tar_path <- fake_tar()
  expect_true(file.exists(tar_path))
  
  mockery::stub(assess_pkg_r_package, "get_package_tarfile", function(...) tar_path)
  mockery::stub(assess_pkg_r_package, "modify_description_file", identity)
  mockery::stub(assess_pkg_r_package, "set_up_pkg", function(x) {
    list(build_vignettes = FALSE, package_installed = FALSE, pkg_source_path = tempdir(), rcmdcheck_args = list())
  })
  mockery::stub(assess_pkg_r_package, "install_package_local", function(path) TRUE)
  mockery::stub(assess_pkg_r_package, "assess_pkg", function(...) "cleanup-ok")
  
  res <- assess_pkg_r_package("cleanme")
  expect_equal(res, "cleanup-ok")
  # give filesystem a tick; unlink happens synchronously, but be lenient
  expect_false(file.exists(tar_path))
})
