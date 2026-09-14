test_that("cache_summary_internal() honors pkg.package_cache_dir", {
  skip_on_cran()
  custom <- test_temp_dir()
  withr::local_options(list(pkg.package_cache_dir = custom))

  ret <- cache_summary_internal()
  expect_equal(ret$cachepath, custom)
  expect_equal(ret$files, 0L)
})

test_that("cache_list_internal() honors pkg.package_cache_dir", {
  skip_on_cran()
  custom <- test_temp_dir()
  withr::local_options(list(pkg.package_cache_dir = custom))

  pkgcache::pkg_cache_add_file(
    cachepath = custom,
    file = test_temp_file(),
    package = "foo",
    platform = "source",
    rversion = "*"
  )

  ret <- cache_list_internal(package = "foo")
  expect_equal(nrow(ret), 1L)
})

test_that("cache_delete_internal() honors pkg.package_cache_dir", {
  skip_on_cran()
  custom <- test_temp_dir()
  withr::local_options(list(pkg.package_cache_dir = custom))

  pkgcache::pkg_cache_add_file(
    cachepath = custom,
    file = test_temp_file(),
    package = "foo",
    platform = "source",
    rversion = "*"
  )
  expect_equal(nrow(cache_list_internal(package = "foo")), 1L)

  cache_delete_internal(package = "foo")
  expect_equal(nrow(cache_list_internal(package = "foo")), 0L)
})

test_that("meta_update_internal() and meta_list_internal() honor pkg.metadata_cache_dir", {
  skip_if_offline()
  skip_on_cran()
  custom <- test_temp_dir()
  withr::local_options(list(
    pkg.metadata_cache_dir = custom,
    repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")
  ))

  meta_update_internal()
  # The replica cache lives directly under the configured directory, not a
  # session tempdir, so pkg_install() (which sets up its own cache with the
  # same replica_path, see pkg-plan.R) sees the same, already-warm data.
  expect_true(length(dir(file.path(custom, "_metadata"), recursive = TRUE)) > 0)

  ret <- meta_list_internal("cli")
  expect_true("cli" %in% ret$package)
})
