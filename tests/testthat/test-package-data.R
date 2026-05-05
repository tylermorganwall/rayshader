test_that("montereybay active binding returns a SpatRaster", {
	skip_if_not_installed("terra")

	expect_true(exists(
		"montereybay",
		envir = asNamespace("rayshader"),
		inherits = FALSE
	))
	expect_s4_class(rayshader::montereybay, "SpatRaster")
})

test_that("montereybay is read-only", {
	ns = asNamespace("rayshader")
	was_locked = bindingIsLocked("montereybay", ns)
	if (was_locked) {
		unlockBinding("montereybay", ns)
	}
	withr::defer({
		if (was_locked && !bindingIsLocked("montereybay", ns)) {
			lockBinding("montereybay", ns)
		}
	})

	expect_error(
		assign("montereybay", NULL, envir = ns),
		"read-only package data"
	)
})

test_that(".montereybay_packed remains internal", {
	expect_false(".montereybay_packed" %in% getNamespaceExports("rayshader"))
})
