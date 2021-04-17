# Contributing

When contributing to this repository, please first discuss the change you wish to make via opening 
an email with your requested change. 

Please note the code of conduct, please follow it in all your interactions with the project.

## Bug Reports

1. First, search the closed and open issues to see if your issue has been answered before.
2. Update your package version to the latest available, using the remotes package to install via github:
`remotes::install_github("tylermorganwall/{package name}")`. If this does not fix your issue, proceed to the next step.
3. Open an GH issue, including a minimally reproducible example (with code) that does not require downloading
external data (unless the data is included in an R package, or can be easily requested via one, e.g. the
elevatr package). I recommend using the reprex package.  Be sure to include your system information via the `Sys.info()` function.

## Pull Request Process

1. Be sure to have opened an issue describing your intended changes first. 
2. Fork the repo and make your changes.
3. Run the included tests using testthat, ensuring there are no regressions/bugs introduced from previous versions.
The tests check consistency with previous renders: if your contributions result in intended changes to the rendering
behavior (via changes the random number generation process, lighting integrator, surface materials, objects, etc), document
the changes and each failing test in your pull request. Further instructions are at the bottom of each testthat file that
describe how to regenerate new standard checksums that incorporate the requested changes.
4. Ensure the package passes an R CMD CHECK with no warnings or errors, preferably with `rhub::check_for_cran()`. If making changes
to the compiled code, please also use `rhub::check_on_solaris()`, `rhub::check_on_macos()`, and `rhub::check_on_windows()` (or upload the source packages to Win-Builder).