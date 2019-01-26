get_stage("before_script") %>%
  add_code_step(system('sudo apt-get gdal')) %>%
  add_code_step(system('export DISPLAY=:99.0')) %>%
  add_code_step(system('sh -e /etc/init.d/xvfb start')) %>%
  add_code_step(system("sleep 3"))

add_package_checks()

if (Sys.getenv("id_rsa") != "" && !ci()$is_tag()) {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages"))
}