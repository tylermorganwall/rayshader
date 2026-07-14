ray_has_tex_envir = new.env(parent = emptyenv())
ray_has_norm_envir = new.env(parent = emptyenv())
ray_cache_scene_envir = new.env(parent = emptyenv())
ray_surface_texture_envir = new.env(parent = emptyenv())
ray_road_path_envir = new.env(parent = emptyenv())

assign("scene_cache", NULL, envir = ray_cache_scene_envir)
assign("scene_context_token", NULL, envir = ray_cache_scene_envir)

register_montereybay_active_binding = function(pkgname = "rayshader") {
  ns = asNamespace(pkgname)

  if (exists("montereybay", envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }

  makeActiveBinding(
    "montereybay",
    function(value) {
      if (!missing(value)) {
        stop("`montereybay` is read-only package data.", call. = FALSE)
      }

      if (!exists(".montereybay_packed", envir = ns, inherits = FALSE)) {
        stop(
          "Internal object `.montereybay_packed` is missing from `R/sysdata.rda`.",
          call. = FALSE
        )
      }

      terra::unwrap(get(".montereybay_packed", envir = ns, inherits = FALSE))
    },
    ns
  )

  invisible(TRUE)
}

.onLoad = function(libname, pkgname) {
  register_montereybay_active_binding(pkgname)
}
