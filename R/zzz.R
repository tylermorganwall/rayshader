ray_has_tex_envir = new.env(parent = emptyenv())
ray_has_norm_envir = new.env(parent = emptyenv())
ray_cache_scene_envir = new.env(parent = emptyenv())
ray_surface_texture_envir = new.env(parent = emptyenv())
ray_road_path_envir = new.env(parent = emptyenv())

assign("scene_cache", NULL, envir = ray_cache_scene_envir)
assign("scene_context_token", NULL, envir = ray_cache_scene_envir)

.onLoad = function(libname, pkgname) {
  ns = asNamespace(pkgname)
  package_data = new.env(parent = emptyenv())
  utils::data(
    "montereybay",
    package = pkgname,
    lib.loc = libname,
    envir = package_data
  )
  if (!exists("montereybay", envir = package_data, inherits = FALSE)) {
    stop("Could not load the `montereybay` package data.", call. = FALSE)
  }
  montereybay_matrix = get(
    "montereybay",
    envir = package_data,
    inherits = FALSE
  )
  if (!exists("montereybay", envir = ns, inherits = FALSE)) {
    assign("montereybay", montereybay_matrix, envir = ns)
  }
  montereybay_extent = get_extent(attr(
    montereybay_matrix,
    "extent",
    exact = TRUE
  ))
  montereybay_crs = attr(montereybay_matrix, "crs", exact = TRUE)
  spatial_raster = terra::rast(
    nrows = ncol(montereybay_matrix),
    ncols = nrow(montereybay_matrix),
    xmin = montereybay_extent["xmin"],
    xmax = montereybay_extent["xmax"],
    ymin = montereybay_extent["ymin"],
    ymax = montereybay_extent["ymax"],
    crs = montereybay_crs
  )
  terra::values(spatial_raster) = as.vector(montereybay_matrix)
  names(spatial_raster) = "Band1"
  assign("montereybay_spatial", spatial_raster, envir = ns)
  invisible(NULL)
}
