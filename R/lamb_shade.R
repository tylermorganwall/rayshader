#'@title Calculate Lambert Shading Map
#'
#'@description Calculates local shadow map for a elevation matrix by calculating the dot
#'product between light direction and the surface normal vector at that point. Each point's
#'intensity is proportional to the cosine of the normal vector.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param sunaltitude Default `45`. The azimuth angle as measured from the horizon from which the light originates.
#'@param sunangle Default `315` (NW). The angle around the matrix from which the light originates.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis.
#'@param vertical_exaggeration Default `1`. One-off multiplier applied to the
#'effective visual relief for this call. Values greater than `1` increase
#'apparent relief and values between `0` and `1` flatten it. This does not
#'update cached `zscale` metadata.
#'@param zero_negative Default `TRUE`. Zeros out all values below 0 (corresponding to surfaces facing away from the light source).
#'@return Matrix of light intensities at each point.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Generate a basic hillshade
#'montereybay |>
#'  lamb_shade() |>
#'  plot_map()
#'
#'#Increase the intensity by decreasing the vertical exaggeration
#'montereybay |>
#'  lamb_shade(vertical_exaggeration = 4) |>
#'  plot_map()
#'
#'#Change the sun direction
#'montereybay |>
#'  lamb_shade(sunangle=45, vertical_exaggeration = 4) |>
#'  plot_map()
#'
#'#Change the sun altitude
#'montereybay |>
#'  lamb_shade(sunaltitude=60, vertical_exaggeration = 4) |>
#'  plot_map()
#'
#'#Change the sun to directly overhead, the shading here represents the slope angle
#'montereybay |>
#'  lamb_shade(sunaltitude=90, vertical_exaggeration = 8) |>
#'  plot_map()
lamb_shade = function(
  heightmap,
  sunaltitude = 45,
  sunangle = 315,
  zscale = 1,
  vertical_exaggeration = 1,
  zero_negative = TRUE
) {
  heightmap_missing = missing(heightmap)
  heightmap_cache_label = format_scene_cache_label(deparse(substitute(
    heightmap
  )))
  zscale_cache_input_label = format_scene_cache_label(deparse(substitute(
    zscale
  )))
  heightmap_auto_zscale = NA_real_
  if (heightmap_missing) {
    resolved_heightmap = resolve_hillshade_heightmap(
      heightmap_missing = TRUE,
      caller = "lamb_shade"
    )
    heightmap = resolved_heightmap$heightmap
    allow_scene_zscale_cache = identical(resolved_heightmap$source, "scene")
  } else {
    heightmap_info = coerce_plot_3d_heightmap(heightmap)
    heightmap = heightmap_info$heightmap
    heightmap_auto_zscale = heightmap_info$zscale
    cache_hillshade_heightmap(heightmap, label = heightmap_cache_label)
    allow_scene_zscale_cache = FALSE
  }
  if (!is.matrix(heightmap)) {
    stop("`heightmap` must be a matrix.", call. = FALSE)
  }
  resolved_zscale = resolve_hillshade_zscale(
    zscale = zscale,
    zscale_missing = missing(zscale),
    caller = "lamb_shade",
    auto_zscale = heightmap_auto_zscale,
    allow_hillshade_cache = heightmap_missing,
    allow_scene_cache = allow_scene_zscale_cache
  )
  zscale = resolved_zscale$zscale
  zscale_cache_label = switch(
    resolved_zscale$source,
    explicit = zscale_cache_input_label,
    auto = format_scene_cache_label(sprintf(
      "%s_auto_zscale",
      heightmap_cache_label
    )),
    hillshade = resolved_zscale$label,
    scene = resolved_zscale$label,
    NULL
  )
  cache_hillshade_zscale(zscale, label = zscale_cache_label)
  zscale = apply_vertical_exaggeration(
    zscale = zscale,
    vertical_exaggeration = vertical_exaggeration,
    caller = "lamb_shade"
  )
  sunang_rad = sunangle * pi / 180
  rayang_rad = sunaltitude * pi / 180
  rayvector = c(
    cos(sunang_rad) * cos(rayang_rad),
    sin(sunang_rad) * cos(rayang_rad),
    -sin(rayang_rad)
  )
  heightmap = add_padding(heightmap)
  heightmap = heightmap / zscale
  shadowmatrix = lambshade_cpp(heightmap = heightmap, rayvector = rayvector)
  shadowmatrix = scales::rescale_max(shadowmatrix, c(0, 1))
  if (zero_negative) {
    shadowmatrix[shadowmatrix < 0] = 0
  }
  shadowmatrixremove = shadowmatrix[
    c(-1, -nrow(shadowmatrix)),
    c(-1, -ncol(shadowmatrix))
  ]
  shadowmatrixremove[is.na(shadowmatrixremove)] = 0
  return(t(shadowmatrixremove))
}
