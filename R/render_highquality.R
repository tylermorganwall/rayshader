#'@title Render High Quality
#'
#'@description Renders a raytraced version of the displayed rgl scene, using the `rayrender` package.
#'User can specify the light direction, intensity, and color, as well as specify the material of the
#'ground and add additional scene elements.
#'
#'This function can also generate frames for an animation by passing camera animation information from
#'either [convert_path_to_animation_coords()] or `rayrender::generate_camera_motion()` functions.
#'
#'@param filename Default `NA`. Filename of saved image. If missing, will display to current device.
#'@param samples Default `128`. The maximum number of samples for each pixel. Increase this to increase the quality of the rendering.
#'@param sample_method Default `"sobol_blue"`, unless `samples > 256`, in which it defaults to `"sobol"`.
#'The type of sampling method used to generate random numbers.
#'The other options are `random` (worst quality but fastest),
#'`sobol_blue` (best option for sample counts below 256), and `sobol`
#'(slowest but best quality, better than `sobol_blue` for sample counts greater than 256).
#'@param min_variance Default `1e-6`. Minimum acceptable variance for a block of pixels for the adaptive sampler.
#'Smaller numbers give higher quality images, at the expense of longer rendering times.
#'If this is set to zero, the adaptive sampler will be turned off and the renderer will use the maximum number of samples everywhere.
#'@param light Default `TRUE`. Whether there should be a light in the scene. If not, the scene will be lit with a bluish sky.
#'@param lat Default `NA`. Latitude (degrees) for automatic sky generation.
#'When `lat`, `long`, `datetime`, or `sky_args` are supplied, `render_highquality()` uses
#'`skymodelr::generate_sky_latlong()` to create an EXR environment map, disables the default light,
#'and sets `environment_light` for the render unless direct sky arguments are provided.
#'If `datetime` is supplied and `lat`/`long` are omitted, rayshader will try to
#'derive them from the center of the cached scene extent, transformed to WGS84.
#'@param long Default `NA`. Longitude (degrees; west < 0) for automatic sky generation.
#'@param datetime Default `NA`. POSIXct or character date-time used to position the sun.
#'If `lat`/`long` are omitted, rayshader will try to derive them from the cached
#'scene extent center (converted to latitude/longitude) before generating the sky.
#'@param sky_sun_elevation Default `NA`. If supplied, uses `skymodelr::generate_sky()` and
#'passes this value to its `elevation` argument.
#'@param sky_sun_azimuth Default `NA`. If supplied, uses `skymodelr::generate_sky()` and
#'passes this value to its `azimuth` argument.
#'@param sky_altitude Default `NA`. If supplied, uses `skymodelr::generate_sky()` and
#'passes this value to its `altitude` argument.
#'@param sky_args Default empty `list()`. Additional arguments passed to
#'`skymodelr::generate_sky_latlong()` (default) or `skymodelr::generate_sky()`
#'when direct sun arguments are used (except `filename`, which is managed internally).
#'The EXR is cached in `tempdir()` using a filename derived from these inputs.
#'@param lightdirection Default `315`. Position of the light angle around the scene.
#'If this is a vector longer than one, multiple lights will be generated (using values from
#'`lightaltitude`, `lightintensity`, and `lightcolor`)
#'@param lightaltitude Default `45`. Angle above the horizon that the light is located.
#'If this is a vector longer than one, multiple lights will be generated (using values from
#'`lightdirection`, `lightintensity`, and `lightcolor`)
#'@param lightsize Default `NULL`. Radius of the light(s). Automatically chosen, but can be set here by the user.
#'@param lightintensity Default `500`. Intensity of the light.
#'@param lightcolor Default `white`. The color of the light.
#'@param water_attenuation Default `0`, no attenuation. Amount that light should be attenuated when traveling through water. This
#'calculates 1-color
#'@param water_surface_color Default `TRUE`. Whether the water should have a colored surface or not. This is in contrast to
#' setting a non-zero water attenuation, where the color comes from the attenuation of light in the water itself.
#'@param water_ior Default `1.33`. Water index of refraction.
#'@param water_material Default `"glassy"`. Water material used in
#'`render_highquality()`. `"glassy"` renders water with
#'`rayrender::dielectric()`; `"microfacet"` renders water with
#'`rayrender::microfacet(transmission = TRUE)`.
#'@param water_roughness Default `0.1`. Roughness used when `water_material = "microfacet"`.
#'@param material Default `rayrender::diffuse()`. The material properties of the object file. Only used if `override_material = TRUE`
#'@param override_material Default `FALSE`. Whether to override the default diffuse material with that in argument `material`.
#'@param cache_scene Default `FALSE`. Whether to cache the current scene to memory so it does not have to be converted to a `raymesh` object
#'each time [render_snapshot()] is called. If `TRUE` and a scene has been cached, it will be used when rendering.
#'@param reset_scene_cache Default `FALSE`. Resets the scene cache before rendering.
#'@param width Defaults to the width of the rgl window. Width of the rendering.
#'@param height Defaults to the height of the rgl window. Height of the rendering.
#'@param ortho_dimensions Default `NULL`, which uses the orthographic dimensions
#'inferred from the current rgl projection when `fov = 0`. Supply a numeric
#'length-2 vector to override the inferred orthographic width/height.
#'@param text_angle Default `NULL`, which forces the text always to face the camera. If a single angle (degrees),
#'will specify the absolute angle all the labels are facing. If three angles, this will specify all three orientations
#'(relative to the x,y, and z axes) of the text labels.
#'@param text_size Default `16`. Base height of the text.
#'@param text_offset Default `c(0,text_size/2,0)`. Offset to be applied to all world-space
#'text labels. For screen-space text, no default offset is applied unless this is
#'explicitly supplied.
#'@param text_render Default `"screen"`. Whether labels and z-axis text are rendered as screen-space
#'annotations (`"screen"`) or as 3D text geometry (`"world"`).
#'@param text_occlusion Default `TRUE`. Whether screen-space text should be hidden when occluded by scene geometry.
#'Only used when `text_render = "screen"`.
#'@param text_occlusion_mode Default `"partial"`. Occlusion mode passed to `rayrender::screen_text()`.
#'Options are `"anchor"`, which hides text when the anchor point is occluded, `"label"`, which
#'checks the text label area, and `"partial"`, an alias for `"label"`.
#'@param text_occlusion_tolerance Default `0.001`. Occlusion tolerance passed to `rayrender::screen_text()`.
#'@param screen_text_args Default empty `list()`. Additional named arguments passed to
#'`rayrender::screen_text()` for screen-space labels. Arguments in `screen_text_args`
#'override values generated by rayshader when names overlap.
#'@param line_radius Default `0.5`. Radius of line/path segments.
#'@param line_render Default `"auto"`. Whether rgl line objects are rendered as screen-space annotations
#'or 3D geometry. `"auto"` renders z-axis lines/ticks and [render_label()] connector lines in screen
#'space while keeping paths and contours as 3D geometry. Use `"screen"` for all line/path objects or
#'`"world"` for the previous 3D behavior.
#'@param line_occlusion Default `TRUE`. Whether screen-space lines should be hidden when occluded by scene geometry.
#'Only used for lines rendered in screen space.
#'@param line_occlusion_mode Default `"partial"`. Occlusion mode passed to `rayrender::screen_line()`.
#'Options are `"anchor"`, which hides lines when the anchor point is occluded, `"line"`, which
#'checks the line extent, and `"partial"`, an alias for `"line"`.
#'@param line_occlusion_tolerance Default `0.001`. Occlusion tolerance passed to `rayrender::screen_line()`.
#'@param screen_line_args Default empty `list()`. Additional named arguments passed to
#'`rayrender::screen_line()` for screen-space lines. Arguments in `screen_line_args`
#'override values generated by rayshader when names overlap.
#'@param smooth_line Default `FALSE`. If `TRUE`, the line will be rendered with a continuous smooth line, rather
#'than straight segments.
#'@param use_extruded_paths Default `TRUE`. If `FALSE`, paths will be generated with the `rayrender::path()` object, instead
#'of `rayrender::extruded_path()`.
#' @param joined_stream_mesh Default `FALSE`. If `TRUE`, stream paths from
#'[render_streams()] are rendered as joined terrain-clipped water meshes in
#'[render_highquality()] instead of one rectangular extrusion per line.
#'@param point_radius Default `1`. Radius of 3D points (rendered with [render_points()]). This scales the existing
#'value of size specified in [render_points()].
#'@param scale_text_angle Default `NULL`. Same as `text_angle`, but for the scale bar.
#'@param scale_text_size Default `16`. Base height of the scale bar text.
#'@param scale_text_offset Default `c(0,scale_text_size/2,0)`. Offset to be applied to all
#'world-space scale bar text labels. For screen-space text, no default offset is
#'applied unless this is explicitly supplied.
#'@param title_text Default `NULL`. Text. Adds a title to the image, using magick::image_annotate.
#'@param title_offset Default `c(20,20)`. Distance from the top-left (default, `gravity` direction in
#'image_annotate) corner to offset the title.
#'@param title_size Default `30`. Font size in pixels.
#'@param title_color Default `black`. Font color.
#'@param title_font Default `sans`. String with font family such as "sans", "mono", "serif", "Times", "Helvetica",
#'"Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#'@param title_just Default `left`. Justification of the title.
#'@param title_bar_color Default `NA`. If a color, this will create a colored bar under the title.
#'@param title_bar_alpha Default `0.5`. Transparency of the title bar.
#'@param ground_material Default `diffuse()`. Material defined by the rayrender material functions.
#'@param ground_size Default `100000`. The width of the plane representing the ground.
#'@param camera_location Default `NULL`. Custom position of the camera. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window. By default, numeric length-3 vectors are interpreted as existing render_highquality scene-camera coordinates. To use spatial coordinates, supply a named vector such as `c(long = ..., lat = ..., altitude = ...)`, an `sf`/`sfc`/`sfg`/`sp` POINT with altitude stored in a Z coordinate or `altitude` column, or a list such as `list(location = point, altitude = ..., crs = ...)`.
#'@param camera_lookat Default `NULL`. Custom point at which the camera is directed. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window. Spatial inputs are accepted using the same formats as `camera_location`.
#'@param camera_interpolate Default `c(0,0)`. Maximum `1`, minimum `0`. Sets the camera at a point between the `rgl` view and the `camera_location`
#'and `camera_lookat` vectors.
#'@param scene_elements Default `NULL`. Extra scene elements to add to the scene, created with rayrender.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param print_scene_info Default `FALSE`. If `TRUE`, it will print the position and lookat point of the camera.
#'@param clamp_value Default `1000`. Radiance clamp value. If `NA`, uses `1000`.
#'@param return_scene Default `FALSE`. If `TRUE`, this will return the rayrender scene (instead of rendering the image).
#'@param load_normals Default `TRUE`. Whether to load the vertex normals if they exist in the OBJ file.
#'@param calculate_consistent_normals Default `FALSE`. Whether to calculate consistent vertex normals to prevent energy
#'loss at edges.
#'@param rgl_materials Default empty `list()`. Named list of material overrides for rgl objects.
#'Names must be rgl tags or rgl ids. Values can be evaluated
#'rayrender materials, rayrender material functions, or lists with a `material` entry and optional
#'`args`/`material_args` entry. When a material function is supplied, the rgl object color is passed
#'to its `color` argument unless `color` is already supplied in `args`.
#'@param animation_camera_coords Default `NULL`. Expects camera animation output from either [convert_path_to_animation_coords()]
#'or `rayrender::generate_camera_motion()` functions.
#'@param plot Default `is.na(filename)`. Whether to plot the scene, or just return the RGBA array.
#'@param ... Additional parameters to pass to `rayrender::render_scene`()
#'
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Render the volcano dataset using pathtracing
#'volcano |>
#'  sphere_shade(vertical_exaggeration = 2) |>
#'  plot_3d(vertical_exaggeration = 1/2, shadowdepth=min(volcano)*0.8)
#'render_highquality(min_variance = 0, sample_method = "sobol_blue", samples = 16)
#'
#'#Change position of light
#'render_highquality(lightdirection = 45, min_variance = 0,
#'                   sample_method = "sobol_blue", samples = 16)
#'
#'#Change vertical position of light
#'render_highquality(lightdirection = 45, lightaltitude = 10,
#'                   min_variance = 0, samples = 16)
#'
#'#Change the ground material
#'render_highquality(lightdirection = 45, lightaltitude=60,
#'                   ground_material = rayrender::diffuse(checkerperiod = 30, checkercolor="grey50"),
#'                   min_variance = 0, samples = 16)
#'
#'#Add three different color lights and a title
#'render_highquality(lightdirection = c(0,120,240), lightaltitude=45,
#'                   lightcolor=c("red","green","blue"), title_text = "Red, Green, Blue",
#'                   title_bar_color="white", title_bar_alpha=0.8,
#'                   min_variance = 0, samples = 16)
#'
#'#Change the camera:
#'render_camera(theta=-45,phi=60,fov=60,zoom=0.8)
#'render_highquality(lightdirection = c(0),
#'                   title_bar_color="white", title_bar_alpha=0.8,
#'                   min_variance = 0, samples = 16)
#'#Add a shiny metal sphere
#'render_camera(theta=-45,phi=60,fov=60,zoom=0.8)
#'render_highquality(lightdirection = c(0,120,240), lightaltitude=45,
#'                   lightcolor=c("red","green","blue"),
#'                   scene_elements = rayrender::sphere(z=-60,y=0,
#'                                                      radius=20,material=rayrender::metal()),
#'                   min_variance = 0, samples = 16)
#'
#'#Add a red light to the volcano and change the ambient light to dusk
#'render_camera(theta=45,phi=45)
#'render_highquality(lightdirection = c(240), lightaltitude=30,
#'                   lightcolor=c("#5555ff"),
#'                   scene_elements = rayrender::sphere(z=0,y=6, x=-18, radius=5,
#'                                    material=rayrender::light(color="red",intensity=100)),
#'                   min_variance = 0, samples = 16)
#'#Manually change the camera location and direction
#'render_camera(fov=111)
#'render_highquality(lightdirection = c(240), lightaltitude=30, lightcolor=c("#5555ff"),
#'                   camera_location = c(-8.91, 24.36, 6.96), camera_lookat = c(4.25, 20.86, 2.18),
#'                   scene_elements = rayrender::sphere(z=0,y=15, x=-18, radius=5,
#'                                    material=rayrender::light(color="red",intensity=10)),
#'                   min_variance = 0, samples = 16)
#'# Render the shadow of the Washington Monument with a realistic sky at that datetime
#'# using the `skymodelr` package.
#'@examplesIf length(find.package("sf", quiet = TRUE)) && length(find.package("elevatr", quiet = TRUE)) && length(find.package("raster", quiet = TRUE)) && (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#'library(sf)
#'#Set location of washington monument
#'washington_monument_location =  st_point(c(-77.035249, 38.889462))
#'wm_point = washington_monument_location |>
#'  st_buffer(0.01) |>
#'  st_sfc(crs = 4326) |>
#'  st_transform(st_crs(washington_monument_multipolygonz))
#'
#'elevation_data = elevatr::get_elev_raster(locations = wm_point, z = 14)
#'
#'wash_point = washington_monument_location |>
#'  st_sfc(crs = 4326) |>
#'  st_transform(st_crs(washington_monument_multipolygonz))
#'scene_bbox = st_bbox(st_buffer(wash_point,500))
#'cropped_data = raster::crop(elevation_data, scene_bbox)
#'
#'#Plot a 3D map of the national mall
#'cropped_data |>
#'  sphere_shade(vertical_exaggeration = 30) |>
#'  plot_3d(water = TRUE, waterdepth = 1.5,
#'          soliddepth=-50, windowsize = 800)
#'#Zoom in on the monument
#'render_camera(theta=115,  phi=25, zoom= 0.25, fov=40)
#'#Render the national monument at solar noon on the solstice
#'rgl::par3d(ignoreExtent = TRUE)
#'render_multipolygonz(washington_monument_multipolygonz,
#'                     zscale = 4, color = "grey80")
#' #Render using the built-in (but less accurate) Hosek model.
#' # Here's it's more yellow than it should be, but it's accurate enough for most renders.
#' # We manually set the exposure (via iso) down to capture the softer morning light.
#'render_highquality(
#'	min_variance = 0,
#'	samples = 16,
#'	long = -77.035249,
#'	lat = 38.889462,
#'	iso = 8,
#'	clamp_value = 1000,
#'	datetime = as.POSIXct("2025-12-21 08:00:00", tz = "EST")
#')
#'
#'# Render the more-accurate Prague model (that requires
#'# supplemental data that will be downloaded on the first call) and
#'# specify a higher resolution environment map via `sky_args`
#'# We use auto_exposure to account for the high mid-day radiance.
#'render_highquality(
#'	min_variance = 0,
#'	samples = 16,
#'	long = -77.035249,
#'	lat = 38.889462,
#'	sky_args = list(hosek = FALSE,resolution=4000),
#'	auto_exposure = TRUE,
#'	clamp_value = 1000,
#'	datetime = as.POSIXct("2025-12-21 11:00:00", tz = "EST")
#')
render_highquality = function(
  filename = NA,
  samples = 128,
  sample_method = "sobol_blue",
  min_variance = 1e-7,
  light = TRUE,
  lat = NA,
  long = NA,
  datetime = NA,
  sky_sun_elevation = NA,
  sky_sun_azimuth = NA,
  sky_altitude = NA,
  sky_args = list(),
  lightdirection = 315,
  lightaltitude = 45,
  lightsize = NULL,
  lightintensity = 500,
  lightcolor = "white",
  material = rayrender::diffuse(),
  water_attenuation = 0,
  water_surface_color = TRUE,
  water_ior = 1.33,
  water_material = c("glassy", "microfacet"),
  water_roughness = 0.1,
  override_material = FALSE,
  cache_scene = FALSE,
  reset_scene_cache = FALSE,
  width = NULL,
  height = NULL,
  ortho_dimensions = NULL,
  text_angle = NULL,
  text_size = 16,
  text_offset = c(0, text_size / 2, 0),
  text_render = c("screen", "world"),
  text_occlusion = TRUE,
  text_occlusion_mode = "partial",
  text_occlusion_tolerance = 0.001,
  screen_text_args = list(),
  line_radius = 0.5,
  line_render = c("auto", "screen", "world"),
  line_occlusion = TRUE,
  line_occlusion_mode = "partial",
  line_occlusion_tolerance = 0.001,
  screen_line_args = list(),
  point_radius = 0.5,
  smooth_line = FALSE,
  use_extruded_paths = FALSE,
  joined_stream_mesh = FALSE,
  scale_text_angle = NULL,
  scale_text_size = 16,
  scale_text_offset = c(0, scale_text_size / 2, 0),
  title_text = NULL,
  title_offset = c(20, 20),
  title_color = "black",
  title_size = 30,
  title_font = "sans",
  title_just = "left",
  title_bar_color = NA,
  title_bar_alpha = 0.5,
  ground_material = rayrender::diffuse(),
  ground_size = 100000,
  scene_elements = NULL,
  camera_location = NULL,
  camera_lookat = NULL,
  camera_interpolate = 1,
  clear = FALSE,
  return_scene = FALSE,
  print_scene_info = FALSE,
  clamp_value = 1000,
  calculate_consistent_normals = FALSE,
  load_normals = TRUE,
  rgl_materials = list(),
  animation_camera_coords = NULL,
  plot = is.na(filename),
  ...
) {
  text_offset_missing = missing(text_offset)
  scale_text_offset_missing = missing(scale_text_offset)
  water_material = match.arg(water_material)
  water_roughness = validate_render_highquality_water_roughness(
    water_roughness
  )
  text_render = match.arg(text_render)
  line_render = match.arg(line_render)
  joined_stream_mesh = suppressWarnings(as.logical(joined_stream_mesh))
  text_occlusion = suppressWarnings(as.logical(text_occlusion))
  line_occlusion = suppressWarnings(as.logical(line_occlusion))
  if (!length(joined_stream_mesh) || is.na(joined_stream_mesh[1])) {
    stop("`joined_stream_mesh` must be TRUE or FALSE.")
  }
  joined_stream_mesh = joined_stream_mesh[1]
  if (!length(text_occlusion) || is.na(text_occlusion[1])) {
    stop("`text_occlusion` must be TRUE or FALSE.")
  }
  if (!length(line_occlusion) || is.na(line_occlusion[1])) {
    stop("`line_occlusion` must be TRUE or FALSE.")
  }
  text_occlusion = text_occlusion[1]
  line_occlusion = line_occlusion[1]
  text_occlusion_mode = normalize_render_highquality_occlusion_mode(
    text_occlusion_mode,
    type = "text"
  )
  line_occlusion_mode = normalize_render_highquality_occlusion_mode(
    line_occlusion_mode,
    type = "line"
  )
  text_occlusion_tolerance = as.numeric(text_occlusion_tolerance)[1]
  line_occlusion_tolerance = as.numeric(line_occlusion_tolerance)[1]
  if (!is.finite(text_occlusion_tolerance) || text_occlusion_tolerance < 0) {
    stop("`text_occlusion_tolerance` must be a non-negative number.")
  }
  if (!is.finite(line_occlusion_tolerance) || line_occlusion_tolerance < 0) {
    stop("`line_occlusion_tolerance` must be a non-negative number.")
  }
  screen_text_args = validate_render_highquality_screen_args(
    screen_text_args,
    arg_name = "screen_text_args"
  )
  screen_line_args = validate_render_highquality_screen_args(
    screen_line_args,
    arg_name = "screen_line_args"
  )
  screen_text_offset = if (text_offset_missing) c(0, 0, 0) else text_offset
  screen_scale_text_offset = if (scale_text_offset_missing) {
    c(0, 0, 0)
  } else {
    scale_text_offset
  }
  ortho_dimensions_override = ortho_dimensions
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  if (samples > 256 && sample_method == "sobol_blue") {
    warning(
      r"{When `sample_method = "sobol_blue"`, `samples` must be less than or equal to 256. Setting `sample_method` to `"sobol"`.}"
    )
    sample_method = "sobol"
  }
  if (reset_scene_cache) {
    reset_scene_context(
      clear_scene_metadata = FALSE,
      clear_scene_cache = TRUE
    )
  }
  if (!is.na(filename)) {
    if (dirname(filename) != ".") {
      if (!dir.exists(dirname(filename))) {
        stop(sprintf(
          "Error: directory '%s' does not exist.",
          dirname(filename)
        ))
      }
    }
  }
  if (!(length(find.package("rayrender", quiet = TRUE)) > 0)) {
    stop("`rayrender` package required for render_highquality()")
  }
  validate_render_highquality_screen_arg_names(
    screen_text_args,
    rayrender::screen_text,
    arg_name = "screen_text_args"
  )
  validate_render_highquality_screen_arg_names(
    screen_line_args,
    rayrender::screen_line,
    arg_name = "screen_line_args"
  )
  dot_args = list(...)
  render_scene_formals = formals(rayrender::render_scene)
  render_scene_supports_environment_light_bake_white =
    "environment_light_bake_white" %in% names(render_scene_formals)
  if (is.na(clamp_value)) {
    clamp_value = 1000
  }
  removed_material_args = intersect(
    names(dot_args),
    c(
      "point_material",
      "point_material_args",
      "path_material",
      "path_material_args"
    )
  )
  if (length(removed_material_args) > 0) {
    stop(
      sprintf(
        "`%s` %s not a render_highquality() argument. Use `rgl_materials` instead.",
        paste(removed_material_args, collapse = "`, `"),
        ifelse(length(removed_material_args) == 1, "is", "are")
      ),
      call. = FALSE
    )
  }
  if (is.null(sky_args)) {
    sky_args = list()
  }
  if (!is.list(sky_args)) {
    stop("`sky_args` must be a list.")
  }

  use_sky = FALSE
  has_direct_sky = FALSE
  if (!is.null(lat) && !is.na(lat)) {
    if (length(lat) != 1) {
      stop("`lat` must be length 1.")
    }
    use_sky = TRUE
  }
  if (!is.null(long) && !is.na(long)) {
    if (length(long) != 1) {
      stop("`long` must be length 1.")
    }
    use_sky = TRUE
  }
  if (!is.null(datetime) && !is.na(datetime)) {
    if (length(datetime) != 1) {
      stop("`datetime` must be length 1.")
    }
    use_sky = TRUE
  }
  if (!is.null(sky_sun_elevation) && !is.na(sky_sun_elevation)) {
    if (length(sky_sun_elevation) != 1) {
      stop("`sky_sun_elevation` must be length 1.")
    }
    use_sky = TRUE
    has_direct_sky = TRUE
  }
  if (!is.null(sky_sun_azimuth) && !is.na(sky_sun_azimuth)) {
    if (length(sky_sun_azimuth) != 1) {
      stop("`sky_sun_azimuth` must be length 1.")
    }
    use_sky = TRUE
    has_direct_sky = TRUE
  }
  if (!is.null(sky_altitude) && !is.na(sky_altitude)) {
    if (length(sky_altitude) != 1) {
      stop("`sky_altitude` must be length 1.")
    }
    use_sky = TRUE
    has_direct_sky = TRUE
  }
  if (length(sky_args) > 0) {
    use_sky = TRUE
  }
  sky_arg_names_raw = names(sky_args)
  if (
    !is.null(sky_arg_names_raw) &&
      any(c("elevation", "azimuth") %in% sky_arg_names_raw)
  ) {
    has_direct_sky = TRUE
  }
  if (
    has_direct_sky &&
      ((!is.null(lat) && !is.na(lat)) ||
        (!is.null(long) && !is.na(long)) ||
        (!is.null(datetime) && !is.na(datetime)))
  ) {
    warning(
      "Both lat/long/datetime and direct sun inputs detected. Using `skymodelr::generate_sky()` inputs."
    )
  }
  sky_mode = ifelse(has_direct_sky, "direct", "latlong")

  sky_file = NULL
  sky_environment_light_bake_white = FALSE
  if (use_sky) {
    if (!requireNamespace("skymodelr", quietly = TRUE)) {
      stop(
        "`skymodelr` package required for automatic sky generation. ",
        "Install it or provide `environment_light` via `...`."
      )
    }
    if ("environment_light" %in% names(dot_args)) {
      warning(
        "`environment_light` supplied in `...` ignored because ",
        "`lat`, `long`, `datetime`, `sky_sun_elevation`, `sky_sun_azimuth`, ",
        "`sky_altitude`, or `sky_args` are set."
      )
      dot_args$environment_light = NULL
    }
    if ("environment_light_bake_white" %in% names(dot_args)) {
      warning(
        "`environment_light_bake_white` supplied in `...` ignored because ",
        "`lat`, `long`, `datetime`, `sky_sun_elevation`, `sky_sun_azimuth`, ",
        "`sky_altitude`, or `sky_args` generated an environment map."
      )
      dot_args$environment_light_bake_white = NULL
    }
    if ("filename" %in% names(sky_args)) {
      warning(
        "`sky_args$filename` ignored: sky EXR is cached in tempdir()."
      )
      sky_args$filename = NULL
    }

    format_sky_value = function(value) {
      if (is.null(value)) {
        return("null")
      }
      if (length(value) == 0) {
        return("empty")
      }
      if (inherits(value, "POSIXt")) {
        return(format(value, "%Y%m%dT%H%M%S%z"))
      }
      if (is.numeric(value)) {
        return(paste(
          format(value, digits = 8, scientific = FALSE, trim = TRUE),
          collapse = "-"
        ))
      }
      if (is.logical(value)) {
        return(paste(ifelse(value, "TRUE", "FALSE"), collapse = "-"))
      }
      if (is.character(value)) {
        return(paste(value, collapse = "-"))
      }
      paste(utils::capture.output(dput(value)), collapse = "")
    }

    lat_value = lat
    if (
      !is.null(sky_arg_names_raw) &&
        (is.null(lat_value) || is.na(lat_value)) &&
        "lat" %in% sky_arg_names_raw
    ) {
      lat_value = sky_args$lat
    }
    long_value = long
    if (
      !is.null(sky_arg_names_raw) &&
        (is.null(long_value) || is.na(long_value)) &&
        "lon" %in% sky_arg_names_raw
    ) {
      long_value = sky_args$lon
    }
    if (
      !is.null(sky_arg_names_raw) &&
        (is.null(long_value) || is.na(long_value)) &&
        "long" %in% sky_arg_names_raw
    ) {
      long_value = sky_args$long
    }
    datetime_value = datetime
    if (
      !is.null(sky_arg_names_raw) &&
        (is.null(datetime_value) || is.na(datetime_value)) &&
        "datetime" %in% sky_arg_names_raw
    ) {
      datetime_value = sky_args$datetime
    }
    elevation_value = sky_sun_elevation
    if (
      !is.null(sky_arg_names_raw) &&
        (is.null(elevation_value) || is.na(elevation_value)) &&
        "elevation" %in% sky_arg_names_raw
    ) {
      elevation_value = sky_args$elevation
    }
    azimuth_value = sky_sun_azimuth
    if (
      !is.null(sky_arg_names_raw) &&
        (is.null(azimuth_value) || is.na(azimuth_value)) &&
        "azimuth" %in% sky_arg_names_raw
    ) {
      azimuth_value = sky_args$azimuth
    }
    altitude_value = sky_altitude
    if (
      !is.null(sky_arg_names_raw) &&
        (is.null(altitude_value) || is.na(altitude_value)) &&
        "altitude" %in% sky_arg_names_raw
    ) {
      altitude_value = sky_args$altitude
    }
    if (
      sky_mode == "latlong" &&
        !is.null(datetime_value) &&
        !is.na(datetime_value) &&
        (is.null(lat_value) ||
          is.na(lat_value) ||
          is.null(long_value) ||
          is.na(long_value))
    ) {
      auto_latlong = resolve_cached_extent_center_latlong(
        caller = "render_highquality"
      )
      if (!is.null(auto_latlong)) {
        if (is.null(lat_value) || is.na(lat_value)) {
          lat_value = auto_latlong$lat
        }
        if (is.null(long_value) || is.na(long_value)) {
          long_value = auto_latlong$long
        }
      }
    }
    if (
      sky_mode == "latlong" &&
        !is.null(datetime_value) &&
        !is.na(datetime_value) &&
        (is.null(lat_value) ||
          is.na(lat_value) ||
          is.null(long_value) ||
          is.na(long_value))
    ) {
      stop(
        paste(
          "`datetime` requires `lat` and `long`, or cached extent and CRS metadata",
          "so rayshader can derive the scene center in latitude/longitude."
        ),
        call. = FALSE
      )
    }

    lat_key = if (!is.null(lat_value) && !is.na(lat_value)) {
      format_sky_value(lat_value)
    } else {
      "default"
    }
    long_key = if (!is.null(long_value) && !is.na(long_value)) {
      format_sky_value(long_value)
    } else {
      "default"
    }
    datetime_key = if (!is.null(datetime_value) && !is.na(datetime_value)) {
      format_sky_value(datetime_value)
    } else {
      "default"
    }
    elevation_key = if (!is.null(elevation_value) && !is.na(elevation_value)) {
      format_sky_value(elevation_value)
    } else {
      "default"
    }
    azimuth_key = if (!is.null(azimuth_value) && !is.na(azimuth_value)) {
      format_sky_value(azimuth_value)
    } else {
      "default"
    }
    altitude_key = if (!is.null(altitude_value) && !is.na(altitude_value)) {
      format_sky_value(altitude_value)
    } else {
      "default"
    }

    sky_args_for_key = sky_args
    sky_args_for_key[c(
      "lat",
      "lon",
      "long",
      "datetime",
      "elevation",
      "azimuth",
      "altitude"
    )] = NULL
    if (length(sky_args_for_key) > 0) {
      sky_arg_names = names(sky_args_for_key)
      if (is.null(sky_arg_names)) {
        sky_arg_names = paste0("arg", seq_along(sky_args_for_key))
      } else if (any(sky_arg_names == "")) {
        sky_arg_names[sky_arg_names == ""] = paste0(
          "arg",
          seq_along(sky_args_for_key)
        )[sky_arg_names == ""]
      }
      sky_args_key = paste(
        vapply(
          seq_along(sky_args_for_key),
          function(i) {
            paste0(
              sky_arg_names[i],
              "=",
              format_sky_value(sky_args_for_key[[i]])
            )
          },
          ""
        ),
        collapse = "__"
      )
    } else {
      sky_args_key = "default"
    }

    if (sky_mode == "direct") {
      sky_key = paste(
        "mode",
        "direct",
        "elevation",
        elevation_key,
        "azimuth",
        azimuth_key,
        "altitude",
        altitude_key,
        sky_args_key,
        sep = "__"
      )
    } else {
      sky_key = paste(
        "mode",
        "latlong",
        "lat",
        lat_key,
        "long",
        long_key,
        "datetime",
        datetime_key,
        sky_args_key,
        sep = "__"
      )
    }
    sky_key = gsub("[^A-Za-z0-9_-]", "-", sky_key)
    sky_key = gsub("-+", "-", sky_key)
    sky_key = substr(sky_key, 1, 160)
    sky_prefix = paste0("rayshader_sky_", sky_key, "_")
    existing_files = list.files(
      tempdir(),
      pattern = paste0("^", sky_prefix, ".*\\.exr$"),
      full.names = TRUE
    )
    if (length(existing_files) > 0) {
      sky_file = existing_files[1]
    } else {
      sky_file = tempfile(pattern = sky_prefix, fileext = ".exr")
    }

    if (!file.exists(sky_file)) {
      sky_call_args = sky_args
      sky_call_args$filename = sky_file
      if (sky_mode == "direct") {
        sky_call_args[c("lat", "lon", "long", "datetime")] = NULL
        if (!is.null(elevation_value) && !is.na(elevation_value)) {
          sky_call_args$elevation = elevation_value
        }
        if (!is.null(azimuth_value) && !is.na(azimuth_value)) {
          sky_call_args$azimuth = azimuth_value
        }
        if (!is.null(altitude_value) && !is.na(altitude_value)) {
          sky_call_args$altitude = altitude_value
        }
        do.call(skymodelr::generate_sky, sky_call_args)
      } else {
        sky_call_args[c("elevation", "azimuth")] = NULL
        if (
          "long" %in% names(sky_call_args) && !("lon" %in% names(sky_call_args))
        ) {
          sky_call_args$lon = sky_call_args$long
        }
        sky_call_args$long = NULL
        if (!is.null(lat_value) && !is.na(lat_value)) {
          sky_call_args$lat = lat_value
        }
        if (!is.null(long_value) && !is.na(long_value)) {
          sky_call_args$lon = long_value
        }
        if (!is.null(datetime_value) && !is.na(datetime_value)) {
          sky_call_args$datetime = datetime_value
        }
        do.call(skymodelr::generate_sky_latlong, sky_call_args)
      }
    }

    light = FALSE
    sky_environment_light_bake_white = TRUE
  }

  rgl_materials = validate_render_highquality_rgl_materials(rgl_materials)

  #Get scene info
  windowrect = rgl::par3d()$windowRect
  if (!is.null(title_text)) {
    has_title = TRUE
  } else {
    has_title = FALSE
  }
  if (is.null(width)) {
    width = windowrect[3] - windowrect[1]
  }
  if (is.null(height)) {
    height = windowrect[4] - windowrect[2]
  }
  if (.Platform$OS.type == "windows") {
    sepval = "\\"
  } else {
    sepval = "/"
  }

  surfaceid = get_ids_with_labels(typeval = c("surface", "surface_tris"))
  surfacevertices = rgl.attrib(surfaceid$id[1], "vertices")
  polygonid = get_ids_with_labels(typeval = c("polygon3d"))
  baseid = get_ids_with_labels(typeval = c("base"))
  if (nrow(polygonid) > 0) {
    polyrange = c()
    polyrange_x = c()
    polyrange_z = c()
    for (i in seq_len(nrow(polygonid))) {
      tempverts = range(rgl.attrib(polygonid$id[i], "vertices")[, 2])
      tempverts_x = range(rgl.attrib(polygonid$id[i], "vertices")[, 1])
      tempverts_z = range(rgl.attrib(polygonid$id[i], "vertices")[, 3])

      if (all(!is.na(tempverts))) {
        polyrange = range(c(tempverts, polyrange))
      }
      if (all(!is.na(tempverts_x))) {
        polyrange_x = range(c(tempverts_x, polyrange_x))
      }
      if (all(!is.na(tempverts_z))) {
        polyrange_z = range(c(tempverts_z, polyrange_z))
      }
    }
  }
  if (nrow(baseid) > 0) {
    baserange = c()
    for (i in seq_len(nrow(baseid))) {
      tempverts = range(rgl.attrib(baseid$id[i], "vertices")[, 2])
      if (all(!is.na(tempverts))) {
        baserange = range(c(tempverts, baserange))
      }
    }
  }
  surfacerange = range(surfacevertices[, 2], na.rm = TRUE)
  if (nrow(polygonid) > 0) {
    surfacerange[2] = range(c(surfacerange, polyrange))[2]
  }
  if (nrow(baseid) > 0) {
    surfacerange[2] = range(c(surfacerange, baserange))[2]
  }
  shadowid = get_ids_with_labels(typeval = "shadow")
  if (nrow(shadowid) > 0) {
    shadowvertices = rgl.attrib(shadowid$id[1], "vertices")
    shadowdepth = shadowvertices[1, 2]
    has_shadow = TRUE
  } else {
    has_shadow = FALSE
  }
  camera_interpolate[camera_interpolate > 1] = 1
  camera_interpolate[camera_interpolate < 0] = 0
  fov = rgl::par3d()$FOV
  rotmat = rot_to_euler(rgl::par3d()$userMatrix)
  projmat = rgl::par3d()$projMatrix
  zoom = rgl::par3d()$zoom
  scalevals = rgl::par3d("scale")

  phi = rotmat[1]
  if (90 - abs(phi) < 1e-3) {
    phi = -phi
  }
  if (0.001 > abs(abs(rotmat[3]) - 180)) {
    theta = -rotmat[2] + 180
    movevec = rgl::rotationMatrix(-rotmat[2] * pi / 180, 0, 1, 0) %*%
      rgl::rotationMatrix(-phi * pi / 180, 1, 0, 0) %*%
      rgl::par3d()$userMatrix[, 4]
  } else {
    theta = rotmat[2]
    movevec = rgl::rotationMatrix(rotmat[3] * pi / 180, 0, 0, 1) %*%
      rgl::rotationMatrix(rotmat[2] * pi / 180, 0, 1, 0) %*%
      rgl::rotationMatrix(-phi * pi / 180, 1, 0, 0) %*%
      rgl::par3d()$userMatrix[, 4]
  }
  movevec = movevec[1:3]
  observer_radius = rgl::par3d()$observer[3]
  lookvals = rgl::par3d()$bbox
  # lookvals[4] = surfacerange[2]
  if (fov == 0) {
    ortho_dimensions = c(2 / projmat[1, 1], 2 / projmat[2, 2])
  } else {
    fov = 2 * atan(1 / projmat[2, 2]) * 180 / pi
    ortho_dimensions = c(1, 1)
  }
  if (!is.null(ortho_dimensions_override)) {
    ortho_dimensions_override = suppressWarnings(as.numeric(
      ortho_dimensions_override
    )[1:2])
    if (
      length(ortho_dimensions_override) != 2 ||
        any(!is.finite(ortho_dimensions_override)) ||
        any(ortho_dimensions_override <= 0)
    ) {
      stop(
        "`ortho_dimensions` must be a length-2 numeric vector with positive values."
      )
    }
    ortho_dimensions = ortho_dimensions_override
  }
  bbox_center = c(
    mean(lookvals[1:2]),
    mean(lookvals[3:4]),
    mean(lookvals[5:6])
  ) -
    movevec
  camera_location = resolve_render_highquality_camera_point(
    camera_location,
    arg_name = "camera_location",
    bbox_center = bbox_center,
    caller = "render_highquality"
  )
  camera_lookat = resolve_render_highquality_camera_point(
    camera_lookat,
    arg_name = "camera_lookat",
    bbox_center = bbox_center,
    caller = "render_highquality"
  )
  rgl_material_info = get_render_highquality_rgl_material_info(rgl_materials)
  raymesh_material_info = get_render_highquality_raymesh_material_info(
    rgl_material_info
  )
  raymesh_material_info = add_render_highquality_water_material_info(
    raymesh_material_info = raymesh_material_info,
    water_material = water_material,
    rgl_material_info = rgl_material_info
  )
  raymesh_material_ids = unique(raymesh_material_info$id)
  if (
    !use_extruded_paths &&
      has_render_highquality_dielectric_path_override(
        rgl_materials,
        rgl_material_info
      )
  ) {
    message(
      "dielectric material for paths selected--setting `use_extruded_paths = TRUE` for accurate rendering of material"
    )
    use_extruded_paths = TRUE
  }
  theta = theta + 180
  observery = sinpi(phi / 180) * observer_radius
  observerx = cospi(phi / 180) * sinpi(theta / 180) * observer_radius
  observerz = cospi(phi / 180) * cospi(theta / 180) * observer_radius
  if (is.null(camera_location)) {
    lookfrom = c(observerx, observery, observerz)
  } else {
    lookfrom = camera_location
  }
  if (length(camera_interpolate) == 1) {
    camera_interpolate = c(camera_interpolate, camera_interpolate)
  }
  if (is.null(camera_lookat)) {
    camera_lookat = c(0, 0, 0)
  }
  if (all(camera_interpolate != 1)) {
    if (!is.null(camera_location)) {
      lookfrom = (1 - camera_interpolate[1]) *
        c(observerx, observery, observerz) +
        camera_interpolate[1] * camera_location
    }
    if (!is.null(camera_lookat)) {
      camera_lookat = camera_interpolate[2] * camera_lookat
    }
  }
  water_attenuation = abs(water_attenuation)
  if (any(water_attenuation > 1)) {
    warnings(
      "`water_attenuation` should only be a value of 1 or less--clamping at 0-1"
    )
    water_attenuation[water_attenuation > 1] = 1
  }
  if (cache_scene && length(raymesh_material_ids) == 0) {
    ray_scene = get_scene_cache(default = NULL)
    if (is.null(ray_scene)) {
      ray_scene = convert_rgl_to_raymesh(
        save_shadow = FALSE,
        water_attenuation = water_attenuation,
        water_surface_color = water_surface_color,
        water_ior = water_ior
      )
      cache_scene_cache(ray_scene)
    }
  } else {
    ray_scene = convert_rgl_to_raymesh(
      save_shadow = FALSE,
      water_attenuation = water_attenuation,
      water_surface_color = water_surface_color,
      water_ior = water_ior,
      exclude_ids = raymesh_material_ids
    )
  }

  if (is_empty_raymesh_scene(ray_scene)) {
    scene = NULL
  } else if (!override_material) {
    scene = rayrender::raymesh_model(
      ray_scene,
      x = -bbox_center[1],
      y = -bbox_center[2],
      z = -bbox_center[3],
      override_material = FALSE,
      flip_transmittance = FALSE,
      calculate_consistent_normals = calculate_consistent_normals
    )
  } else {
    scene = rayrender::raymesh_model(
      ray_scene,
      x = -bbox_center[1],
      y = -bbox_center[2],
      z = -bbox_center[3],
      material = material,
      override_material = TRUE,
      flip_transmittance = FALSE,
      calculate_consistent_normals = calculate_consistent_normals
    )
  }
  if (nrow(raymesh_material_info) > 0) {
    for (material_row in seq_len(nrow(raymesh_material_info))) {
      temp_ray_scene = convert_rgl_to_raymesh(
        save_shadow = FALSE,
        water_attenuation = water_attenuation,
        water_surface_color = water_surface_color,
        water_ior = water_ior,
        include_ids = raymesh_material_info$id[[material_row]]
      )
      if (is_empty_raymesh_scene(temp_ray_scene)) {
        next
      }
      temp_material = resolve_render_highquality_rgl_material(
        rgl_materials = rgl_materials,
        id = raymesh_material_info$id[[material_row]],
        tag = raymesh_material_info$tag[[material_row]],
        color = get_render_highquality_rgl_material_color(
          raymesh_material_info$id[[material_row]]
        )
      )
      if (
        is.null(temp_material) &&
          identical(water_material, "microfacet") &&
          identical(raymesh_material_info$tag[[material_row]], "water")
      ) {
        temp_material = make_render_highquality_water_microfacet_material(
          color = get_render_highquality_rgl_material_color(
            raymesh_material_info$id[[material_row]]
          ),
          water_roughness = water_roughness,
          water_ior = water_ior,
          water_attenuation = water_attenuation,
          water_surface_color = water_surface_color
        )
      }
      if (is.null(temp_material)) {
        next
      }
      temp_model = rayrender::raymesh_model(
        temp_ray_scene,
        x = -bbox_center[1],
        y = -bbox_center[2],
        z = -bbox_center[3],
        material = temp_material,
        override_material = TRUE,
        flip_transmittance = FALSE,
        calculate_consistent_normals = calculate_consistent_normals
      )
      if (is.null(scene)) {
        scene = temp_model
      } else {
        scene = rayrender::add_object(scene, temp_model)
      }
    }
  }
  has_rayimage = TRUE
  needs_rayimage_text = identical(text_render, "world")
  if (
    needs_rayimage_text && !(length(find.package("rayimage", quiet = TRUE)) > 0)
  ) {
    warning("`rayimage` package required for labels")
    has_rayimage = FALSE
  }
  labelids = get_ids_with_labels(
    typeval = c("raytext", "zaxis_labels", "zaxis_title")
  )$id
  labels = list()
  labels_screen = list()
  counter = 1
  screen_counter = 1
  for (i in seq_len(length(labelids))) {
    if (!has_rayimage && identical(text_render, "world")) {
      break
    }
    temp_label = rgl.attrib(labelids[i], "texts")
    temp_center = rgl.attrib(labelids[i], "centers")
    temp_color = rgl.attrib(labelids[i], "colors")
    temp_adj = tryCatch(
      rgl.attrib(labelids[i], "adj"),
      error = function(e) NULL
    )
    temp_cex = tryCatch(
      rgl.attrib(labelids[i], "cex"),
      error = function(e) NULL
    )
    for (j in seq_len(nrow(temp_label))) {
      temp_size = text_size * get_render_highquality_text_cex(temp_cex, j)
      if (identical(text_render, "screen")) {
        screen_point = transform_render_highquality_screen_points(
          temp_center[j, ] + screen_text_offset,
          bbox_center = bbox_center
        )
        labels_screen[[screen_counter]] = make_render_highquality_screen_text(
          screen_text_args = screen_text_args,
          label = temp_label[j, 1],
          point = screen_point,
          size = temp_size,
          color = format_render_highquality_screen_color(
            select_render_highquality_color(temp_color, j)
          ),
          hjust = get_render_highquality_screen_text_just(temp_adj, 1, j),
          vjust = get_render_highquality_screen_text_just(temp_adj, 2, j),
          occlusion = text_occlusion,
          occlusion_mode = text_occlusion_mode,
          occlusion_tolerance = text_occlusion_tolerance
        )
        screen_counter = screen_counter + 1
      } else {
        anglevec = resolve_render_label_text_angle_rayrender(
          text_angle = text_angle,
          phi = phi,
          theta = theta
        )
        labels[[counter]] = rayrender::text3d(
          label = temp_label[j, 1],
          x = temp_center[j, 1] - bbox_center[1] + text_offset[1],
          y = temp_center[j, 2] - bbox_center[2] + text_offset[2],
          z = temp_center[j, 3] - bbox_center[3] + text_offset[3],
          angle = anglevec,
          text_height = temp_size,
          font_color = select_render_highquality_color(temp_color, j)[1:3]
        )
        counter = counter + 1
      }
    }
  }
  if (length(labels) > 0) {
    all_labels = do.call(rbind, labels)
    scene = rayrender::add_object(scene, all_labels)
  }
  labellineinfo = get_ids_with_labels(typeval = "textline")
  labellineids = labellineinfo$id
  labelline = list()
  labelline_screen = list()
  counter = 1
  screen_counter = 1
  for (i in seq_len(length(labellineids))) {
    temp_verts = rgl.attrib(labellineids[i], "vertices")
    temp_color = rgl.attrib(labellineids[i], "colors")
    temp_lwd = material3d("lwd", id = labellineids[i])
    use_screen_line = should_render_highquality_screen_line(
      line_render = line_render,
      tag = labellineinfo$tag[i]
    )
    for (j in seq_len(nrow(temp_verts) / 2)) {
      temp_color_single = select_render_highquality_color(temp_color, j)
      if (use_screen_line) {
        line_start = transform_render_highquality_screen_points(
          temp_verts[2 * j - 1, ],
          bbox_center = bbox_center
        )
        line_end = transform_render_highquality_screen_points(
          temp_verts[2 * j, ],
          bbox_center = bbox_center
        )
        labelline_screen[[
          screen_counter
        ]] = make_render_highquality_screen_line(
          screen_line_args = screen_line_args,
          start = line_start,
          end = line_end,
          width = max(1, temp_lwd * line_radius * 2),
          color = format_render_highquality_screen_color(temp_color_single),
          alpha = select_render_highquality_alpha(temp_color_single),
          occlusion = line_occlusion,
          occlusion_mode = line_occlusion_mode,
          occlusion_tolerance = line_occlusion_tolerance
        )
        screen_counter = screen_counter + 1
      } else {
        temp_material = resolve_render_highquality_rgl_material(
          rgl_materials = rgl_materials,
          id = labellineids[i],
          tag = labellineinfo$tag[i],
          color = temp_color_single[1:3]
        )
        if (is.null(temp_material)) {
          temp_material = rayrender::diffuse(color = temp_color_single[1:3])
        }
        labelline[[counter]] = rayrender::segment(
          start = temp_verts[2 * j - 1, ] - bbox_center,
          end = temp_verts[2 * j, ] - bbox_center,
          radius = line_radius,
          material = temp_material
        )
        counter = counter + 1
      }
    }
  }
  pathinfo = get_ids_with_labels(
    typeval = c(
      "path3d",
      "contour3d",
      "water_path",
      "road_path",
      "zaxis_axis",
      "zaxis_ticks"
    )
  )
  pathids = pathinfo$id
  pathline = list()
  pathline_screen = list()
  counter = 1
  screen_counter = 1
  water_path_surface = resolve_render_highquality_water_path_surface()
  water_path_tasks = list()
  road_path_tasks = list()
  for (i in seq_len(length(pathids))) {
    temp_verts = rgl.attrib(pathids[i], "vertices")
    temp_verts_split = split_render_highquality_path_vertices(temp_verts)
    temp_color = rgl.attrib(pathids[i], "colors")
    temp_lwd_raw = material3d("lwd", id = pathids[i])
    temp_lwd = temp_lwd_raw * line_radius
    is_water_path = identical(pathinfo$tag[i], "water_path")
    is_road_path = identical(pathinfo$tag[i], "road_path")
    road_path_info = if (is_road_path) {
      get_render_road_path_info(pathids[i])
    } else {
      NULL
    }
    road_texture_file = road_path_info$texture_file
    road_texture_length = road_path_info$texture_length
    if (is.null(road_texture_length)) {
      road_texture_length = 13
    }
    road_texture_repeats = road_path_info$texture_repeats
    road_texture_world_scale = road_path_info$texture_world_scale
    use_screen_line = should_render_highquality_screen_line(
      line_render = line_render,
      tag = pathinfo$tag[i]
    )
    for (j in seq_along(temp_verts_split)) {
      temp_verts_single = as.matrix(temp_verts_split[[j]])
      if (is_water_path || is_road_path) {
        temp_verts_single = collapse_render_highquality_path_vertices(
          temp_verts_single
        )
      }
      if (nrow(temp_verts_single) < 2) {
        next
      }
      if (nrow(temp_color) == 1) {
        temp_color = matrix(
          temp_color[1:3],
          byrow = TRUE,
          ncol = 3,
          nrow = nrow(temp_verts_single)
        )
      }
      temp_color_single = select_render_highquality_color(temp_color, 1)
      if (use_screen_line) {
        if (identical(pathinfo$tag[i], "zaxis_ticks")) {
          for (tick_index in seq_len(nrow(temp_verts_single))) {
            tick_point = transform_render_highquality_screen_points(
              temp_verts_single[tick_index, ],
              bbox_center = bbox_center
            )
            pathline_screen[[
              screen_counter
            ]] = make_render_highquality_screen_line(
              screen_line_args = screen_line_args,
              start = tick_point,
              end = tick_point,
              offset = c(-max(2, temp_lwd * 2), 0),
              end_offset = c(max(2, temp_lwd * 2), 0),
              width = max(1, temp_lwd * 2),
              color = format_render_highquality_screen_color(temp_color_single),
              alpha = select_render_highquality_alpha(temp_color_single),
              occlusion = line_occlusion,
              occlusion_mode = line_occlusion_mode,
              occlusion_tolerance = line_occlusion_tolerance
            )
            screen_counter = screen_counter + 1
          }
        } else if (nrow(temp_verts_single) >= 2) {
          screen_points = transform_render_highquality_screen_points(
            temp_verts_single,
            bbox_center = bbox_center
          )
          for (segment_index in seq_len(nrow(screen_points) - 1)) {
            pathline_screen[[
              screen_counter
            ]] = make_render_highquality_screen_line(
              screen_line_args = screen_line_args,
              start = screen_points[segment_index, , drop = FALSE],
              end = screen_points[segment_index + 1, , drop = FALSE],
              width = max(1, temp_lwd * 2),
              color = format_render_highquality_screen_color(temp_color_single),
              alpha = select_render_highquality_alpha(temp_color_single),
              occlusion = line_occlusion,
              occlusion_mode = line_occlusion_mode,
              occlusion_tolerance = line_occlusion_tolerance
            )
            screen_counter = screen_counter + 1
          }
        }
        next
      }
      matrix_center = matrix(
        bbox_center,
        byrow = TRUE,
        ncol = 3,
        nrow = nrow(temp_verts_single)
      )
      temp_material = resolve_render_highquality_rgl_material(
        rgl_materials = rgl_materials,
        id = pathids[i],
        tag = pathinfo$tag[i],
        color = temp_color_single[1:3]
      )
      has_material_override = !is.null(temp_material)
      if (is.null(temp_material)) {
        if (is_water_path) {
          temp_material = make_render_highquality_water_path_material(
            color = temp_color_single[1:3],
            water_material = water_material,
            water_roughness = water_roughness,
            water_ior = water_ior,
            water_attenuation = water_attenuation,
            water_surface_color = water_surface_color
          )
        } else if (is_road_path && !is.null(road_texture_file)) {
          temp_material = rayrender::diffuse(
            color = "white",
            image_texture = road_texture_file,
            image_repeat = 1
          )
        } else {
          temp_material = rayrender::diffuse(color = temp_color_single[1:3])
        }
      }

      if (is_water_path) {
        water_path_tasks[[length(water_path_tasks) + 1L]] = list(
          points = temp_verts_single,
          bbox_center = bbox_center,
          width = temp_lwd_raw,
          heightmap = water_path_surface$heightmap,
          zscale = water_path_surface$zscale,
          material = temp_material
        )
        next
      } else if (is_road_path) {
        road_path_tasks[[length(road_path_tasks) + 1L]] = list(
          points = temp_verts_single,
          bbox_center = bbox_center,
          width = temp_lwd_raw,
          heightmap = water_path_surface$heightmap,
          zscale = water_path_surface$zscale,
          material = temp_material,
          texture_file = if (!has_material_override) {
            road_texture_file
          } else {
            NULL
          },
          texture_length = road_texture_length,
          texture_repeats = road_texture_repeats,
          texture_world_scale = road_texture_world_scale
        )
        next
      } else if (use_extruded_paths) {
        pathline[[counter]] = rayrender::extruded_path(
          points = temp_verts_single - matrix_center,
          width = temp_lwd * 2,
          smooth_normals = TRUE,
          straight = !smooth_line,
          material = temp_material
        )
      } else {
        pathline[[counter]] = rayrender::path(
          points = temp_verts_single - matrix_center,
          width = temp_lwd * 2,
          straight = !smooth_line,
          material = temp_material
        )
      }
      counter = counter + 1
    }
  }
  water_path_meshes = if (isTRUE(joined_stream_mesh)) {
    make_render_highquality_joined_water_path_meshes(water_path_tasks)
  } else {
    make_render_highquality_water_path_meshes(water_path_tasks)
  }
  if (length(water_path_meshes) > 0) {
    pathline = c(pathline, water_path_meshes)
  }
  road_path_meshes = make_render_highquality_road_path_meshes(road_path_tasks)
  if (length(road_path_meshes) > 0) {
    pathline = c(pathline, road_path_meshes)
  }
  pointinfo = get_ids_with_labels(typeval = "points3d")
  pointids = pointinfo$id
  pointlist = list()
  counter = 1
  for (i in seq_len(length(pointids))) {
    temp_verts = rgl.attrib(pointids[i], "vertices")
    temp_color = rgl.attrib(pointids[i], "colors")
    temp_size = material3d("size", id = pointids[i]) * point_radius
    can_use_instances = FALSE
    if (nrow(temp_color) == 1) {
      temp_color = matrix(
        temp_color[1:3],
        byrow = TRUE,
        ncol = 3,
        nrow = nrow(temp_verts)
      )
      can_use_instances = TRUE
    }
    if (can_use_instances) {
      temp_material = resolve_render_highquality_rgl_material(
        rgl_materials = rgl_materials,
        id = pointids[i],
        tag = pointinfo$tag[i],
        color = temp_color[1, 1:3]
      )
      if (is.null(temp_material)) {
        temp_material = rayrender::diffuse(color = temp_color[1, 1:3])
      }
      pointlist[[counter]] = rayrender::create_instances(
        rayrender::sphere(
          radius = temp_size,
          material = temp_material
        ),
        x = temp_verts[, 1] - bbox_center[1],
        y = temp_verts[, 2] - bbox_center[2],
        z = temp_verts[, 3] - bbox_center[3],
      )
      counter = counter + 1
    } else {
      for (j in seq_len(nrow(temp_verts))) {
        temp_material = resolve_render_highquality_rgl_material(
          rgl_materials = rgl_materials,
          id = pointids[i],
          tag = pointinfo$tag[i],
          color = temp_color[j, 1:3]
        )
        if (is.null(temp_material)) {
          temp_material = rayrender::diffuse(color = temp_color[j, 1:3])
        }

        pointlist[[counter]] = rayrender::sphere(
          x = temp_verts[j, 1] - bbox_center[1],
          y = temp_verts[j, 2] - bbox_center[2],
          z = temp_verts[j, 3] - bbox_center[3],
          radius = temp_size,
          material = temp_material
        )
        counter = counter + 1
      }
    }
  }
  scalelabelids = get_ids_with_labels(typeval = "text_scalebar")$id
  scalelabels = list()
  scalelabels_screen = list()
  counter = 1
  screen_counter = 1
  for (i in seq_len(length(scalelabelids))) {
    if (!has_rayimage && identical(text_render, "world")) {
      break
    }
    temp_label = rgl.attrib(scalelabelids[i], "texts")
    temp_center = rgl.attrib(scalelabelids[i], "centers")
    temp_color = rgl.attrib(scalelabelids[i], "colors")
    temp_adj = tryCatch(
      rgl.attrib(scalelabelids[i], "adj"),
      error = function(e) NULL
    )
    temp_cex = tryCatch(
      rgl.attrib(scalelabelids[i], "cex"),
      error = function(e) NULL
    )
    for (j in seq_len(nrow(temp_label))) {
      temp_size = scale_text_size * get_render_highquality_text_cex(temp_cex, j)
      if (identical(text_render, "screen")) {
        screen_point = transform_render_highquality_screen_points(
          temp_center[j, ] + screen_scale_text_offset,
          bbox_center = bbox_center
        )
        scalelabels_screen[[
          screen_counter
        ]] = make_render_highquality_screen_text(
          screen_text_args = screen_text_args,
          label = temp_label[j, 1],
          point = screen_point,
          size = temp_size,
          color = format_render_highquality_screen_color(
            select_render_highquality_color(temp_color, j)
          ),
          hjust = get_render_highquality_screen_text_just(temp_adj, 1, j),
          vjust = get_render_highquality_screen_text_just(temp_adj, 2, j),
          occlusion = text_occlusion,
          occlusion_mode = text_occlusion_mode,
          occlusion_tolerance = text_occlusion_tolerance
        )
        screen_counter = screen_counter + 1
      } else {
        anglevec = resolve_render_label_text_angle_rayrender(
          text_angle = text_angle,
          phi = phi,
          theta = theta
        )
        scalelabels[[counter]] = rayrender::text3d(
          x = temp_center[j, 1] - bbox_center[1] + scale_text_offset[1],
          y = temp_center[j, 2] - bbox_center[2] + scale_text_offset[2],
          z = temp_center[j, 3] - bbox_center[3] + scale_text_offset[3],
          angle = anglevec,
          label = temp_label[j, 1],
          text_height = temp_size,
          font_color = select_render_highquality_color(temp_color, j)[1:3]
        )
        counter = counter + 1
      }
    }
  }
  screen_labels = c(labels_screen, scalelabels_screen)
  screen_lines = c(labelline_screen, pathline_screen)
  if (length(labels) > 0) {
    all_labels = do.call(rbind, labels)
    scene = rayrender::add_object(scene, all_labels)
  }
  if (length(labelline) > 0) {
    all_labellines = do.call(rbind, labelline)
    scene = rayrender::add_object(scene, all_labellines)
  }
  if (length(scalelabels) > 0) {
    all_scalelabels = do.call(rbind, scalelabels)
    scene = rayrender::add_object(scene, all_scalelabels)
  }
  if (length(pathline) > 0) {
    all_pathline = do.call(rbind, pathline)
    scene = rayrender::add_object(scene, all_pathline)
  }
  if (length(pointlist) > 0) {
    all_pointlist = do.call(rbind, pointlist)
    scene = rayrender::add_object(scene, all_pointlist)
  }
  screen_text_objects = if (length(screen_labels) > 0) {
    do.call(rbind, screen_labels)
  } else {
    NULL
  }
  screen_line_objects = if (length(screen_lines) > 0) {
    do.call(rbind, screen_lines)
  } else {
    NULL
  }
  if (has_shadow) {
    scene = rayrender::add_object(
      scene,
      rayrender::xz_rect(
        zwidth = ground_size,
        xwidth = ground_size,
        y = shadowdepth - bbox_center[2],
        material = ground_material
      )
    )
  }
  if (any(round(scalevals, 4) != 1)) {
    scene = rayrender::group_objects(
      scene,
      scale = scalevals,
      pivot_point = c(0, 0, 0)
    )
  }
  lookfrom_rayrender = lookfrom
  camera_lookat_rayrender = camera_lookat
  animation_camera_coords_rayrender = animation_camera_coords

  if (light) {
    if (is.null(lightsize)) {
      lightsize = observer_radius / 5
    }
    if (length(lightaltitude) >= 1 || length(lightdirection) >= 1) {
      if (
        length(lightaltitude) > 1 &&
          length(lightdirection) > 1 &&
          length(lightdirection) != length(lightaltitude)
      ) {
        stop(
          "lightaltitude vector ",
          lightaltitude,
          " and lightdirection vector ",
          lightdirection,
          "both greater than length 1 but not equal length"
        )
      }
      numberlights = ifelse(
        length(lightaltitude) > length(lightdirection),
        length(lightaltitude),
        length(lightdirection)
      )
      lightaltitudetemp = lightaltitude[1]
      lightdirectiontemp = lightdirection[1]
      lightintensitytemp = lightintensity[1]
      lightcolortemp = lightcolor[1]
      lightsizetemp = lightsize[1]
      for (i in seq_len(numberlights)) {
        if (!is.na(lightaltitude[i])) {
          lightaltitudetemp = lightaltitude[i]
        }
        if (!is.na(lightdirection[i])) {
          lightdirectiontemp = lightdirection[i]
        }
        if (!is.na(lightintensity[i])) {
          lightintensitytemp = lightintensity[i]
        }
        if (!is.na(lightcolor[i])) {
          lightcolortemp = lightcolor[i]
        }
        if (!is.na(lightsize[i])) {
          lightsizetemp = lightsize[i]
        }
        scene = rayrender::add_object(
          scene,
          rayrender::sphere(
            x = observer_radius *
              5 *
              cospi(lightaltitudetemp / 180) *
              sinpi(lightdirectiontemp / 180),
            y = observer_radius *
              5 *
              sinpi(lightaltitudetemp / 180),
            z = -observer_radius *
              5 *
              cospi(lightaltitudetemp / 180) *
              cospi(lightdirectiontemp / 180),
            radius = lightsizetemp,
            material = rayrender::light(
              color = lightcolortemp,
              intensity = lightintensitytemp
            )
          )
        )
      }
    }
  }
  if (print_scene_info) {
    dist_val = sqrt(sum((camera_lookat - lookfrom)^2))
    print(sprintf(
      "Camera position: c(%0.2f, %0.2f, %0.2f), Camera Lookat: c(%0.2f, %0.2f, %0.2f) Focal Distance: %0.2f Scene Offset:  c(%0.2f, %0.2f, %0.2f)",
      lookfrom[1],
      lookfrom[2],
      lookfrom[3],
      camera_lookat[1],
      camera_lookat[2],
      camera_lookat[3],
      dist_val,
      -bbox_center[1],
      -bbox_center[2],
      -bbox_center[3]
    ))
  }
  if (!is.null(scene_elements)) {
    scene = rayrender::add_object(scene, scene_elements)
  }

  scene = rayrender::group_objects(scene, angle = c(0, 180, 0))

  if (return_scene) {
    attr(scene, "screen_text") = screen_text_objects
    attr(scene, "screen_line") = screen_line_objects
    if (!is.null(sky_file)) {
      attr(scene, "environment_light") = sky_file
      attr(scene, "environment_light_bake_white") =
        sky_environment_light_bake_white
    }
    return(scene)
  }

  if (!is.null(animation_camera_coords)) {
    if (
      length(dim(animation_camera_coords)) != 2 ||
        ncol(animation_camera_coords) != 14
    ) {
      stop(
        "`animation_camera_coords` must be a two-dimensional object with exactly 14 columns.",
        call. = FALSE
      )
    }
    animation_args = list(
      scene = scene,
      camera_motion = animation_camera_coords_rayrender,
      width = width,
      height = height,
      min_variance = min_variance,
      samples = samples,
      sample_method = sample_method,
      filename = filename,
      clamp_value = clamp_value
    )
    if (!is.null(screen_text_objects) || !is.null(screen_line_objects)) {
      warning(
        "Screen-space text and lines are not supported by `rayrender::render_animation()`; ",
        "use `text_render = \"world\"` and `line_render = \"world\"` for animated labels."
      )
    }
    if (!is.null(sky_file)) {
      animation_args$environment_light = sky_file
      if (
        sky_environment_light_bake_white &&
          "environment_light_bake_white" %in%
            names(formals(rayrender::render_animation))
      ) {
        animation_args$environment_light_bake_white = TRUE
      }
    }
    do.call(rayrender::render_animation, c(animation_args, dot_args))
    return()
  }

  render_scene_args = list(
    scene = scene,
    lookfrom = lookfrom_rayrender,
    lookat = camera_lookat_rayrender,
    fov = fov,
    min_variance = min_variance,
    samples = samples,
    sample_method = sample_method,
    ortho_dimensions = ortho_dimensions,
    width = width,
    height = height,
    clamp_value = clamp_value,
    plot_scene = plot
  )
  if (!is.null(screen_text_objects)) {
    render_scene_args$screen_text = screen_text_objects
  }
  if (!is.null(screen_line_objects)) {
    render_scene_args$screen_line = screen_line_objects
  }
  if (!is.null(sky_file)) {
    render_scene_args$environment_light = sky_file
    if (
      sky_environment_light_bake_white &&
        render_scene_supports_environment_light_bake_white
    ) {
      render_scene_args$environment_light_bake_white = TRUE
    }
  }
  duplicate_render_scene_args = intersect(
    names(render_scene_args),
    names(dot_args)
  )
  if (length(duplicate_render_scene_args) > 0) {
    dot_args[duplicate_render_scene_args] = NULL
  }
  render_scene_call = function(extra_args = list()) {
    do.call(
      rayrender::render_scene,
      c(render_scene_args, extra_args, dot_args)
    )
  }

  if (has_title) {
    temp = tempfile(fileext = ".png")
    debug_return = render_scene_call(list(filename = temp))
    if (plot) {
      temp = rayimage::ray_read_image(temp)
      rayimage::render_title(
        temp,
        title_text = title_text,
        title_color = title_color,
        title_font = title_font,
        title_offset = title_offset,
        title_just = title_just,
        title_bar_alpha = title_bar_alpha,
        title_bar_color = title_bar_color,
        title_size = title_size,
        preview = TRUE
      )
    } else {
      temp = rayimage::ray_read_image(temp)
      rayimage::render_title(
        temp,
        title_text = title_text,
        title_color = title_color,
        title_font = title_font,
        title_offset = title_offset,
        title_just = title_just,
        title_bar_alpha = title_bar_alpha,
        title_bar_color = title_bar_color,
        title_size = title_size,
        filename = filename
      )
    }
  } else {
    if (plot) {
      debug_return = render_scene_call()
    } else {
      debug_return = render_scene_call(list(filename = filename))
    }
  }
  if (clear) {
    clear_plot_3d_surface_textures()
    rgl::clear3d()
  }
  return(invisible(debug_return))
}

get_render_highquality_camera_list_value = function(
  camera_point,
  candidate_names
) {
  camera_names = names(camera_point)
  if (is.null(camera_names)) {
    return(NULL)
  }
  value_index = match(candidate_names, camera_names, nomatch = 0)
  value_index = value_index[value_index > 0]
  if (!length(value_index)) {
    return(NULL)
  }
  camera_point[[value_index[1]]]
}

has_render_highquality_camera_list_value = function(
  camera_point,
  candidate_names
) {
  camera_names = names(camera_point)
  !is.null(camera_names) && any(candidate_names %in% camera_names)
}

normalize_render_highquality_camera_scalar = function(
  value,
  value_name,
  arg_name,
  caller = NULL,
  allow_null = FALSE
) {
  if (is.null(value)) {
    if (isTRUE(allow_null)) {
      return(NULL)
    }
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`",
        arg_name,
        "$",
        value_name,
        "` must be a finite number."
      ),
      call. = FALSE
    )
  }
  value = suppressWarnings(as.numeric(value)[1])
  if (!is.finite(value)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`",
        arg_name,
        "$",
        value_name,
        "` must be a finite number."
      ),
      call. = FALSE
    )
  }
  value
}

normalize_render_highquality_camera_altitude = function(
  altitude,
  arg_name,
  caller = NULL
) {
  normalize_render_highquality_camera_scalar(
    altitude,
    value_name = "altitude",
    arg_name = arg_name,
    caller = caller,
    allow_null = TRUE
  )
}

normalize_render_highquality_camera_raw_vector = function(
  camera_point,
  arg_name,
  caller = NULL
) {
  camera_point = suppressWarnings(as.numeric(camera_point))
  if (length(camera_point) != 3 || any(!is.finite(camera_point))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`",
        arg_name,
        "` must be a numeric length-3 scene coordinate vector, ",
        "a named `long`/`lat`/`altitude` vector, a spatial POINT, ",
        "or a list with spatial camera fields."
      ),
      call. = FALSE
    )
  }
  unname(camera_point)
}

normalize_render_highquality_camera_input = function(
  camera_point,
  arg_name,
  caller = NULL
) {
  if (is.null(camera_point)) {
    return(NULL)
  }
  if (inherits(camera_point, c("sf", "sfc", "sfg", "Spatial"))) {
    return(list(
      type = "location",
      location = camera_point,
      altitude = NULL,
      crs = NULL,
      panel = NULL
    ))
  }
  if (is.data.frame(camera_point)) {
    if (nrow(camera_point) != 1) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`",
          arg_name,
          "` data frame inputs must contain exactly one row."
        ),
        call. = FALSE
      )
    }
    camera_point = lapply(camera_point, function(value) value[[1]])
  }
  if (is.numeric(camera_point)) {
    if (
      has_render_highquality_camera_list_value(
        camera_point,
        c("lat", "latitude")
      ) &&
        has_render_highquality_camera_list_value(
          camera_point,
          c("long", "lon", "lng", "longitude")
        )
    ) {
      return(list(
        type = "xy",
        x = normalize_render_highquality_camera_scalar(
          get_render_highquality_camera_list_value(
            camera_point,
            c("long", "lon", "lng", "longitude")
          ),
          value_name = "long",
          arg_name = arg_name,
          caller = caller
        ),
        y = normalize_render_highquality_camera_scalar(
          get_render_highquality_camera_list_value(
            camera_point,
            c("lat", "latitude")
          ),
          value_name = "lat",
          arg_name = arg_name,
          caller = caller
        ),
        altitude = normalize_render_highquality_camera_altitude(
          get_render_highquality_camera_list_value(
            camera_point,
            c("altitude", "alt", "elevation", "z")
          ),
          arg_name = arg_name,
          caller = caller
        ),
        crs = 4326,
        panel = NULL
      ))
    }
    return(normalize_render_highquality_camera_raw_vector(
      camera_point,
      arg_name = arg_name,
      caller = caller
    ))
  }
  if (is.list(camera_point)) {
    location = get_render_highquality_camera_list_value(
      camera_point,
      c("location", "point", "geometry")
    )
    crs = get_render_highquality_camera_list_value(camera_point, "crs")
    panel = get_render_highquality_camera_list_value(camera_point, "panel")
    altitude = get_render_highquality_camera_list_value(
      camera_point,
      c("altitude", "alt", "elevation", "z")
    )
    if (!is.null(location)) {
      return(list(
        type = "location",
        location = location,
        altitude = normalize_render_highquality_camera_altitude(
          altitude,
          arg_name = arg_name,
          caller = caller
        ),
        crs = crs,
        panel = panel
      ))
    }
    if (
      has_render_highquality_camera_list_value(
        camera_point,
        c("lat", "latitude")
      ) &&
        has_render_highquality_camera_list_value(
          camera_point,
          c("long", "lon", "lng", "longitude")
        )
    ) {
      return(list(
        type = "xy",
        x = normalize_render_highquality_camera_scalar(
          get_render_highquality_camera_list_value(
            camera_point,
            c("long", "lon", "lng", "longitude")
          ),
          value_name = "long",
          arg_name = arg_name,
          caller = caller
        ),
        y = normalize_render_highquality_camera_scalar(
          get_render_highquality_camera_list_value(
            camera_point,
            c("lat", "latitude")
          ),
          value_name = "lat",
          arg_name = arg_name,
          caller = caller
        ),
        altitude = normalize_render_highquality_camera_altitude(
          altitude,
          arg_name = arg_name,
          caller = caller
        ),
        crs = if (is.null(crs)) 4326 else crs,
        panel = panel
      ))
    }
    if (
      has_render_highquality_camera_list_value(camera_point, "x") &&
        has_render_highquality_camera_list_value(camera_point, "y") &&
        !is.null(altitude)
    ) {
      return(list(
        type = "xy",
        x = normalize_render_highquality_camera_scalar(
          get_render_highquality_camera_list_value(camera_point, "x"),
          value_name = "x",
          arg_name = arg_name,
          caller = caller
        ),
        y = normalize_render_highquality_camera_scalar(
          get_render_highquality_camera_list_value(camera_point, "y"),
          value_name = "y",
          arg_name = arg_name,
          caller = caller
        ),
        altitude = normalize_render_highquality_camera_altitude(
          altitude,
          arg_name = arg_name,
          caller = caller
        ),
        crs = crs,
        panel = panel
      ))
    }
    if (
      length(camera_point) == 3 &&
        is.null(names(camera_point)) &&
        all(vapply(camera_point, is.numeric, logical(1)))
    ) {
      return(normalize_render_highquality_camera_raw_vector(
        unlist(camera_point, use.names = FALSE),
        arg_name = arg_name,
        caller = caller
      ))
    }
  }
  stop(
    paste0(
      format_render_caller_prefix(caller),
      "`",
      arg_name,
      "` must be a numeric length-3 scene coordinate vector, ",
      "a named `long`/`lat`/`altitude` vector, a spatial POINT, ",
      "or a list with spatial camera fields."
    ),
    call. = FALSE
  )
}

resolve_render_highquality_spatial_camera_altitude = function(
  location,
  altitude = NULL,
  crs = NULL,
  arg_name,
  caller = NULL
) {
  altitude = normalize_render_highquality_camera_altitude(
    altitude,
    arg_name = arg_name,
    caller = caller
  )
  if (!is.null(altitude)) {
    return(altitude)
  }
  point_input = coerce_scene_point_input(
    location = location,
    crs = crs,
    caller = caller
  )
  if (!identical(as.integer(point_input$geometry_count), 1L)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`",
        arg_name,
        "` spatial camera inputs must resolve to exactly one POINT."
      ),
      call. = FALSE
    )
  }
  altitude_columns = intersect(
    c("altitude", "alt", "elevation", "z"),
    names(point_input$sf_data)
  )
  if (length(altitude_columns)) {
    return(normalize_render_highquality_camera_altitude(
      point_input$sf_data[[altitude_columns[1]]][1],
      arg_name = arg_name,
      caller = caller
    ))
  }
  point_coords = sf::st_coordinates(point_input$point_sf_data)
  if ("Z" %in% colnames(point_coords)) {
    return(normalize_render_highquality_camera_altitude(
      point_coords[1, "Z"],
      arg_name = arg_name,
      caller = caller
    ))
  }
  NULL
}

resolve_render_highquality_camera_extent = function(
  heightmap = NULL,
  panel = NULL,
  caller = NULL
) {
  extent = resolve_scene_render_extent(
    heightmap = heightmap,
    panel = panel,
    caller = caller,
    error_if_missing = FALSE
  )
  if (
    is.null(extent) &&
      !is.null(heightmap) &&
      is.null(get_cached_plot_gg_transform_info(
        heightmap = heightmap,
        default = NULL
      ))
  ) {
    extent = c(
      xmin = 1,
      xmax = nrow(heightmap),
      ymin = 1,
      ymax = ncol(heightmap)
    )
  }
  if (is.null(extent)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "Could not determine cached scene extent for spatial camera input."
      ),
      call. = FALSE
    )
  }
  extent
}

resolve_render_highquality_camera_zscale = function() {
  zscale = get_scene_effective_zscale(default = 1)
  zscale = suppressWarnings(as.numeric(zscale)[1])
  if (!is.finite(zscale) || zscale <= 0) {
    return(1)
  }
  zscale
}

resolve_render_highquality_camera_point = function(
  camera_point,
  arg_name,
  bbox_center = c(0, 0, 0),
  caller = NULL
) {
  camera_spec = normalize_render_highquality_camera_input(
    camera_point,
    arg_name = arg_name,
    caller = caller
  )
  if (is.null(camera_spec)) {
    return(NULL)
  }
  if (is.numeric(camera_spec)) {
    return(camera_spec)
  }
  heightmap = resolve_scene_render_heightmap(caller = caller)
  extent = resolve_render_highquality_camera_extent(
    heightmap = heightmap,
    panel = camera_spec$panel,
    caller = caller
  )
  zscale = resolve_render_highquality_camera_zscale()
  if (identical(camera_spec$type, "location")) {
    altitude = resolve_render_highquality_spatial_camera_altitude(
      location = camera_spec$location,
      altitude = camera_spec$altitude,
      crs = camera_spec$crs,
      arg_name = arg_name,
      caller = caller
    )
    point_input = resolve_render_location_input(
      location = camera_spec$location,
      extent = extent,
      heightmap = heightmap,
      panel = camera_spec$panel,
      crs = camera_spec$crs,
      caller = caller
    )
    if (!identical(as.integer(point_input$geometry_count), 1L)) {
      stop(
        paste0(
          format_render_caller_prefix(caller),
          "`",
          arg_name,
          "` spatial camera inputs must resolve to exactly one POINT."
        ),
        call. = FALSE
      )
    }
    if (!is.null(point_input$extent)) {
      extent = point_input$extent
    }
    camera_coords = transform_into_heightmap_coords(
      extent = extent,
      heightmap = heightmap,
      lat = point_input$y[1],
      long = point_input$x[1],
      altitude = altitude,
      offset = 0,
      zscale = zscale,
      panel = point_input$panel,
      transform_scene = FALSE,
      caller = caller
    )
  } else {
    camera_coords = transform_into_heightmap_coords(
      extent = extent,
      heightmap = heightmap,
      lat = camera_spec$y,
      long = camera_spec$x,
      altitude = camera_spec$altitude,
      offset = 0,
      zscale = zscale,
      crs = camera_spec$crs,
      panel = camera_spec$panel,
      caller = caller
    )
  }
  camera_coords = as.numeric(camera_coords[1, ])
  if (!is.null(bbox_center)) {
    bbox_center = suppressWarnings(as.numeric(bbox_center)[1:3])
    if (length(bbox_center) >= 2 && is.finite(bbox_center[2])) {
      camera_coords[2] = camera_coords[2] - bbox_center[2]
    }
  }
  camera_coords
}

validate_render_highquality_rgl_materials = function(rgl_materials) {
  if (is.null(rgl_materials)) {
    return(list())
  }
  if (!is.list(rgl_materials)) {
    stop("`rgl_materials` must be a named list.", call. = FALSE)
  }
  if (length(rgl_materials) == 0) {
    return(rgl_materials)
  }
  material_names = names(rgl_materials)
  if (
    is.null(material_names) ||
      any(is.na(material_names)) ||
      any(!nzchar(material_names))
  ) {
    stop(
      "`rgl_materials` must be a named list with non-empty names.",
      call. = FALSE
    )
  }
  for (material_index in seq_along(rgl_materials)) {
    validate_render_highquality_rgl_material_spec(
      rgl_materials[[material_index]],
      material_names[[material_index]]
    )
  }
  rgl_materials
}

validate_render_highquality_rgl_material_spec = function(material_spec, name) {
  if (
    is_rayrender_material(material_spec) || inherits(material_spec, "function")
  ) {
    return(invisible(TRUE))
  }
  if (is.list(material_spec) && !is.null(material_spec$material)) {
    if (
      !is_rayrender_material(material_spec$material) &&
        !inherits(material_spec$material, "function")
    ) {
      stop(
        sprintf(
          "`rgl_materials[[\"%s\"]]$material` must be a rayrender material or material function.",
          name
        ),
        call. = FALSE
      )
    }
    material_args = material_spec$args
    if (is.null(material_args)) {
      material_args = material_spec$material_args
    }
    if (!is.null(material_args) && !is.list(material_args)) {
      stop(
        sprintf(
          "`rgl_materials[[\"%s\"]]$args` must be a list.",
          name
        ),
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }
  stop(
    sprintf(
      "`rgl_materials[[\"%s\"]]` must be a rayrender material, material function, or list with a `material` entry.",
      name
    ),
    call. = FALSE
  )
}

resolve_render_highquality_rgl_material = function(
  rgl_materials,
  id,
  tag,
  color = NULL
) {
  if (length(rgl_materials) == 0) {
    return(NULL)
  }
  material_names = names(rgl_materials)
  id_name = as.character(id)
  material_index = match(id_name, material_names, nomatch = 0)
  if (material_index == 0) {
    material_index = match(tag, material_names, nomatch = 0)
  }
  if (material_index == 0) {
    return(NULL)
  }
  make_render_highquality_rgl_material(
    material_spec = rgl_materials[[material_index]],
    color = color,
    name = material_names[[material_index]]
  )
}

validate_render_highquality_water_roughness = function(water_roughness) {
  water_roughness = suppressWarnings(as.numeric(water_roughness))
  if (
    length(water_roughness) != 1 ||
      !is.finite(water_roughness) ||
      water_roughness < 0
  ) {
    stop(
      "`water_roughness` must be a single non-negative number.",
      call. = FALSE
    )
  }
  water_roughness
}

get_render_highquality_rgl_material_info = function(rgl_materials) {
  if (length(rgl_materials) == 0) {
    return(data.frame(id = integer(), tag = character()))
  }
  id_info = get_ids_with_labels()
  material_names = names(rgl_materials)
  matches = rep(FALSE, nrow(id_info))
  for (material_name in material_names) {
    matches = matches |
      as.character(id_info$id) == material_name |
      id_info$tag == material_name
  }
  id_info[matches, c("id", "tag"), drop = FALSE]
}

get_render_highquality_raymesh_material_info = function(rgl_material_info) {
  if (nrow(rgl_material_info) == 0) {
    return(rgl_material_info)
  }
  raymesh_index = vapply(
    rgl_material_info$tag,
    is_render_highquality_raymesh_tag,
    logical(1)
  )
  rgl_material_info[raymesh_index, , drop = FALSE]
}

add_render_highquality_water_material_info = function(
  raymesh_material_info,
  water_material,
  rgl_material_info
) {
  if (!identical(water_material, "microfacet")) {
    return(raymesh_material_info)
  }
  water_info = get_ids_with_labels(typeval = "water")
  if (nrow(water_info) == 0) {
    return(raymesh_material_info)
  }
  water_info = water_info[, c("id", "tag"), drop = FALSE]
  if (nrow(rgl_material_info) > 0) {
    water_info = water_info[
      !(water_info$id %in% rgl_material_info$id),
      ,
      drop = FALSE
    ]
  }
  if (nrow(water_info) == 0) {
    return(raymesh_material_info)
  }
  if (nrow(raymesh_material_info) == 0) {
    row.names(water_info) = NULL
    return(water_info)
  }
  raymesh_material_info = unique(rbind(raymesh_material_info, water_info))
  row.names(raymesh_material_info) = NULL
  raymesh_material_info
}

has_render_highquality_dielectric_path_override = function(
  rgl_materials,
  rgl_material_info
) {
  if (nrow(rgl_material_info) == 0) {
    return(FALSE)
  }
  path_override_info = rgl_material_info[
    vapply(
      rgl_material_info$tag,
      is_render_highquality_path_tag,
      logical(1)
    ),
    ,
    drop = FALSE
  ]
  if (nrow(path_override_info) == 0) {
    return(FALSE)
  }
  for (material_row in seq_len(nrow(path_override_info))) {
    material = resolve_render_highquality_rgl_material(
      rgl_materials = rgl_materials,
      id = path_override_info$id[[material_row]],
      tag = path_override_info$tag[[material_row]],
      color = c(1, 1, 1)
    )
    if (
      !is.null(material) &&
        material[[1]]$type == rayrender::dielectric()[[1]]$type
    ) {
      return(TRUE)
    }
  }
  FALSE
}

is_render_highquality_path_tag = function(tag) {
  tag %in%
    c(
      "path3d",
      "contour3d",
      "water_path",
      "road_path",
      "zaxis_axis",
      "zaxis_ticks"
    )
}

should_render_highquality_screen_line = function(line_render, tag) {
  if (tag %in% c("water_path", "road_path")) {
    return(FALSE)
  }
  if (identical(line_render, "screen")) {
    return(TRUE)
  }
  if (identical(line_render, "world")) {
    return(FALSE)
  }
  tag %in% c("textline", "zaxis_axis", "zaxis_ticks")
}

#' Split render_highquality path vertices
#'
#' @param vertices Path vertex matrix, optionally separated by `NA` rows.
#'
#' @return List of path vertex matrices.
#' @keywords internal
split_render_highquality_path_vertices = function(vertices) {
  vertices = as.matrix(vertices)
  if (nrow(vertices) == 0) {
    return(list())
  }
  na_rows = rowSums(is.na(vertices)) > 0
  groups = cumsum(na_rows)
  vertex_indices = split(seq_len(nrow(vertices)), groups)
  out = vector("list", length(vertex_indices))
  for (index in seq_along(vertex_indices)) {
    path_vertices = vertices[vertex_indices[[index]], , drop = FALSE]
    if (index > 1L && nrow(path_vertices) > 0) {
      path_vertices = path_vertices[-1L, , drop = FALSE]
    }
    out[[index]] = path_vertices
  }
  out[vapply(out, nrow, integer(1)) > 0L]
}

transform_render_highquality_screen_points = function(points, bbox_center) {
  points = as.matrix(points)
  if (ncol(points) != 3) {
    points = matrix(points, ncol = 3)
  }
  points = sweep(points, 2, bbox_center, FUN = "-")
  points[, 1] = -points[, 1]
  points[, 3] = -points[, 3]
  points
}

select_render_highquality_color = function(color, index) {
  if (is.character(color)) {
    color = color[!is.na(color)]
    if (length(color) == 0) {
      return("black")
    }
    return(color[min(index, length(color))])
  }
  color = as.matrix(color)
  if (nrow(color) == 0) {
    return(c(0, 0, 0, 1))
  }
  color_index = min(index, nrow(color))
  color_value = color[color_index, , drop = TRUE]
  if (length(color_value) < 4) {
    color_value = c(color_value[1:3], 1)
  }
  color_value
}

select_render_highquality_alpha = function(color) {
  if (is.character(color)) {
    return(1)
  }
  if (length(color) >= 4 && is.finite(color[4])) {
    return(color[4])
  }
  1
}

format_render_highquality_screen_color = function(color) {
  if (is.character(color)) {
    return(color[1])
  }
  color = suppressWarnings(as.numeric(color))
  if (length(color) >= 3 && all(is.finite(color[1:3]))) {
    return(grDevices::rgb(color[1], color[2], color[3]))
  }
  "black"
}

get_render_highquality_screen_text_just = function(
  adj,
  index,
  label_index = 1
) {
  convert_just = function(value) {
    if (index == 2 && value < 0) {
      return(0.5 - value)
    }
    value
  }
  if (is.null(adj)) {
    return(ifelse(index == 1, 0.5, 0.5))
  }
  if (is.matrix(adj) || is.data.frame(adj)) {
    adj = as.matrix(adj)
    adj_row = min(label_index, nrow(adj))
    if (ncol(adj) >= index) {
      just_value = suppressWarnings(as.numeric(adj[adj_row, index]))
      if (is.finite(just_value)) {
        return(convert_just(just_value))
      }
    }
  }
  adj = as.numeric(adj)
  if (length(adj) < index || !is.finite(adj[index])) {
    return(0.5)
  }
  convert_just(adj[index])
}

get_render_highquality_text_cex = function(cex, index) {
  if (is.null(cex)) {
    return(1)
  }
  cex = suppressWarnings(as.numeric(cex))
  if (!length(cex)) {
    return(1)
  }
  cex_value = cex[min(index, length(cex))]
  if (!is.finite(cex_value) || cex_value <= 0) {
    return(1)
  }
  cex_value
}

normalize_render_highquality_occlusion_mode = function(mode, type) {
  mode = tolower(as.character(mode)[1])
  if (is.na(mode) || !nzchar(mode)) {
    stop(
      sprintf("`%s_occlusion_mode` must be a non-empty string.", type),
      call. = FALSE
    )
  }
  if (identical(type, "text")) {
    if (!mode %in% c("anchor", "label", "partial")) {
      stop(
        "`text_occlusion_mode` must be one of: \"anchor\", \"label\", or \"partial\".",
        call. = FALSE
      )
    }
    if (identical(mode, "partial")) {
      return("label")
    }
    return(mode)
  }
  if (!mode %in% c("anchor", "line", "partial")) {
    stop(
      "`line_occlusion_mode` must be one of: \"anchor\", \"line\", or \"partial\".",
      call. = FALSE
    )
  }
  if (identical(mode, "partial")) {
    return("line")
  }
  mode
}

validate_render_highquality_screen_args = function(args, arg_name) {
  if (is.null(args)) {
    return(list())
  }
  if (!is.list(args)) {
    stop(sprintf("`%s` must be a named list.", arg_name), call. = FALSE)
  }
  if (length(args) == 0) {
    return(args)
  }
  arg_names = names(args)
  if (
    is.null(arg_names) ||
      any(is.na(arg_names)) ||
      any(!nzchar(arg_names))
  ) {
    stop(sprintf("`%s` must be a named list.", arg_name), call. = FALSE)
  }
  args
}

validate_render_highquality_screen_arg_names = function(args, fun, arg_name) {
  if (length(args) == 0) {
    return(invisible(TRUE))
  }
  unknown_args = setdiff(names(args), names(formals(fun)))
  if (length(unknown_args) > 0) {
    stop(
      sprintf(
        "`%s` contains unsupported argument%s: %s.",
        arg_name,
        ifelse(length(unknown_args) == 1, "", "s"),
        paste(sprintf("`%s`", unknown_args), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

make_render_highquality_screen_text = function(screen_text_args, ...) {
  do.call(
    rayrender::screen_text,
    utils::modifyList(list(...), screen_text_args)
  )
}

make_render_highquality_screen_line = function(screen_line_args, ...) {
  do.call(
    rayrender::screen_line,
    utils::modifyList(list(...), screen_line_args)
  )
}

is_render_highquality_raymesh_tag = function(tag) {
  grepl("^surface", tag) ||
    grepl("obj", tag, fixed = TRUE) ||
    tag %in%
      c(
        "base",
        "basebottom",
        "water",
        "north_symbol",
        "arrow_symbol",
        "bevel_symbol",
        "background_symbol",
        "scalebar_col1",
        "scalebar_col2",
        "polygon3d",
        "floating_overlay",
        "floating_overlay_tris",
        "base_soil1",
        "base_soil2"
      )
}

get_render_highquality_rgl_material_color = function(id) {
  material = rgl::material3d(id = id)
  color = material$color
  if (is.null(color) || length(color) == 0 || any(is.na(color))) {
    return(NULL)
  }
  color
}

make_render_highquality_water_microfacet_material = function(
  color,
  water_roughness,
  water_ior,
  water_attenuation,
  water_surface_color
) {
  if (!water_surface_color || is.null(color) || length(color) == 0) {
    color = "white"
  }
  if (is.character(color)) {
    color = color[[1]]
  }
  rayrender::microfacet(
    color = color,
    roughness = water_roughness,
    transmission = TRUE,
    eta = water_ior,
    kappa = water_attenuation
  )
}

make_render_highquality_rgl_material = function(
  material_spec,
  color = NULL,
  name = NULL
) {
  if (is_rayrender_material(material_spec)) {
    return(material_spec)
  }
  material = material_spec
  material_args = list()
  if (is.list(material_spec) && !is.null(material_spec$material)) {
    material = material_spec$material
    material_args = material_spec$args
    if (is.null(material_args)) {
      material_args = material_spec$material_args
    }
    if (is.null(material_args)) {
      material_args = list()
    }
  }
  if (is_rayrender_material(material)) {
    return(material)
  }
  if (!inherits(material, "function")) {
    stop(
      sprintf(
        "`rgl_materials[[\"%s\"]]` must resolve to a rayrender material or material function.",
        name
      ),
      call. = FALSE
    )
  }
  if (!is.null(color)) {
    material_formals = names(formals(material))
    if ("color" %in% material_formals && !("color" %in% names(material_args))) {
      material_args$color = color
    }
  }
  material_value = do.call(material, material_args)
  if (!is_rayrender_material(material_value)) {
    stop(
      sprintf(
        "`rgl_materials[[\"%s\"]]` did not return a rayrender material.",
        name
      ),
      call. = FALSE
    )
  }
  material_value
}

is_rayrender_material = function(x) {
  inherits(x, c("ray_material", "ray_mat"))
}

is_empty_raymesh_scene = function(ray_scene) {
  is.null(ray_scene$shapes) || length(ray_scene$shapes) == 0
}
