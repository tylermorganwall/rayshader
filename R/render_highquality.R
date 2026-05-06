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
#'@param water_ior Default `1`. Water index of refraction.
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
#'@param text_size Default `6`. Height of the text.
#'@param text_offset Default `c(0,0,0)`. Offset to be applied to all text labels.
#'@param line_radius Default `0.5`. Radius of line/path segments.
#'@param smooth_line Default `FALSE`. If `TRUE`, the line will be rendered with a continuous smooth line, rather
#'than straight segments.
#'@param use_extruded_paths Default `TRUE`. If `FALSE`, paths will be generated with the `rayrender::path()` object, instead
#'of `rayrender::extruded_path()`.
#'@param point_radius Default `1`. Radius of 3D points (rendered with [render_points()]). This scales the existing
#'value of size specified in [render_points()].
#'@param scale_text_angle Default `NULL`. Same as `text_angle`, but for the scale bar.
#'@param scale_text_size Default `6`. Height of the scale bar text.
#'@param scale_text_offset Default `c(0,0,0)`. Offset to be applied to all scale bar text labels.
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
#'be derived from the rgl window.
#'@param camera_lookat Default `NULL`. Custom point at which the camera is directed. The `FOV`, `width`, and `height` arguments will still
#'be derived from the rgl window.
#'@param camera_interpolate Default `c(0,0)`. Maximum `1`, minimum `0`. Sets the camera at a point between the `rgl` view and the `camera_location`
#'and `camera_lookat` vectors.
#'@param scene_elements Default `NULL`. Extra scene elements to add to the scene, created with rayrender.
#'@param clear Default `FALSE`. If `TRUE`, the current `rgl` device will be cleared.
#'@param print_scene_info Default `FALSE`. If `TRUE`, it will print the position and lookat point of the camera.
#'@param clamp_value Default `NA`. If `NA`, uses `100` when the OIDN denoiser is unavailable
#'and `10000` when it is available (via `rayrender::has_denoiser()`).
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
#'volcano %>%
#'  sphere_shade() %>%
#'  plot_3d(vertical_exaggeration = 1/2)
#'render_highquality(min_variance = 0, sample_method = "sobol_blue")
#'
#'#Change position of light
#'render_highquality(lightdirection = 45, min_variance = 0, sample_method = "sobol_blue")
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
#'                   min_variance = 0)
#'
#'#Add a red light to the volcano and change the ambient light to dusk
#'render_camera(theta=45,phi=45)
#'render_highquality(lightdirection = c(240), lightaltitude=30,
#'                   lightcolor=c("#5555ff"),
#'                   scene_elements = rayrender::sphere(z=0,y=15, x=-18, radius=5,
#'                                    material=rayrender::light(color="red",intensity=10)),
#'                   min_variance = 0, samples = 16)
#'#Manually change the camera location and direction
#'render_camera(theta=45,phi=45,fov=90)
#'render_highquality(lightdirection = c(240), lightaltitude=30, lightcolor=c("#5555ff"),
#'                   camera_location = c(50,10,10), camera_lookat = c(0,15,0),
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
#'  st_point() |>
#'  st_buffer(0.01) |>
#'  st_sfc(crs = 4326) |>
#'  st_transform(st_crs(washington_monument_multipolygonz))
#'
#'elevation_data = elevatr::get_elev_raster(locations = wm_point, z = 14)
#'
#'scene_bbox = st_bbox(st_buffer(wm_point,300))
#'cropped_data = raster::crop(elevation_data, scene_bbox)
#'
#'#Plot a 3D map of the national mall
#'cropped_data |>
#'  height_shade() |>
#'  add_shadow(lamb_shade(), 0) |>
#'  plot_3d(zscale=3.7, water = TRUE, waterdepth = 1,
#'          soliddepth=-50, windowsize = 800)
#'#Zoom in on the monument
#'render_camera(theta=45,  phi=0, zoom= 0.03, fov=130)
#'#Render the national monument at solar noon on the solstice
#'rgl::par3d(ignoreExtent = TRUE)
#'render_multipolygonz(washington_monument_multipolygonz,
#'                     zscale = 4, color = "grey80")
#' #Render using the built-in (but less accurate) Hosek model.
#' # Here's it's more yellow than it should be, but it's accurate enough for most renders.
#'render_highquality(
#'	min_variance = 0,
#'	samples = 16,
#'	long = -77.035249,
#'	lat = 38.889462,
#'	iso = 8,
#'	clamp_value = 10000,
#'	datetime = as.POSIXct("2025-12-21 16:00:00", tz = "EST")
#')
#'
#'# Render the more-accurate Praguq model (that requires
#'# supplemental data that will be downloaded on the first call) and
#'# specify a higher resolution environment map via `sky_args`
#'render_highquality(
#'	min_variance = 0,
#'	samples = 16,
#'	long = -77.035249,
#'	lat = 38.889462,
#'	sky_args = list(hosek = FALSE,resolution=4000),
#'	iso = 8,
#'	clamp_value = 10000,
#'	datetime = as.POSIXct("2025-12-21 16:00:00", tz = "EST")
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
	water_ior = 1,
	override_material = FALSE,
	cache_scene = FALSE,
	reset_scene_cache = FALSE,
	width = NULL,
	height = NULL,
	ortho_dimensions = NULL,
	text_angle = NULL,
	text_size = 12,
	text_offset = c(0, text_size / 2, 0),
	line_radius = 0.5,
	point_radius = 0.5,
	smooth_line = FALSE,
	use_extruded_paths = FALSE,
	scale_text_angle = NULL,
	scale_text_size = 12,
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
	clamp_value = NA,
	calculate_consistent_normals = FALSE,
	load_normals = TRUE,
	rgl_materials = list(),
	animation_camera_coords = NULL,
	plot = is.na(filename),
	...
) {
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
	if (is.na(clamp_value)) {
		clamp_value = if (rayrender::has_denoiser()) 10000 else 100
	}

	dot_args = list(...)
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
	rgl_material_info = get_render_highquality_rgl_material_info(rgl_materials)
	raymesh_material_info = get_render_highquality_raymesh_material_info(
		rgl_material_info
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
	if (!(length(find.package("rayimage", quiet = TRUE)) > 0)) {
		warning("`rayimage` package required for labels")
		has_rayimage = FALSE
	}
	labelids = get_ids_with_labels(typeval = c("raytext", "zaxis_labels"))$id
	labels = list()
	counter = 1
	for (i in seq_len(length(labelids))) {
		if (!has_rayimage) {
			break
		}
		temp_label = rgl.attrib(labelids[i], "texts")
		temp_center = rgl.attrib(labelids[i], "centers")
		temp_color = rgl.attrib(labelids[i], "colors")
		for (j in seq_len(nrow(temp_label))) {
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
				text_height = text_size,
				font_color = temp_color[j, 1:3]
			)
			counter = counter + 1
		}
	}
	if (length(labels) > 0) {
		all_labels = do.call(rbind, labels)
		scene = rayrender::add_object(scene, all_labels)
	}
	labellineinfo = get_ids_with_labels(typeval = "textline")
	labellineids = labellineinfo$id
	labelline = list()
	counter = 1
	for (i in seq_len(length(labellineids))) {
		if (!has_rayimage) {
			break
		}
		temp_verts = rgl.attrib(labellineids[i], "vertices")
		temp_color = rgl.attrib(labellineids[i], "colors")
		for (j in seq_len(nrow(temp_verts) / 2)) {
			temp_material = resolve_render_highquality_rgl_material(
				rgl_materials = rgl_materials,
				id = labellineids[i],
				tag = labellineinfo$tag[i],
				color = temp_color[j, 1:3]
			)
			if (is.null(temp_material)) {
				temp_material = rayrender::diffuse(color = temp_color[j, 1:3])
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
	pathinfo = get_ids_with_labels(
		typeval = c("path3d", "contour3d", "zaxis_axis", "zaxis_ticks")
	)
	pathids = pathinfo$id
	pathline = list()
	counter = 1
	for (i in seq_len(length(pathids))) {
		temp_verts = rgl.attrib(pathids[i], "vertices")
		temp_verts_split = split(
			as.data.frame(temp_verts),
			cumsum(apply(temp_verts, 1, \(x) any(is.na(x))))
		)
		for (j in seq_along(temp_verts_split)[-1]) {
			temp_verts_split[[j]] = temp_verts_split[[j]][-1, ]
		}
		temp_color = rgl.attrib(pathids[i], "colors")
		temp_lwd = material3d("lwd", id = pathids[i]) * line_radius
		for (j in seq_along(temp_verts_split)) {
			temp_verts_single = temp_verts_split[[j]]
			if (nrow(temp_color) == 1) {
				temp_color = matrix(
					temp_color[1:3],
					byrow = TRUE,
					ncol = 3,
					nrow = nrow(temp_verts_single)
				)
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
				color = temp_color[1, 1:3]
			)
			if (is.null(temp_material)) {
				temp_material = rayrender::diffuse(color = temp_color[1, 1:3])
			}

			if (use_extruded_paths) {
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
	counter = 1
	for (i in seq_len(length(scalelabelids))) {
		if (!has_rayimage) {
			break
		}
		temp_label = rgl.attrib(scalelabelids[i], "texts")
		temp_center = rgl.attrib(scalelabelids[i], "centers")
		temp_color = rgl.attrib(scalelabelids[i], "colors")
		for (j in seq_len(nrow(temp_label))) {
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
				label = temp_label,
				text_height = scale_text_size,
				font_color = temp_color[j, 1:3]
			)
			counter = counter + 1
		}
	}
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
		return(scene)
	}

	if (!is.null(animation_camera_coords)) {
		stopifnot(ncol(animation_camera_coords) == 14)
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
		if (!is.null(sky_file)) {
			animation_args$environment_light = sky_file
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
	if (!is.null(sky_file)) {
		render_scene_args$environment_light = sky_file
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
		rgl::clear3d()
	}
	return(invisible(debug_return))
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
		stop("`rgl_materials` must be a named list with non-empty names.", call. = FALSE)
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
	if (is_rayrender_material(material_spec) || inherits(material_spec, "function")) {
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
		if (!is.null(material) && material[[1]]$type == rayrender::dielectric()[[1]]$type) {
			return(TRUE)
		}
	}
	FALSE
}

is_render_highquality_path_tag = function(tag) {
	tag %in% c("path3d", "contour3d", "zaxis_axis", "zaxis_ticks")
}

is_render_highquality_raymesh_tag = function(tag) {
	grepl("^surface", tag) ||
		grepl("obj", tag, fixed = TRUE) ||
		tag %in% c(
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
