#'@title Calculate Radiance Shade Overlay
#'
#'@description Renders a top-down orthographic radiance pass of either the
#'current hillshade cache, the current 3D rayshader scene, or a supplied
#'elevation matrix.
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'When called with no `heightmap`, this function uses the cached 2D hillshade
#'heightmap and, when available, the cached hillshade/map texture from the most
#'recent rayshader hillshading chain. Set `capture_scene = TRUE` to instead
#'render the currently displayed rayshader 3D scene. When a raw matrix is
#'supplied, rayshader builds a temporary surface and applies `texture` (or a
#'constant `color`, default `"grey50"`).
#'
#'This function uses the same lighting controls as [render_highquality()],
#'including directional lights and auto-sky generation (`lat`, `long`,
#'`datetime`, `sky_sun_elevation`, `sky_sun_azimuth`, `sky_altitude`, and
#'`sky_args`), and returns an RGBA array that can be passed to [add_overlay()].
#'The render runs in an isolated background R session with
#'`RGL_USE_NULL=TRUE` in the subprocess environment, and uses the standard
#'rayshader pipeline:
#'`plot_3d(..., solid = TRUE, shadow = shadow)` followed by
#'`render_highquality(plot = FALSE)`.
#'
#'@param heightmap Default `NULL`. If `NULL`, uses cached 2D hillshade metadata
#'by default. Set `capture_scene = TRUE` to use the current rayshader rgl scene.
#'If a matrix (or supported raster input), renders a top-down radiance pass for
#'that data directly.
#'@param texture Default `NULL`. Texture used only when `heightmap` is supplied.
#'Can be a rayshader-style RGB(A) array, a built-in [sphere_shade()] texture
#'name, a color vector (passed to [height_shade()]), an image filename, or a
#'single color name.
#'@param color Default `"grey50"`. Fallback single-color texture when `heightmap`
#'is supplied and `texture = NULL`.
#'@param zscale Default `1`. Vertical scale when `heightmap` is supplied.
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective
#'visual relief. If omitted while rendering from the active scene, rayshader uses
#'the cached scene value from [plot_3d()] or [plot_gg()] when available; pass
#'explicitly to override for this call. This does not update cached metadata.
#'@param filename Default `NA`. Optional output filename. If supplied, the
#'render is also saved to disk.
#'@param samples Default `128`. Maximum samples per pixel.
#'@param sample_method Default `"sobol_blue"`, unless `samples > 256`, in which
#'case it is switched to `"sobol"`.
#'@param min_variance Default `1e-7`. Adaptive sampler variance threshold.
#'@param shadow Default `TRUE`. Whether to build the temporary top-down scene
#'with the rayshader shadow plane underneath the model. Setting this to `TRUE`
#'can help fill very dark corners in the radiance pass.
#'@param light Default `TRUE`. Whether to add directional lights.
#'@param lat Default `NA`. Latitude used for auto sky generation.
#'@param long Default `NA`. Longitude used for auto sky generation.
#'@param datetime Default `NA`. Datetime used for auto sky generation.
#'@param sky_sun_elevation Default `NA`. If supplied, uses
#'`skymodelr::generate_sky()` and passes this value to its `elevation` argument.
#'@param sky_sun_azimuth Default `NA`. If supplied, uses
#'`skymodelr::generate_sky()` and passes this value to its `azimuth` argument.
#'@param sky_altitude Default `NA`. If supplied, uses `skymodelr::generate_sky()`
#'and passes this value to its `altitude` argument.
#'@param sky_args Default `list()`. Extra arguments passed to
#'`skymodelr::generate_sky_latlong()` (default) or `skymodelr::generate_sky()`
#'when direct sky arguments are used.
#'@param lightdirection Default `315`. Light direction angle(s), in degrees.
#'@param lightaltitude Default `45`. Light altitude angle(s), in degrees.
#'@param lightsize Default `NULL`. Light radius; auto-derived from scene size.
#'@param lightintensity Default `500`. Light intensity value(s).
#'@param lightcolor Default `"white"`. Light color(s).
#'@param material Default `rayrender::diffuse()`. Forwarded to
#'[render_highquality()].
#'@param water_attenuation Default `0`. Forwarded to [render_highquality()].
#'@param water_surface_color Default `TRUE`. Forwarded to [render_highquality()].
#'@param water_ior Default `1.33`. Forwarded to [render_highquality()].
#'@param override_material Default `FALSE`. Forwarded to [render_highquality()].
#'@param cache_scene Default `FALSE`. Forwarded to [render_highquality()].
#'@param reset_scene_cache Default `FALSE`. Forwarded to [render_highquality()].
#'@param width Default `NULL`. Output width in pixels. If omitted, derived from
#'the underlying surface dimensions.
#'@param height Default `NULL`. Output height in pixels. If omitted, derived from
#'the underlying surface dimensions.
#'@param clamp_value Default `1000`. Radiance clamp value. If `NA`, uses `1000`.
#'@param auto_exposure Default `TRUE`. Whether to use rayrender's automatic
#'exposure adjustment in the radiance pass. Forwarded to
#'`rayrender::render_scene()` via [render_highquality()] when supported by the
#'installed rayrender version.
#'@param capture_scene Default `FALSE`. If `TRUE` and `heightmap = NULL`,
#'renders the currently displayed rayshader rgl scene instead of the cached 2D
#'hillshade context.
#'@param scene_elements Default `NULL`. Extra rayrender objects to add.
#'@param plot Default `FALSE`. Whether to preview the rendered image.
#'@param ... Additional parameters passed to [render_highquality()].
#'
#'@return RGBA array representing the top-down radiance render.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'montereybay |>
#'	height_shade(texture = topo.colors(256)) |>
#'	add_overlay(radiance_shade(vertical_exaggeration = 10), 1) |>
#'	plot_map()
#'#Plot using sky args
#'montereybay |>
#'	height_shade(texture = topo.colors(256)) |>
#'	add_overlay(radiance_shade(vertical_exaggeration = 20,
#'                             sky_sun_elevation = 5, sky_sun_azimuth = 315), 1) |>
#'	plot_map()
radiance_shade = function(
  heightmap = NULL,
  texture = NULL,
  color = "grey50",
  zscale = 1,
  vertical_exaggeration = 1,
  filename = NA,
  samples = 128,
  sample_method = "sobol_blue",
  min_variance = 1e-7,
  shadow = TRUE,
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
  override_material = FALSE,
  cache_scene = FALSE,
  reset_scene_cache = FALSE,
  width = NULL,
  height = NULL,
  clamp_value = 1000,
  auto_exposure = TRUE,
  capture_scene = FALSE,
  scene_elements = NULL,
  plot = FALSE,
  ...
) {
  if (samples > 256 && sample_method == "sobol_blue") {
    warning(
      r"{When `sample_method = "sobol_blue"`, `samples` must be less than or equal to 256. Setting `sample_method` to `"sobol"`.}"
    )
    sample_method = "sobol"
  }
  if (!(length(find.package("rayrender", quiet = TRUE)) > 0)) {
    stop("`rayrender` package required for radiance_shade()")
  }
  if (is.na(clamp_value)) {
    clamp_value = 1000
  }
  if (!is.na(filename)) {
    if (dirname(filename) != "." && !dir.exists(dirname(filename))) {
      stop(sprintf(
        "Error: directory '%s' does not exist.",
        dirname(filename)
      ))
    }
  }

  radiance_is_color_string = function(x) {
    if (!is.character(x) || length(x) != 1) {
      return(FALSE)
    }
    !inherits(
      tryCatch(grDevices::col2rgb(x), error = function(e) e),
      "error"
    )
  }

  radiance_matrix_texture = function(heightmap, texture, color, zscale) {
    if (is.null(texture)) {
      return(constant_shade(heightmap, color = color, alpha = 1))
    }
    if (is.array(texture) || is.matrix(texture)) {
      return(rayimage::ray_read_image(texture))
    }
    if (is.character(texture)) {
      if (length(texture) == 1) {
        if (file.exists(texture)) {
          return(rayimage::ray_read_image(texture))
        }
        if (
          texture %in%
            c("imhof1", "imhof2", "imhof3", "imhof4", "desert", "bw", "unicorn")
        ) {
          return(sphere_shade(heightmap, texture = texture, zscale = zscale))
        }
        if (radiance_is_color_string(texture)) {
          return(constant_shade(heightmap, color = texture, alpha = 1))
        }
      }
      if (length(texture) > 1 || radiance_is_color_string(texture[1])) {
        return(height_shade(heightmap, texture = texture))
      }
    }
    stop(
      "Unsupported `texture` for matrix input. Use an RGB(A) array, image path, sphere_shade texture name, a color palette, or a single color."
    )
  }

  radiance_render_scene_subprocess = function(child_args, filename = NA) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop(
        "`callr` package required for isolated radiance rendering. ",
        "Install it with `install.packages('callr')`."
      )
    }

    result_image = tryCatch(
      callr::r(
        func = function(child_args, filename) {
          on.exit(try(rgl::close3d(), silent = TRUE), add = TRUE)

          if (!requireNamespace("rayshader", quietly = TRUE)) {
            stop("`rayshader` package must be available in callr subprocess.")
          }
          if (!requireNamespace("rayimage", quietly = TRUE)) {
            stop("`rayimage` package must be available in callr subprocess.")
          }

          plot_3d_args = list(
            hillshade = child_args$hillshade,
            heightmap = child_args$heightmap,
            zscale = child_args$zscale,
            solid = TRUE,
            shadow = child_args$shadow,
            water = FALSE,
            theta = 0,
            phi = 89.9,
            fov = 0,
            zoom = 1,
            windowsize = c(
              max(64L, as.integer(child_args$width)),
              max(64L, as.integer(child_args$height))
            ),
            plot_new = TRUE,
            close_previous = TRUE,
            clear_previous = TRUE
          )
          do.call(rayshader::plot_3d, plot_3d_args)
          # Use unscaled scene units so orthographic dimensions map directly to matrix axes.
          rgl::par3d(scale = c(1, 1, 1))

          render_args = list(
            filename = filename,
            samples = child_args$samples,
            sample_method = child_args$sample_method,
            min_variance = child_args$min_variance,
            light = child_args$light,
            lat = child_args$lat,
            long = child_args$long,
            datetime = child_args$datetime,
            sky_sun_elevation = child_args$sky_sun_elevation,
            sky_sun_azimuth = child_args$sky_sun_azimuth,
            sky_altitude = child_args$sky_altitude,
            sky_args = child_args$sky_args,
            lightdirection = child_args$lightdirection,
            lightaltitude = child_args$lightaltitude,
            lightsize = child_args$lightsize,
            lightintensity = child_args$lightintensity,
            lightcolor = child_args$lightcolor,
            material = child_args$material,
            water_attenuation = child_args$water_attenuation,
            water_surface_color = child_args$water_surface_color,
            water_ior = child_args$water_ior,
            override_material = child_args$override_material,
            cache_scene = child_args$cache_scene,
            reset_scene_cache = child_args$reset_scene_cache,
            width = child_args$width,
            height = child_args$height,
            clamp_value = child_args$clamp_value,
            scene_elements = child_args$scene_elements,
            plot = FALSE
          )
          if (
            "auto_exposure" %in%
              names(formals(rayrender::render_scene))
          ) {
            render_args$auto_exposure = child_args$auto_exposure
          }
          render_highquality_formals = names(formals(
            rayshader::render_highquality
          ))
          if ("ortho_dimensions" %in% render_highquality_formals) {
            render_args$ortho_dimensions = child_args$ortho_dimensions
          }
          safe_dot_args = child_args$dot_args
          duplicate_args = intersect(names(render_args), names(safe_dot_args))
          if (length(duplicate_args) > 0) {
            safe_dot_args[duplicate_args] = NULL
          }

          output_image = do.call(
            rayshader::render_highquality,
            c(render_args, safe_dot_args)
          )
          output_image
        },
        args = list(child_args = child_args, filename = filename),
        libpath = .libPaths(),
        env = c(callr::rcmd_safe_env(), RGL_USE_NULL = "TRUE")
      ),
      error = function(e) {
        structure(
          list(
            message = conditionMessage(e),
            call = if (!is.null(conditionCall(e))) {
              paste(deparse(conditionCall(e)), collapse = " ")
            } else {
              NA_character_
            }
          ),
          class = "radiance_subprocess_error"
        )
      }
    )
    if (inherits(result_image, "radiance_subprocess_error")) {
      stop(
        "radiance_shade(): subprocess render failed: ",
        result_image$message,
        if (!is.na(result_image$call)) {
          paste0(" [", result_image$call, "]")
        } else {
          ""
        }
      )
    }
    result_image
  }

  dot_args = list(...)
  if (!("camera_up" %in% names(dot_args))) {
    dot_args$camera_up = c(0, 0, 1)
  }
  heightmap_supplied = !is.null(heightmap)
  zscale_missing = missing(zscale)

  if (heightmap_supplied) {
    heightmap_info = coerce_plot_3d_heightmap(heightmap)
    heightmap = heightmap_info$heightmap
    if (!is.matrix(heightmap)) {
      stop("`heightmap` must be a matrix or a supported spatial raster input.")
    }
    if (
      zscale_missing &&
        is.finite(heightmap_info$zscale) &&
        heightmap_info$zscale > 0
    ) {
      zscale = heightmap_info$zscale
    }
    zscale = suppressWarnings(as.numeric(zscale)[1])
    if (!is.finite(zscale) || zscale <= 0) {
      stop("`zscale` must be a positive number when `heightmap` is supplied.")
    }
    zscale = apply_vertical_exaggeration(
      zscale = zscale,
      vertical_exaggeration = vertical_exaggeration,
      caller = "radiance_shade"
    )
    hillshade = radiance_matrix_texture(
      heightmap = heightmap,
      texture = texture,
      color = color,
      zscale = zscale
    )
  } else {
    if (isTRUE(capture_scene)) {
      if (rgl::cur3d() == 0) {
        stop(
          "No rgl window currently open. ",
          "Build a 3D scene first, or call radiance_shade() with `capture_scene = FALSE` to use cached hillshade metadata."
        )
      }
      surface_id = get_ids_with_labels(typeval = c("surface", "surface_tris"))
      if (nrow(surface_id) == 0) {
        stop("No rayshader surface found in the current rgl scene.")
      }

      heightmap = resolve_scene_render_heightmap(
        heightmap = NULL,
        caller = "radiance_shade"
      )
      if (is.null(heightmap) || !is.matrix(heightmap)) {
        stop(
          "No cached `heightmap` found for the active scene. ",
          "Call plot_3d()/plot_gg() first, or pass `heightmap` explicitly."
        )
      }
      zscale = resolve_scene_render_effective_zscale(
        zscale = zscale,
        zscale_missing = zscale_missing,
        vertical_exaggeration = vertical_exaggeration,
        vertical_exaggeration_missing = missing(vertical_exaggeration),
        caller = "radiance_shade"
      )

      surface_texture = as.character(surface_id$texture_file[1])
      has_surface_texture =
        !is.na(surface_texture) &&
        nzchar(surface_texture) &&
        file.exists(surface_texture)
      if (has_surface_texture) {
        hillshade = rayimage::ray_read_image(surface_texture)
      } else {
        warning(
          "Unable to find the active surface texture file in the current scene; using `texture`/`color` fallback."
        )
        hillshade = radiance_matrix_texture(
          heightmap = heightmap,
          texture = texture,
          color = color,
          zscale = zscale
        )
      }
    } else {
      hillshade_heightmap = get_hillshade_heightmap(default = NULL)
      if (is.null(hillshade_heightmap) || !is.matrix(hillshade_heightmap)) {
        if (rgl::cur3d() == 0) {
          stop(
            "No rgl window currently open and no `heightmap` supplied. ",
            "Build a 2D hillshade with rayshader, pass `heightmap`, ",
            "or open a 3D scene and set `capture_scene = TRUE`."
          )
        }
        stop(
          "No `heightmap` supplied and no cached hillshade metadata found. ",
          "Build a 2D hillshade with rayshader, pass `heightmap`, or set `capture_scene = TRUE` to render the current rgl scene."
        )
      }
      emit_scene_cache_message(
        caller = "radiance_shade",
        argument_name = "heightmap",
        cache_name = "hillshade_heightmap",
        cache_label = get_hillshade_heightmap_label(default = NULL)
      )
      resolved_heightmap = list(
        heightmap = hillshade_heightmap,
        source = "hillshade",
        label = get_hillshade_heightmap_label(default = NULL)
      )
      heightmap = resolved_heightmap$heightmap
      zscale = resolve_hillshade_zscale(
        zscale = zscale,
        zscale_missing = zscale_missing,
        caller = "radiance_shade",
        allow_scene_cache = FALSE
      )
      zscale = zscale$zscale
      zscale = apply_vertical_exaggeration(
        zscale = zscale,
        vertical_exaggeration = vertical_exaggeration,
        caller = "radiance_shade"
      )

      if (is.null(texture)) {
        hillshade = get_hillshade_map(default = NULL)
        if (!is.null(hillshade)) {
          emit_scene_cache_message(
            caller = "radiance_shade",
            argument_name = "texture",
            cache_name = "hillshade_map",
            cache_label = get_hillshade_map_label(default = NULL)
          )
          hillshade = rayimage::ray_read_image(hillshade)
        } else {
          hillshade = radiance_matrix_texture(
            heightmap = heightmap,
            texture = NULL,
            color = color,
            zscale = zscale
          )
        }
      } else {
        hillshade = radiance_matrix_texture(
          heightmap = heightmap,
          texture = texture,
          color = color,
          zscale = zscale
        )
      }
    }
  }

  # Keep output in rayshader hillshade/map orientation (ncol x nrow) without
  # post-reorient: rayrender returns arrays as [height, width, channels], so use
  # width <- nrow(heightmap), height <- ncol(heightmap).
  default_width = nrow(heightmap)
  default_height = ncol(heightmap)
  if (!is.finite(default_width) || default_width <= 0) {
    default_width = 1000
  }
  if (!is.finite(default_height) || default_height <= 0) {
    default_height = default_width
  }

  if (is.null(width) && is.null(height)) {
    width = default_width
    height = default_height
  } else if (is.null(width)) {
    width = as.integer(round(height * default_width / default_height))
  } else if (is.null(height)) {
    height = as.integer(round(width * default_height / default_width))
  }
  width = max(as.integer(round(width)), 1L)
  height = max(as.integer(round(height)), 1L)
  if ("ortho_dimensions" %in% names(dot_args)) {
    ortho_dimensions_value = dot_args$ortho_dimensions
    dot_args$ortho_dimensions = NULL
  } else {
    # Match rayshader surface axes for top-down orthographic renders:
    # x <- matrix rows, z <- matrix cols.
    ortho_dimensions_value = c(nrow(heightmap), ncol(heightmap))
  }
  if (!("camera_location" %in% names(dot_args))) {
    height_range = suppressWarnings(range(heightmap / zscale, na.rm = TRUE))
    if (!all(is.finite(height_range))) {
      height_range = c(0, 1)
    }
    scene_span = max(nrow(heightmap), ncol(heightmap), 1)
    camera_distance = scene_span * 5 + max(diff(height_range), 1)
    dot_args$camera_location = c(0, camera_distance, 0)
  }
  if (!("camera_lookat" %in% names(dot_args))) {
    dot_args$camera_lookat = c(0, 0, 0)
  }
  if (!("camera_interpolate" %in% names(dot_args))) {
    dot_args$camera_interpolate = c(1, 1)
  }

  child_args = list(
    hillshade = hillshade,
    heightmap = heightmap,
    zscale = zscale,
    samples = samples,
    sample_method = sample_method,
    min_variance = min_variance,
    shadow = shadow,
    light = light,
    lat = lat,
    long = long,
    datetime = datetime,
    sky_sun_elevation = sky_sun_elevation,
    sky_sun_azimuth = sky_sun_azimuth,
    sky_altitude = sky_altitude,
    sky_args = sky_args,
    lightdirection = lightdirection,
    lightaltitude = lightaltitude,
    lightsize = lightsize,
    lightintensity = lightintensity,
    lightcolor = lightcolor,
    material = material,
    water_attenuation = water_attenuation,
    water_surface_color = water_surface_color,
    water_ior = water_ior,
    override_material = override_material,
    cache_scene = cache_scene,
    reset_scene_cache = reset_scene_cache,
    width = width,
    height = height,
    ortho_dimensions = ortho_dimensions_value,
    clamp_value = clamp_value,
    auto_exposure = auto_exposure,
    scene_elements = scene_elements,
    dot_args = dot_args
  )

  output_image = radiance_render_scene_subprocess(
    child_args = child_args,
    filename = filename
  )
  if (isTRUE(plot)) {
    rayimage::plot_image(output_image, new_page = TRUE)
  }
  return(output_image)
}
