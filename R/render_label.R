#'@title Render Label
#'
#'@description Adds a marker and label to the current 3D plot
#'
#'Cache fallback messages are disabled by default. Set `options(rayshader.verbose_scene_cache = TRUE)` to print when cached metadata is reused.
#'
#'@param location Default `NULL`. Spatial input used to place the rendered label(s) in the scene. Accepts `sf`, `sfc`, `sfg`, or `sp` POINT, MULTIPOINT, POLYGON, or MULTIPOLYGON geometries. POLYGON and MULTIPOLYGON inputs are converted to label points with `sf::st_centroid()`. MULTIPOINT inputs are flattened to point placements internally, and vectorized arguments such as `text`, `z`, `altitude`, `data_column_text`, and `data_column_z` values are applied against that flattened point count. If the input carries a CRS, it will be transformed automatically into the active scene CRS. If it has no CRS, supply `crs`.
#'@param text Default `NULL`. The label text. If omitted, use `data_column_text` to read label text from `location`.
#'@param y Default `NULL`. Y coordinate for the label in the same coordinate reference system as `extent`.
#'If no `extent` is available and the scene uses a plain matrix heightmap, this defaults to matrix dimensions.
#'@param x Default `NULL`. X coordinate for the label in the same coordinate reference system as `extent`.
#'If no `extent` is available and the scene uses a plain matrix heightmap, this defaults to matrix dimensions.
#'@param z Default `NULL`. Elevation of the label, in units of the elevation matrix (scaled by zscale).
#'@param altitude Default `NULL`. Elevation of the label, in units of the elevation matrix (scaled by zscale). If none is passed, this will default to 10 percent above the maximum altitude in the heightmap.
#'@param data_column_z Default `NULL`. Column name in `location` to use for `z`. Requires `location` to be an `sf`/spatial object with values coercible to numeric. Values are applied after polygon centroid conversion and POINT/MULTIPOINT flattening, rows with missing or non-finite values after coercion are omitted, and retained values are multiplied by `scale_data`.
#'@param data_column_text Default `NULL`. Column name in `location` to use for `text`. Requires `location` to be an `sf`/spatial object with the named column. Values are applied after polygon centroid conversion and POINT/MULTIPOINT flattening.
#'@param scale_data Default `1`. If specifying `data_column_z`, how much to scale that value when rendering. If used with `vertical_exaggeration`, both are applied.
#'@param extent Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object.
#' If omitted, rayshader will use extent metadata cached by [plot_3d()] or [plot_gg()]. If no extent metadata
#' is available for a plain matrix scene, rayshader defaults to `c(xmin = 1, xmax = nrow(heightmap), ymin = 1, ymax = ncol(heightmap))`.
#'@param panel Default `NULL`. Facet panel identifier for scenes created with [plot_gg()]. Required
#'to disambiguate faceted ggplot scenes when panel-specific cached metadata is needed. Ignored
#'for non-ggplot scenes.
#'@param zscale Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units
#'@param vertical_exaggeration Default `1`. Multiplier applied to the effective visual relief. If omitted, rayshader uses the cached scene value from [plot_3d()] or [plot_gg()] when available; pass explicitly to override for this call.
#'@param relativez Default `TRUE`. Whether `z` should be measured in relation to the underlying elevation at that point in the heightmap, or set absolutely (`FALSE`).
#'@param offset Elevation above the surface (at the label point) to start drawing the line.
#'@param clear_previous Default `FALSE`. If `TRUE`, it will clear all existing text and lines rendered with [render_label()]. If no
#'other arguments are passed to [render_label()], this will just remove all existing lines.
#'@param textsize Default `1`. A numeric character expansion value.
#'@param line Default `TRUE`. If `FALSE`, the vertical line connecting the label to the surface is not drawn.
#'@param dashed Default `FALSE`. If `TRUE`, the label line is dashed.
#'@param dashlength Default `auto`. Length, in units of the elevation matrix (scaled by `zscale`) of the dashes if `dashed = TRUE`.
#'@param linewidth Default `3`. The line width.
#'@param antialias Default `FALSE`. If `TRUE`, the line with be have anti-aliasing applied. NOTE: anti-aliasing can cause some unpredictable behavior with transparent surfaces.
#'@param alpha Default `1`. Transparency of the label line.
#'@param textalpha Default `1`. Transparency of the label text.
#'@param freetype Default `TRUE`. Set to `FALSE` if freetype is not installed (freetype enables anti-aliased fonts). NOTE: There are occasionally transparency issues when positioning Freetype fonts in front and behind a transparent surface.
#'@param adjustvec Default `c(0.5,-0.5)`. The horizontal and vertical offset for the text. If `freetype = FALSE` and on macOS/Linux, this is adjusted to `c(0.33,-0.5)` to keep the type centered.
#'@param family Default `"sans"`. Font family. Choices are `c("serif", "sans", "mono", "symbol")`.
#'@param fonttype Default `"standard"`. The font type. Choices are `c("standard", "bold", "italic", "bolditalic")`. NOTE: These require FreeType fonts, which may not be installed on your system. See the documentation for rgl::text3d() for more information.
#'@param linecolor Default `black`. Color of the line. Use `"height"` to color label lines by the cached [plot_gg()] height aesthetic palette.
#'@param textcolor Default `black`. Color of the text. Use `"height"` to color label text by the cached [plot_gg()] height aesthetic palette.
#'@param lat Default `NULL`. Alias for `y` for geographic workflows.
#'@param long Default `NULL`. Alias for `x` for geographic workflows.
#'@param crs Default `NULL`. CRS of the input numeric x/y coordinates, or CRS to assign to CRS-less spatial data before transforming it into the active scene CRS. If spatial data already carries a CRS, that CRS is used automatically.
#'@param filter_to_extent Default `TRUE`. If `TRUE`, labels outside the scene extent are omitted. For scenes created with [plot_gg()], filtering uses the ggplot panel extent rather than the full rendered 3D ggplot extent.
#'@param heightmap Default `NULL`. Height matrix for the current scene. If omitted, this is taken from the cached scene set by [plot_3d()] or [plot_gg()]. Pass explicitly to override the cached value.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4,water=TRUE, watercolor="#233aa1",
#'          zoom=0.9, windowsize = 800)
#'render_snapshot()
#'
#'santa_cruz = c(36.962957, -122.021033)
#'#We want to add a label to Santa Cruz, so we use latitude/longitude coordinates.
#'render_label(lat = santa_cruz[1], long = santa_cruz[2],
#'             textsize = 2, altitude=12000, text = "Santa Cruz")
#'render_snapshot()
#'
#'monterey = c(36.603053, -121.892933)
#'#We can also change the linetype to dashed by setting `dashed = TRUE` (additional options allow
#'#the user to control the dash length). You can clear the existing lines by setting
#'#`clear_previous = TRUE`.
#'render_label(lat = monterey[1], long = monterey[2], altitude = 10000,
#'             textsize = 2, text = "Monterey", textcolor = "white", linecolor="darkred",
#'             dashed = TRUE, clear_previous = TRUE)
#'render_snapshot()
#'
#'canyon = c(36.621049, -122.333912)
#'#By default, z specifies the altitude above that point on the elevation matrix. We can also specify
#'#an absolute height by setting `relativez=FALSE`.
#'render_label(lat = canyon[1], long = canyon[2], altitude = 2000,
#'             textsize = 2, text = "Monterey Canyon", relativez=FALSE)
#'render_snapshot()
#'
#'#We can also render labels in high quality with `render_highquality()`, specifying a custom
#'#line radius. By default, the labels point towards the camera, but you can fix their angle with
#'#argument `text_angle`.
#'render_camera(theta=35, phi = 35, zoom = 0.80, fov=60)
#'render_label(lat = monterey[1], long = monterey[2], altitude = 10000,
#'             textsize = 2, text = "Monterey", textcolor = "black", linecolor="darkred",
#'             dashed = TRUE, clear_previous = TRUE)
#'
#'render_label(lat = canyon[1], long = canyon[2],
#'             altitude = 2000, textsize = 2,
#'             textcolor = "black", linecolor="black",
#'             text = "Monterey Canyon", relativez=FALSE)
#'
#'render_highquality(samples = 16)
#'#We can remove all existing labels by calling `render_label(clear_previous = TRUE)`
#'render_label(clear_previous = TRUE)
#'render_snapshot()
render_label = function(
  location = NULL,
  text = NULL,
  y = NULL,
  x = NULL,
  z = NULL,
  altitude = NULL,
  data_column_z = NULL,
  data_column_text = NULL,
  scale_data = 1,
  extent = NULL,
  panel = NULL,
  zscale = 1,
  vertical_exaggeration = 1,
  relativez = TRUE,
  offset = 0,
  clear_previous = FALSE,
  textsize = 1,
  line = TRUE,
  dashed = FALSE,
  dashlength = "auto",
  linewidth = 3,
  antialias = FALSE,
  alpha = 1,
  textalpha = 1,
  freetype = TRUE,
  adjustvec = NULL,
  family = "sans",
  fonttype = "standard",
  linecolor = "black",
  textcolor = "black",
  lat = NULL,
  long = NULL,
  crs = NULL,
  filter_to_extent = TRUE,
  heightmap = NULL
) {
  validate_filter_to_extent(filter_to_extent, caller = "render_label")
  warn_scale_data_with_vertical_exaggeration(
    scale_data_missing = missing(scale_data),
    vertical_exaggeration_missing = missing(vertical_exaggeration),
    caller = "render_label"
  )
  exit_early = FALSE
  if (clear_previous) {
    rgl::pop3d(tag = c("textline", "raytext"))
    if (missing(text) && is.null(data_column_text)) {
      exit_early = TRUE
    }
  }
  if (!exit_early) {
    zscale = resolve_scene_render_effective_zscale(
      zscale = zscale,
      zscale_missing = missing(zscale),
      vertical_exaggeration = vertical_exaggeration,
      vertical_exaggeration_missing = missing(vertical_exaggeration),
      caller = "render_label"
    )
    heightmap = resolve_scene_render_heightmap(
      heightmap,
      caller = "render_label"
    )
    if (is.null(heightmap)) {
      stop(
        "No heightmap found. Call `plot_3d()` or `plot_gg()` first, or pass `heightmap` explicitly."
      )
    }
    z_supplied = !missing(z) && !is.null(z)
    altitude_supplied = !missing(altitude) && !is.null(altitude)
    text_supplied = !missing(text) && !is.null(text)
    if (!is.null(altitude)) {
      z = altitude
    }
    extent = resolve_scene_render_extent(
      extent = extent,
      heightmap = heightmap,
      caller = "render_label",
      panel = panel,
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
    label_location = prepare_render_label_location(
      location = location,
      caller = "render_label"
    )
    point_input = resolve_render_location_input(
      location = label_location,
      x = x,
      y = y,
      long = long,
      lat = lat,
      missing_x = missing(x),
      missing_y = missing(y),
      missing_long = missing(long),
      missing_lat = missing(lat),
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      crs = crs,
      caller = "render_label"
    )
    x = point_input$x
    y = point_input$y
    input_crs = if (is.null(crs)) point_input$source_crs else crs
    if (!is.null(point_input$extent)) {
      extent = point_input$extent
    }
    if (!is.null(data_column_z)) {
      z = resolve_render_label_z_column(
        location = label_location,
        point_input = point_input,
        data_column_z = data_column_z,
        z_supplied = z_supplied,
        altitude_supplied = altitude_supplied,
        scale_data = scale_data,
        crs = crs,
        caller = "render_label"
      )
    }
    if (!is.null(data_column_text)) {
      text = resolve_render_label_text_column(
        location = label_location,
        point_input = point_input,
        data_column_text = data_column_text,
        text_supplied = text_supplied,
        crs = crs,
        caller = "render_label"
      )
    }
    if (!is.null(data_column_z)) {
      data_column_z_keep = is.finite(z)
      n_label_before_data_drop = length(x)
      if (
        length(data_column_z_keep) == n_label_before_data_drop &&
          !all(data_column_z_keep)
      ) {
        x = x[data_column_z_keep]
        y = y[data_column_z_keep]
        z = subset_render_arg(
          z,
          data_column_z_keep,
          n_label_before_data_drop
        )
        text = subset_render_arg(
          text,
          data_column_z_keep,
          n_label_before_data_drop
        )
        offset = subset_render_arg(
          offset,
          data_column_z_keep,
          n_label_before_data_drop
        )
        textsize = subset_render_arg(
          textsize,
          data_column_z_keep,
          n_label_before_data_drop
        )
        line = subset_render_arg(
          line,
          data_column_z_keep,
          n_label_before_data_drop
        )
        dashed = subset_render_arg(
          dashed,
          data_column_z_keep,
          n_label_before_data_drop
        )
        dashlength = subset_render_arg(
          dashlength,
          data_column_z_keep,
          n_label_before_data_drop
        )
        linewidth = subset_render_arg(
          linewidth,
          data_column_z_keep,
          n_label_before_data_drop
        )
        alpha = subset_render_arg(
          alpha,
          data_column_z_keep,
          n_label_before_data_drop
        )
        textalpha = subset_render_arg(
          textalpha,
          data_column_z_keep,
          n_label_before_data_drop
        )
        linecolor = subset_render_color_arg(
          linecolor,
          data_column_z_keep,
          n_label_before_data_drop
        )
        textcolor = subset_render_color_arg(
          textcolor,
          data_column_z_keep,
          n_label_before_data_drop
        )
      }
    }
    if (is.null(z)) {
      z = max(heightmap, na.rm = TRUE) * 1.1
    }
    label_zaxis_raw = if (
      !is.null(data_column_z) && !identical(scale_data, 0)
    ) {
      z / scale_data
    } else {
      z
    }
    label_zaxis_label = if (!is.null(data_column_z)) data_column_z else "label"
    if (is.null(text)) {
      stop(
        paste0(
          format_render_caller_prefix("render_label"),
          "Must provide `text` or `data_column_text`."
        ),
        call. = FALSE
      )
    }
    if (is.null(x) || is.null(y)) {
      stop("Must provide `x`/`y` coordinates.", call. = FALSE)
    }
    if (!point_input$location_supplied) {
      scene_xy = auto_transform_scene_xy(
        x = x,
        y = y,
        extent = extent,
        heightmap = heightmap,
        panel = panel,
        crs = input_crs,
        caller = "render_label"
      )
      x = scene_xy$x
      y = scene_xy$y
      if (!is.null(scene_xy$extent)) {
        extent = scene_xy$extent
      }
    }
    n_label_before_filter = length(x)
    filtered_label = filter_scene_xy_to_extent(
      x = x,
      y = y,
      extent = extent,
      heightmap = heightmap,
      panel = panel,
      filter_to_extent = filter_to_extent,
      caller = "render_label"
    )
    x = filtered_label$x
    y = filtered_label$y
    if (length(filtered_label$keep) == n_label_before_filter) {
      z = subset_render_arg(z, filtered_label$keep, n_label_before_filter)
      label_zaxis_raw = subset_render_arg(
        label_zaxis_raw,
        filtered_label$keep,
        n_label_before_filter
      )
      text = subset_render_arg(text, filtered_label$keep, n_label_before_filter)
      offset = subset_render_arg(
        offset,
        filtered_label$keep,
        n_label_before_filter
      )
      textsize = subset_render_arg(
        textsize,
        filtered_label$keep,
        n_label_before_filter
      )
      line = subset_render_arg(line, filtered_label$keep, n_label_before_filter)
      dashed = subset_render_arg(
        dashed,
        filtered_label$keep,
        n_label_before_filter
      )
      dashlength = subset_render_arg(
        dashlength,
        filtered_label$keep,
        n_label_before_filter
      )
      linewidth = subset_render_arg(
        linewidth,
        filtered_label$keep,
        n_label_before_filter
      )
      alpha = subset_render_arg(
        alpha,
        filtered_label$keep,
        n_label_before_filter
      )
      textalpha = subset_render_arg(
        textalpha,
        filtered_label$keep,
        n_label_before_filter
      )
      linecolor = subset_render_color_arg(
        linecolor,
        filtered_label$keep,
        n_label_before_filter
      )
      textcolor = subset_render_color_arg(
        textcolor,
        filtered_label$keep,
        n_label_before_filter
      )
    }
    if (!length(x) || !length(y)) {
      return(invisible(NULL))
    }
    if (length(x) != length(y)) {
      stop(
        paste0(
          format_render_caller_prefix("render_label"),
          "`x` and `y` must resolve to the same number of points."
        ),
        call. = FALSE
      )
    }
    linecolor = resolve_ggplot_height_palette_color(
      color = linecolor,
      values = label_zaxis_raw,
      heightmap = heightmap,
      caller = "render_label",
      arg_name = "linecolor"
    )
    textcolor = resolve_ggplot_height_palette_color(
      color = textcolor,
      values = label_zaxis_raw,
      heightmap = heightmap,
      caller = "render_label",
      arg_name = "textcolor"
    )
    label_height_transform = get_scene_height_transform(
      heightmap = heightmap,
      extent = extent
    )
    if (!is.null(label_height_transform)) {
      label_height_reference = suppressWarnings(as.numeric(
        label_height_transform$height_range
      ))
      label_height_reference =
        label_height_reference[is.finite(label_height_reference)]
      if (length(unique(label_height_reference)) <= 1) {
        label_height_reference = z
      }
      z = map_scene_altitudes(
        z,
        height_transform = label_height_transform,
        reference_values = label_height_reference
      )
    }
    n_label = length(x)
    validate_render_label_vector_arg(text, "text", n_label)
    validate_render_label_vector_arg(z, "z", n_label)
    validate_render_label_vector_arg(offset, "offset", n_label)
    validate_render_label_vector_arg(textsize, "textsize", n_label)
    validate_render_label_vector_arg(line, "line", n_label)
    validate_render_label_vector_arg(dashed, "dashed", n_label)
    validate_render_label_vector_arg(dashlength, "dashlength", n_label)
    validate_render_label_vector_arg(linewidth, "linewidth", n_label)
    validate_render_label_vector_arg(alpha, "alpha", n_label)
    validate_render_label_vector_arg(textalpha, "textalpha", n_label)
    validate_render_label_vector_arg(
      linecolor,
      "linecolor",
      n_label,
      color = TRUE
    )
    validate_render_label_vector_arg(
      textcolor,
      "textcolor",
      n_label,
      color = TRUE
    )
    if (rgl::cur3d() == 0) {
      stop("No rgl window currently open.")
    }
    if (.Platform$OS.type == "unix") {
      windows = FALSE
    } else {
      windows = TRUE
    }
    fontlist = list("standard" = 1, "bold" = 2, "italic" = 3, "bolditalic" = 4)
    fonttype = fontlist[[fonttype]]
    e = get_extent(extent)
    nrow_map = nrow(heightmap) - 1
    ncol_map = ncol(heightmap) - 1
    label_scene_altitude = z
    if (isTRUE(relativez)) {
      label_surface_altitude = tryCatch(
        transform_into_heightmap_coords(
          extent = extent,
          heightmap = heightmap,
          lat = y,
          long = x,
          altitude = NULL,
          offset = 0,
          zscale = 1,
          panel = panel,
          transform_scene = FALSE,
          caller = "render_label"
        )[, 2],
        error = function(e) rep(NA_real_, length(z))
      )
      label_scene_altitude = z +
        ifelse(
          is.finite(label_surface_altitude),
          label_surface_altitude,
          0
        )
    }
    cache_altitude_zaxis_data(
      source = "label",
      altitude = label_zaxis_raw,
      scene_altitude = label_scene_altitude,
      label = label_zaxis_label
    )
    ignoreex = par3d()$ignoreExtent
    par3d(ignoreExtent = TRUE)
    on.exit(par3d(ignoreExtent = ignoreex), add = TRUE)
    if (freetype) {
      seriflist = c(
        "fonts/FreeSerif.ttf",
        "fonts/FreeSerifBold.ttf",
        "fonts/FreeSerifItalic.ttf",
        "fonts/FreeSerifBoldItalic.ttf"
      )
      sanslist = c(
        "fonts/FreeSans.ttf",
        "fonts/FreeSansBold.ttf",
        "fonts/FreeSansOblique.ttf",
        "fonts/FreeSansBoldOblique.ttf"
      )
      monolist = c(
        "fonts/FreeMono.ttf",
        "fonts/FreeMonoBold.ttf",
        "fonts/FreeMonoOblique.ttf",
        "fonts/FreeMonoBoldOblique.ttf"
      )
      symbollist = c(
        "fonts/ESSTIX10.TTF",
        "fonts/ESSTIX12.TTF",
        "fonts/ESSTIX9_.TTF",
        "fonts/ESSTIX11.TTF"
      )
      seriflist2 = unlist(lapply(seriflist, system.file, package = "rgl"))
      sanslist2 = unlist(lapply(sanslist, system.file, package = "rgl"))
      monolist2 = unlist(lapply(monolist, system.file, package = "rgl"))
      symbollist2 = unlist(lapply(symbollist, system.file, package = "rgl"))
      rglFonts(
        serif = seriflist2,
        sans = sanslist2,
        mono = monolist2,
        symbol = symbollist2
      )
      warningstring = " "
      if (family == "serif") {
        if (nchar(seriflist2[[fonttype]]) == 0) {
          family = "bitmap"
          if (fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          freetype = FALSE
          if (!windows) {
            textsize = 1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          warning(paste0(
            seriflist[[fonttype]],
            " not found. Turning freetype off",
            warningstring,
            windowsstring
          ))
          fonttype = 1
        }
      }
      if (family == "sans") {
        if (nchar(sanslist2[[fonttype]]) == 0) {
          family = "bitmap"
          if (fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          if (!windows) {
            textsize = 1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          freetype = FALSE
          warning(paste0(
            sanslist[[fonttype]],
            " not found. Turning freetype off",
            warningstring,
            windowsstring
          ))
          fonttype = 1
        }
      }
      if (family == "mono") {
        if (nchar(monolist2[[fonttype]]) == 0) {
          family = "bitmap"
          if (fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          if (!windows) {
            textsize = 1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          freetype = FALSE
          warning(paste0(
            monolist[[fonttype]],
            " not found. Turning freetype off",
            warningstring,
            windowsstring
          ))
          fonttype = 1
        }
      }
      if (family == "symbol") {
        if (nchar(symbollist2[[fonttype]]) == 0) {
          family = "bitmap"
          if (fonttype != 1) {
            warningstring = ", setting fonttype to \"standard\", "
          }
          if (!windows) {
            textsize = 1
            windowsstring = "and setting textsize to 1."
          } else {
            windowsstring = "."
          }
          freetype = FALSE
          warning(paste0(
            symbollist[[fonttype]],
            " not found. Turning freetype off",
            warningstring,
            windowsstring
          ))
          fonttype = 1
        }
      }
    } else {
      warningstring = ""
      family = "bitmap"
      if (fonttype != 1) {
        warningstring = " and fonttype to \"standard\""
        fonttype = 1
      }
      freetype = FALSE
      if (any(textsize != 1) && !windows) {
        warning(
          "Bitmap fonts do not support variable text sizes--setting textsize back to 1",
          warningstring,
          "."
        )
        textsize = 1
      }
    }
    if (is.null(adjustvec)) {
      if (freetype || windows) {
        adjustvec = c(0.5, -0.5)
      } else {
        adjustvec = c(0.33, -0.5)
      }
    }
    for (label_index in seq_len(n_label)) {
      render_single_label(
        label_index = label_index,
        x = x,
        y = y,
        z = z,
        text = text,
        offset = offset,
        line = line,
        heightmap = heightmap,
        extent = e,
        nrow_map = nrow_map,
        ncol_map = ncol_map,
        zscale = zscale,
        relativez = relativez,
        dashed = dashed,
        dashlength = dashlength,
        linewidth = linewidth,
        antialias = antialias,
        alpha = alpha,
        textalpha = textalpha,
        linecolor = linecolor,
        textcolor = textcolor,
        textsize = textsize,
        adjustvec = adjustvec,
        freetype = freetype,
        family = family,
        fonttype = fonttype
      )
    }
  }
  invisible(NULL)
}

prepare_render_label_location = function(location = NULL, caller = NULL) {
  if (is.null(location)) {
    return(NULL)
  }
  if (!inherits(location, c("sf", "sfc", "sfg", "Spatial"))) {
    return(location)
  }
  if (!(length(find.package("sf", quiet = TRUE)) > 0)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`sf` package required for spatial `location` inputs."
      ),
      call. = FALSE
    )
  }
  coerced_input = coerce_scene_sf_input(location)
  sf_data = coerced_input$sf_data
  if (!nrow(sf_data)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`location` cannot be empty."
      ),
      call. = FALSE
    )
  }
  if (any(sf::st_is_empty(sf_data))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`location` cannot contain empty geometries."
      ),
      call. = FALSE
    )
  }
  geometry_types = as.character(sf::st_geometry_type(
    sf_data,
    by_geometry = TRUE
  ))
  allowed_types = c("POINT", "MULTIPOINT", "POLYGON", "MULTIPOLYGON")
  if (any(!geometry_types %in% allowed_types)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`location` must contain only POINT, MULTIPOINT, POLYGON, or MULTIPOLYGON geometries."
      ),
      call. = FALSE
    )
  }
  polygon_index = geometry_types %in% c("POLYGON", "MULTIPOLYGON")
  if (!any(polygon_index)) {
    return(location)
  }
  geometry = sf::st_geometry(sf_data)
  centroid_geometry = suppressWarnings(sf::st_geometry(sf::st_centroid(
    sf_data[polygon_index, , drop = FALSE]
  )))
  polygon_positions = which(polygon_index)
  for (centroid_index in seq_along(polygon_positions)) {
    geometry[[polygon_positions[[centroid_index]]]] = centroid_geometry[[
      centroid_index
    ]]
  }
  sf::st_geometry(sf_data) = sf::st_sfc(
    as.list(geometry),
    crs = sf::st_crs(sf_data)
  )
  rebuild_scene_sf_output(sf_data, coerced_input$input_class)
}

resolve_render_label_z_column = function(
  location,
  point_input = NULL,
  data_column_z = NULL,
  z_supplied = FALSE,
  altitude_supplied = FALSE,
  scale_data = 1,
  crs = NULL,
  caller = NULL
) {
  if (is.null(location)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_z` requires `location`."
      ),
      call. = FALSE
    )
  }
  if (isTRUE(z_supplied) || isTRUE(altitude_supplied)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_z` cannot be combined with `z` or `altitude`."
      ),
      call. = FALSE
    )
  }
  if (
    !is.character(data_column_z) ||
      length(data_column_z) != 1 ||
      !nzchar(trimws(data_column_z))
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_z` must be a single non-empty column name."
      ),
      call. = FALSE
    )
  }
  point_sf_data = point_input$point_sf_data
  if (is.null(point_sf_data)) {
    point_sf_data = coerce_scene_point_input(
      location = location,
      crs = crs,
      caller = caller
    )$point_sf_data
  }
  if (!data_column_z %in% names(point_sf_data)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_z` was not found in `location`: ",
        data_column_z
      ),
      call. = FALSE
    )
  }
  z = point_sf_data[[data_column_z]]
  z = coerce_render_data_column_numeric(
    data_values = z,
    data_column_name = data_column_z,
    data_column_arg = "data_column_z",
    caller = caller
  )
  z * scale_data
}

resolve_render_label_text_column = function(
  location,
  point_input = NULL,
  data_column_text = NULL,
  text_supplied = FALSE,
  crs = NULL,
  caller = NULL
) {
  if (is.null(location)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_text` requires `location`."
      ),
      call. = FALSE
    )
  }
  if (isTRUE(text_supplied)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_text` cannot be combined with `text`."
      ),
      call. = FALSE
    )
  }
  if (
    !is.character(data_column_text) ||
      length(data_column_text) != 1 ||
      !nzchar(trimws(data_column_text))
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_text` must be a single non-empty column name."
      ),
      call. = FALSE
    )
  }
  point_sf_data = point_input$point_sf_data
  if (is.null(point_sf_data)) {
    point_sf_data = coerce_scene_point_input(
      location = location,
      crs = crs,
      caller = caller
    )$point_sf_data
  }
  if (!data_column_text %in% names(point_sf_data)) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_text` was not found in `location`: ",
        data_column_text
      ),
      call. = FALSE
    )
  }
  text = point_sf_data[[data_column_text]]
  if (
    inherits(text, "matrix") || inherits(text, "data.frame") || is.list(text)
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_text` must refer to a vector column."
      ),
      call. = FALSE
    )
  }
  text = as.character(text)
  if (any(is.na(text))) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`data_column_text` cannot contain NA values."
      ),
      call. = FALSE
    )
  }
  text
}

validate_render_label_vector_arg = function(
  value,
  name,
  n_expected,
  color = FALSE,
  caller = "render_label"
) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (isTRUE(color) && is.numeric(value) && length(value) == 3) {
    return(invisible(NULL))
  }
  if (length(value) %in% c(1, n_expected)) {
    return(invisible(NULL))
  }
  stop(
    paste0(
      format_render_caller_prefix(caller),
      "`",
      name,
      "` must have length 1 or match the number of labels."
    ),
    call. = FALSE
  )
}

render_label_arg_value = function(value, index, n_expected, color = FALSE) {
  if (isTRUE(color) && is.numeric(value) && length(value) == 3) {
    return(value)
  }
  if (length(value) == n_expected) {
    return(value[[index]])
  }
  value
}

render_single_label = function(
  label_index,
  x,
  y,
  z,
  text,
  offset,
  line,
  heightmap,
  extent,
  nrow_map,
  ncol_map,
  zscale,
  relativez,
  dashed,
  dashlength,
  linewidth,
  antialias,
  alpha,
  textalpha,
  linecolor,
  textcolor,
  textsize,
  adjustvec,
  freetype,
  family,
  fonttype
) {
  n_label = length(x)
  x = render_label_arg_value(x, label_index, n_label)
  y = render_label_arg_value(y, label_index, n_label)
  z = render_label_arg_value(z, label_index, n_label)
  text = render_label_arg_value(text, label_index, n_label)
  offset = render_label_arg_value(offset, label_index, n_label)
  line = render_label_arg_value(line, label_index, n_label)
  dashed = render_label_arg_value(dashed, label_index, n_label)
  dashlength = render_label_arg_value(dashlength, label_index, n_label)
  linewidth = render_label_arg_value(linewidth, label_index, n_label)
  alpha = render_label_arg_value(alpha, label_index, n_label)
  textalpha = render_label_arg_value(textalpha, label_index, n_label)
  linecolor = render_label_arg_value(
    linecolor,
    label_index,
    n_label,
    color = TRUE
  )
  textcolor = render_label_arg_value(
    textcolor,
    label_index,
    n_label,
    color = TRUE
  )
  textsize = render_label_arg_value(textsize, label_index, n_label)
  x_index = (x - extent["xmin"]) /
    (extent["xmax"] - extent["xmin"]) *
    nrow_map +
    1
  y_index = 1 +
    ncol_map -
    (y - extent["ymin"]) / (extent["ymax"] - extent["ymin"]) * ncol_map
  x_index_clamped = x_index
  y_index_clamped = y_index
  x_index_clamped[floor(x_index_clamped) >= nrow(heightmap)] = nrow(heightmap)
  y_index_clamped[floor(y_index_clamped) >= ncol(heightmap)] = ncol(heightmap)
  x_index_clamped[floor(x_index_clamped) < 1] = 1
  y_index_clamped[floor(y_index_clamped) < 1] = 1
  in_bounds = TRUE
  if (
    x_index > nrow(heightmap) ||
      x_index < 1 ||
      y_index < 1 ||
      y_index > ncol(heightmap)
  ) {
    in_bounds = FALSE
  } else {
    if (!length(find.package("rayimage", quiet = TRUE)) > 0) {
      flipped_mat = flipud(t(heightmap))
      surface_altitude = flipped_mat[
        floor(y_index_clamped),
        floor(x_index_clamped)
      ]
    } else {
      surface_altitude = rayimage::interpolate_array(
        t(heightmap),
        x_index_clamped,
        y_index_clamped
      )
    }
    if (is.na(surface_altitude)) {
      in_bounds = FALSE
    }
  }
  startline = 0
  if (!in_bounds) {
    shadow_id = get_ids_with_labels("shadow")$id
    if (length(shadow_id) > 0) {
      shadow_vertices = rgl::rgl.attrib(shadow_id, "vertices")
      startline = min(shadow_vertices[, 2], na.rm = TRUE)
    }
  }
  z = z / zscale
  offset = offset / zscale
  if (in_bounds) {
    startline = surface_altitude / zscale
  }
  if (relativez && in_bounds) {
    z = z + startline
  }
  x = x_index - nrow_map / 2 - 1
  y = y_index - ncol_map / 2 - 1
  if (isTRUE(line)) {
    if (dashlength == "auto") {
      dashlength = (z - startline + offset) / 20
    } else {
      dashlength = as.numeric(dashlength)
    }
    linelist = list()
    if (isTRUE(dashed)) {
      counter = 1
      while (startline + dashlength < z) {
        linelist[[counter]] = matrix(
          c(x, x, startline + dashlength + offset, startline + offset, y, y),
          2,
          3
        )
        startline = startline + dashlength * 2
        counter = counter + 1
      }
      linelist[[counter]] = matrix(
        c(x, x, z + offset, startline + offset, y, y),
        2,
        3
      )
    } else {
      linelist[[1]] = matrix(
        c(x, x, z + offset, startline + offset, y, y),
        2,
        3
      )
    }
    for (i in seq_along(linelist)) {
      rgl::lines3d(
        linelist[[i]],
        color = linecolor,
        lwd = linewidth,
        lit = FALSE,
        line_antialias = antialias,
        depth_test = "less",
        alpha = alpha,
        tag = "textline"
      )
    }
  }
  text3d(
    x,
    z + offset,
    y,
    text,
    color = textcolor,
    adj = adjustvec,
    useFreeType = freetype,
    alpha = textalpha,
    family = family,
    font = fonttype,
    cex = textsize,
    depth_test = "less",
    tag = "raytext",
    lit = FALSE
  )
}

resolve_render_label_text_angle = function(text_angle = NULL, default_angle) {
  if (is.null(text_angle)) {
    return(default_angle)
  }
  if (length(text_angle) == 1) {
    return(c(0, text_angle, 0))
  }
  text_angle
}

resolve_render_label_text_angle_rayrender = function(
  text_angle = NULL,
  phi,
  theta
) {
  resolve_render_label_text_angle(
    text_angle = text_angle,
    default_angle = c(-phi, theta + 180, 0)
  )
}

resolve_render_label_text_angle_rayvertex = function(
  text_angle = NULL,
  theta,
  rotmat
) {
  resolve_render_label_text_angle(
    text_angle = text_angle,
    default_angle = c(rotmat[1], -theta, 0)
  )
}
