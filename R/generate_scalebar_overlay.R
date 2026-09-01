#'@title Generate Scalebar Overlay
#'
#'@description This function creates an overlay with a scale bar. By default,
#'the scale bar spans approximately half of the longer map dimension and uses
#'pretty breaks and units derived from the cached spatial metadata.
#'It uses the coordinates of the map (specified by passing an extent)
#'and then creates a scale bar at a specified x/y proportion across the map. If the map is not projected
#'(i.e. is in lat/long coordinates) this function will use the `geosphere` package to create a
#'scale bar of the proper length.
#'
#'@param extent Default `NULL`. Either an object representing the spatial extent of the scene
#' (either from the `raster`, `terra`, `sf`, or `sp` packages),
#' a length-4 numeric vector specifying `c("xmin", "xmax","ymin","ymax")`, or the spatial object (from
#' the previously aforementioned packages) which will be automatically converted to an extent object. If this is in
#' lat/long coordinates, rayshader will infer that from the spatial metadata when possible. If omitted, rayshader will infer the extent
#' from `heightmap` when possible, otherwise from the active scene or cached hillshade metadata.
#'@param length Default `NULL`. Length of the scale bar in `unit`. If omitted,
#'rayshader derives a pretty length approximately half of the longer map side.
#'@param x Default `0.05`. The x-coordinate of the bottom-left corner of the scale bar, as a proportion of the full map width.
#'@param y Default `0.05`. The y-coordinate of the bottom-left corner of the scale bar, as a proportion of the full map height.
#'@param latlong Default `NA`. If `NA`, rayshader infers whether the map is in lat/long coordinates from
#'spatial metadata on `extent`, `heightmap`, the active scene, or cached hillshade data. Explicit values are only
#'used as a fallback for matrix heightmaps without cached spatial metadata.
#'@param thickness Default `NA`, automatically computed as 1/20th the length of the scale bar. Width of the scale bar.
#'@param bearing Default `NULL`. Direction measured clockwise from grid north.
#'If omitted, the scale bar follows the longer map axis.
#'@param unit Default `NULL`. Distance unit for `length` and the displayed label.
#'If omitted, rayshader uses metres or kilometres for longitude/latitude scenes,
#'the CRS linear units for projected scenes, and no suffix for raw matrices.
#'@param labels Default `NA`. Manually specify the three labels with a length-3 character vector.
#'Use this if you want display units other than meters.
#'@param flip_ticks Default `FALSE`. Whether to flip the ticks to the other side of the scale bar.
#'@param text_size Default `1`. Text size.
#'@param decimals Default `0`. Number of decimal places for scale bar labels.
#'@param text_offset Default `1`. Amount of offset to apply to the text from the scale bar, as a multiple of
#'`thickness`.
#'@param adj Default `0.5`, centered. Text justification. `0` is left-justified, and `1` is right-justified.
#'@param heightmap Default `NULL`. The original height map. Pass this in to extract the dimensions of the resulting
#'RGB image array automatically.
#'@param width Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param height Default `NA`. Width of the resulting image array. Default the same dimensions as height map.
#'@param resolution_multiply Default `1`. If passing in `heightmap` instead of width/height, amount to
#'increase the resolution of the overlay, which should make lines/polygons/text finer.
#'Should be combined with \code{\link[=add_overlay]{add_overlay()}} with `rescale_original = TRUE` to ensure those added details are captured
#'in the final map.
#'@param color1 Default `black`. Primary color of the scale bar.
#'@param color2 Default `white`. Secondary color of the scale bar.
#'@param text_color Default `black`. Text color.
#'@param font Default `1`. An integer which specifies which font to use for text.
#'If possible, device drivers arrange so that 1 corresponds to plain text (the default),
#'2 to bold face, 3 to italic and 4 to bold italic.
#'@param border_color Default `black`. Border color of the scale bar.
#'@param tick_color Default `black`. Tick color of the scale bar.
#'@param border_width Default `1`. Width of the scale bar border.
#'@param tick_width Default `1`. Width of the tick.
#'@param halo_color Default `NA`, no halo. If a color is specified, the text label will be surrounded by a halo
#'of this color.
#'@param halo_expand Default `1`. Number of pixels to expand the halo.
#'@param halo_alpha Default `1`. Transparency of the halo.
#'@param halo_offset Default `c(0,0)`. Horizontal and vertical offset to apply to the halo, as a proportion of the full scene.
#'@param halo_blur Default `0`. Amount of blur to apply to the halo. Values greater than `30` won't result in further blurring.
#'@param halo_edge_softness Default `0.1`. Width of the softened halo edge transition, in pixels.
#'@param halo_gap_fill Default `2`. Maximum alpha gap width, in pixels, to bridge in the halo outline.
#'@param halo_gap_fill_alpha_threshold Default `0.25`. Alpha threshold used to protect enclosed interior halo gaps from `halo_gap_fill`.
#'@return Semi-transparent overlay with a scale bar.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'#Create the water palette
#'water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
#'bathy_hs = height_shade(montereybay_spatial, texture = water_palette)
#'#Set scalebar font
#'par(family = "Arial")
#'
#'#Generate flat water heightmap
#'mbay = montereybay_spatial
#'mbay[mbay < 0] = 0
#'
#'base_map = mbay |>
#' height_shade() |>
#' add_overlay(generate_altitude_overlay(bathy_hs, montereybay_spatial, 0, 0))  |>
#' add_shadow(lamb_shade(vertical_exaggeration = 4),0.3)
#'
#'#Add a scalebar
#'base_map |>
#' add_overlay(generate_scalebar_overlay(length = 40000,
#'                                       latlong=TRUE)) |>
#' plot_map()
#'#Change the text color
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 40000,
#'                                        text_color = "white",
#'                                        latlong=TRUE)) |>
#'  plot_map()
#'#Change the length
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 30000,
#'                                        text_color = "white",
#'                                        latlong=TRUE)) |>
#'  plot_map()
#'#Change the thickness (default is length/20)
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 30000,
#'                                        text_color = "white", thickness = 30000/10,
#'                                        latlong=TRUE)) |>
#'  plot_map()
#'#Change the text offset (given in multiples of thickness)
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 30000,
#'                                        text_color = "white", thickness = 30000/10,
#'                                        text_offset = 0.75,
#'                                        latlong=TRUE)) |>
#'  plot_map()
#'#Change the primary and secondary colors, along with the border and tick color
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 30000,
#'                                        text_color = "white", border_color = "white",
#'                                        tick_color = "white",
#'                                        color1 = "darkolivegreen4", color2 = "burlywood3",
#'                                        latlong=TRUE)) |>
#'  plot_map()
#'#Add a halo
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 40000,
#'                                        halo_color = "white", halo_expand = 1,
#'                                        font = 2,
#'                                        latlong=TRUE)) |>
#'  plot_map()
#'#Change the orientation, position, text alignment, and flip the ticks to the other side
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 40000, x = 0.07,
#'                                        bearing=0, adj = 0, flip_ticks = TRUE,
#'                                        halo_color = "white", halo_expand = 1.5,
#'                                        font = 2,
#'                                        latlong=TRUE)) |>
#'  plot_map()
#'#64373.8 meters in 40 miles
#'#Create custom labels, change font and text size, remove the border/ticks, and change the color
#'#Here, we specify a width and height to double the resolution of the image (for sharper text)
#'base_map |>
#'  add_overlay(generate_scalebar_overlay(length = 64373.8, x = 0.07,
#'                                        labels = c("0", "20", "40 miles"), thickness=2500,
#'                                        text_size=3, font = 2, text_offset = 0,
#'                                        text_color="white", color2="#bf323b", border_color=NA,
#'                                        tick_color="red", tick_width=0,
#'                                        bearing=0, adj = 0, flip_ticks = TRUE,
#'                                        halo_color="black", halo_blur=3, halo_alpha=0.5,
#'                                        width = ncol(montereybay_spatial)*2,
#'                                        height = nrow(montereybay_spatial)*2,
#'                                        latlong=TRUE), rescale_original=TRUE) |>
#'  plot_map()
generate_scalebar_overlay = function(
  extent = NULL,
  length = NULL,
  x = 0.05,
  y = 0.05,
  latlong = NA,
  thickness = NA,
  bearing = NULL,
  unit = NULL,
  flip_ticks = FALSE,
  labels = NA,
  text_size = 1,
  decimals = 0,
  text_offset = 1,
  adj = 0.5,
  heightmap = NULL,
  width = NA,
  height = NA,
  resolution_multiply = 1,
  color1 = "white",
  color2 = "black",
  text_color = "black",
  font = 1,
  border_color = "black",
  tick_color = "black",
  border_width = 1,
  tick_width = 1,
  halo_color = NA,
  halo_expand = 1,
  halo_alpha = 1,
  halo_offset = c(0, 0),
  halo_blur = 0,
  halo_edge_softness = 0.1,
  halo_gap_fill = 2,
  halo_gap_fill_alpha_threshold = 0.25
) {
  length_missing = missing(length) || is.null(length)
  bearing_missing = missing(bearing) || is.null(bearing)
  unit_missing = missing(unit) || is.null(unit)
  loc = rep(0, 2)
  heightmap = resolve_overlay_heightmap(
    heightmap = heightmap,
    heightmap_missing = missing(heightmap),
    width = width,
    height = height,
    caller = "generate_scalebar_overlay"
  )
  latlong = resolve_scalebar_overlay_latlong(
    latlong = latlong,
    extent = extent,
    heightmap = heightmap,
    caller = "generate_scalebar_overlay"
  )
  extent = resolve_scene_render_extent(
    extent = extent,
    heightmap = heightmap,
    caller = "generate_scalebar_overlay"
  )
  scene_info = resolve_scalebar_overlay_scene_info(
    heightmap = heightmap,
    extent = extent,
    caller = "generate_scalebar_overlay"
  )
  scalebar_specification = resolve_scalebar_overlay_specification(
    length = length,
    length_missing = length_missing,
    unit = unit,
    unit_missing = unit_missing,
    bearing = bearing,
    bearing_missing = bearing_missing,
    latlong = latlong,
    thickness = thickness,
    scene_info = scene_info
  )
  length = scalebar_specification$geometry_length
  display_length = scalebar_specification$display_length
  unit = scalebar_specification$unit
  bearing = scalebar_specification$bearing
  thickness = scalebar_specification$geometry_thickness
  extent = get_extent(extent)
  xdiff = extent["xmax"] - extent["xmin"]
  ydiff = extent["ymax"] - extent["ymin"]

  loc[1] = x * xdiff + extent["xmin"]
  loc[2] = y * ydiff + extent["ymin"]

  halo_offset[1] = halo_offset[1] * xdiff
  halo_offset[2] = halo_offset[2] * ydiff

  if (!(length(find.package("ragg", quiet = TRUE)) > 0)) {
    png_device = grDevices::png
  } else {
    png_device = ragg::agg_png
  }
  if (is.na(height)) {
    height = ncol(heightmap)
  }
  if (is.na(width)) {
    width = nrow(heightmap)
  }
  og_height = height
  og_width = width
  height = height * resolution_multiply
  width = width * resolution_multiply
  text_size = text_size * resolution_multiply
  border_width = border_width * resolution_multiply
  tick_width = tick_width * resolution_multiply
  halo_expand = halo_expand * resolution_multiply
  halo_blur = halo_blur * resolution_multiply
  halo_edge_softness = halo_edge_softness * resolution_multiply
  halo_gap_fill = halo_gap_fill * resolution_multiply

  if (all(!is.na(labels)) && length(labels) != 3) {
    stop("If specified, `labels` must be length-3 vector")
  }

  poly_list = list()
  line_list = list()
  text_list = list()

  if (latlong) {
    if (!(length(find.package("geosphere", quiet = TRUE)) > 0)) {
      stop(
        "{geosphere} package required for generate_scalebar_overlay() using lat/long coordinates"
      )
    }
    length_val = length / 4
    for (i in 1:4) {
      temppoly = matrix(0, nrow = 4, ncol = 2)
      templine = matrix(0, nrow = 2, ncol = 2)
      if (i == 1) {
        temppoly[1, ] = c(loc[1], loc[2])
        temppoly[2, ] = geosphere::destPoint(
          c(loc[1], loc[2]),
          b = bearing,
          d = length_val
        )
        temppoly[4, ] = geosphere::destPoint(
          c(loc[1], loc[2]),
          b = bearing - 90,
          d = thickness
        )
        temppoly[3, ] = geosphere::destPoint(
          temppoly[4, ],
          b = bearing,
          d = length_val
        )
        if (!flip_ticks) {
          templine[1, ] = temppoly[4, ]
          templine[2, ] = geosphere::destPoint(
            temppoly[4, ],
            b = bearing - 90,
            d = thickness / 2
          )
        } else {
          templine[1, ] = temppoly[1, ]
          templine[2, ] = geosphere::destPoint(
            temppoly[1, ],
            b = bearing + 90,
            d = thickness / 2
          )
        }
      } else {
        temppoly[1, ] = poly_list[[i - 1]][2, ]
        temppoly[2, ] = geosphere::destPoint(
          temppoly[1, ],
          b = bearing,
          d = length_val
        )
        temppoly[4, ] = geosphere::destPoint(
          temppoly[1, ],
          b = bearing - 90,
          d = thickness
        )
        temppoly[3, ] = geosphere::destPoint(
          temppoly[4, ],
          b = bearing,
          d = length_val
        )
        if (!flip_ticks) {
          templine[1, ] = temppoly[4, ]
          templine[2, ] = geosphere::destPoint(
            temppoly[4, ],
            b = bearing - 90,
            d = thickness / 2
          )
        } else {
          templine[1, ] = temppoly[1, ]
          templine[2, ] = geosphere::destPoint(
            temppoly[1, ],
            b = bearing + 90,
            d = thickness / 2
          )
        }
      }
      poly_list[[i]] = temppoly
      line_list[[i]] = templine
    }
    line_list[[5]] = matrix(0, nrow = 2, ncol = 2)

    if (!flip_ticks) {
      line_list[[5]][1, ] = poly_list[[4]][3, ]
      line_list[[5]][2, ] = geosphere::destPoint(
        poly_list[[4]][3, ],
        b = bearing - 90,
        d = thickness / 2
      )

      text_list[[1]] = geosphere::destPoint(
        line_list[[1]][2, ],
        b = bearing - 90,
        d = thickness * text_offset
      )
      text_list[[2]] = geosphere::destPoint(
        line_list[[3]][2, ],
        b = bearing - 90,
        d = thickness * text_offset
      )
      text_list[[3]] = geosphere::destPoint(
        line_list[[5]][2, ],
        b = bearing - 90,
        d = thickness * text_offset
      )
    } else {
      line_list[[5]][1, ] = poly_list[[4]][2, ]
      line_list[[5]][2, ] = geosphere::destPoint(
        poly_list[[4]][2, ],
        b = bearing + 90,
        d = thickness / 2
      )

      text_list[[1]] = geosphere::destPoint(
        line_list[[1]][2, ],
        b = bearing + 90,
        d = thickness * text_offset
      )
      text_list[[2]] = geosphere::destPoint(
        line_list[[3]][2, ],
        b = bearing + 90,
        d = thickness * text_offset
      )
      text_list[[3]] = geosphere::destPoint(
        line_list[[5]][2, ],
        b = bearing + 90,
        d = thickness * text_offset
      )
    }
  } else {
    length_val = length / 4
    dir = c(sinpi(bearing / 180), cospi(bearing / 180))
    dir2 = c(sinpi(bearing / 180 - 1 / 2), cospi(bearing / 180 - 1 / 2))
    dir3 = c(sinpi(bearing / 180 + 1 / 2), cospi(bearing / 180 + 1 / 2))

    for (i in 1:4) {
      temppoly = matrix(0, nrow = 4, ncol = 2)
      templine = matrix(0, nrow = 2, ncol = 2)
      if (i == 1) {
        temppoly[1, ] = c(loc[1], loc[2])
        temppoly[2, ] = c(loc[1], loc[2]) + length_val * dir
        temppoly[4, ] = c(loc[1], loc[2]) + thickness * dir2
        temppoly[3, ] = temppoly[4, ] + length_val * dir
        if (!flip_ticks) {
          templine[1, ] = temppoly[4, ]
          templine[2, ] = temppoly[4, ] + thickness / 2 * dir2
        } else {
          templine[1, ] = temppoly[1, ]
          templine[2, ] = temppoly[1, ] + thickness / 2 * dir3
        }
      } else {
        temppoly[1, ] = poly_list[[i - 1]][2, ]
        temppoly[2, ] = temppoly[1, ] + length_val * dir
        temppoly[4, ] = temppoly[1, ] + thickness * dir2
        temppoly[3, ] = temppoly[4, ] + length_val * dir
        if (!flip_ticks) {
          templine[1, ] = temppoly[4, ]
          templine[2, ] = temppoly[4, ] + thickness / 2 * dir2
        } else {
          templine[1, ] = temppoly[1, ]
          templine[2, ] = temppoly[1, ] + thickness / 2 * dir3
        }
      }
      poly_list[[i]] = temppoly
      line_list[[i]] = templine
    }
    line_list[[5]] = matrix(0, nrow = 2, ncol = 2)
    if (!flip_ticks) {
      line_list[[5]][1, ] = poly_list[[4]][3, ]
      line_list[[5]][2, ] = poly_list[[4]][3, ] + thickness / 2 * dir2

      text_list[[1]] = line_list[[1]][2, ] + thickness * text_offset * dir2
      text_list[[2]] = line_list[[3]][2, ] + thickness * text_offset * dir2
      text_list[[3]] = line_list[[5]][2, ] + thickness * text_offset * dir2
    } else {
      line_list[[5]][1, ] = poly_list[[4]][2, ]
      line_list[[5]][2, ] = poly_list[[4]][2, ] + thickness / 2 * dir3

      text_list[[1]] = line_list[[1]][2, ] + thickness * text_offset * dir3
      text_list[[2]] = line_list[[3]][2, ] + thickness * text_offset * dir3
      text_list[[3]] = line_list[[5]][2, ] + thickness * text_offset * dir3
    }
  }
  tempoverlay = tempfile(fileext = ".png")
  png_device(
    filename = tempoverlay,
    width = width,
    height = height,
    units = "px",
    bg = "transparent"
  )
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot(
    x = c(extent["xmin"], extent["ymin"]),
    y = c(extent["xmax"], extent["ymax"]),
    xlim = c(extent["xmin"], extent["xmax"]),
    ylim = c(extent["ymin"], extent["ymax"]),
    pch = 0,
    bty = "n",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i",
    cex = 0,
    col = NA
  )

  cols = rep(c(color1, color2), 2)
  for (i in 1:4) {
    graphics::polygon(
      poly_list[[i]],
      col = cols[i],
      border = border_color,
      lwd = border_width
    )
  }
  for (i in 1:5) {
    graphics::segments(
      line_list[[i]][1, 1],
      line_list[[i]][1, 2],
      line_list[[i]][2, 1],
      line_list[[i]][2, 2],
      col = tick_color,
      lwd = tick_width
    )
  }

  if (all(is.na(labels)) || length(labels) != 3) {
    format_string = paste0(c("%0.", decimals, "f"), collapse = "")
    labels = paste0(
      c(sprintf(format_string, c(0, display_length / 2, display_length))),
      c("", "", unit)
    )
  }

  graphics::text(
    text_list[[1]][1],
    text_list[[1]][2],
    labels = labels[1],
    adj = adj,
    cex = text_size,
    col = text_color,
    font = font
  )
  graphics::text(
    text_list[[2]][1],
    text_list[[2]][2],
    labels = labels[2],
    adj = adj,
    cex = text_size,
    col = text_color,
    font = font
  )
  graphics::text(
    text_list[[3]][1],
    text_list[[3]][2],
    labels = labels[3],
    adj = adj,
    cex = text_size,
    col = text_color,
    font = font
  )

  grDevices::dev.off() #resets par
  overlay_temp = rayimage::ray_read_image(tempoverlay)
  if (!is.na(halo_color)) {
    if (!(length(find.package("rayimage", quiet = TRUE)) > 0)) {
      stop("{rayimage} package required for `halo_color`")
    }
    tempoverlay = tempfile(fileext = ".png")
    png_device(
      filename = tempoverlay,
      width = width,
      height = height,
      units = "px",
      bg = "transparent"
    )
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot(
      x = c(extent["xmin"], extent["ymin"]),
      y = c(extent["xmax"], extent["ymax"]),
      xlim = c(extent["xmin"], extent["xmax"]),
      ylim = c(extent["ymin"], extent["ymax"]),
      pch = 0,
      bty = "n",
      axes = FALSE,
      xaxs = "i",
      yaxs = "i",
      cex = 0,
      col = NA
    )

    cols = rep(c(color1, color2), 2)
    offset_mat = matrix(halo_offset, nrow = 4, ncol = 2, byrow = TRUE)
    for (i in 1:4) {
      graphics::polygon(
        poly_list[[i]] + offset_mat,
        col = cols[i],
        border = border_color,
        lwd = border_width
      )
    }
    for (i in 1:5) {
      graphics::segments(
        line_list[[i]][1, 1] + halo_offset[1],
        line_list[[i]][1, 2] + halo_offset[2],
        line_list[[i]][2, 1] + halo_offset[1],
        line_list[[i]][2, 2] + halo_offset[2],
        col = tick_color,
        lwd = tick_width
      )
    }

    graphics::text(
      text_list[[1]][1] + halo_offset[1],
      text_list[[1]][2] + halo_offset[2],
      labels = labels[1],
      adj = adj,
      cex = text_size,
      col = text_color,
      font = font
    )
    graphics::text(
      text_list[[2]][1] + halo_offset[1],
      text_list[[2]][2] + halo_offset[2],
      labels = labels[2],
      adj = adj,
      cex = text_size,
      col = text_color,
      font = font
    )
    graphics::text(
      text_list[[3]][1] + halo_offset[1],
      text_list[[3]][2] + halo_offset[2],
      labels = labels[3],
      adj = adj,
      cex = text_size,
      col = text_color,
      font = font
    )

    grDevices::dev.off() #resets par
    overlay_temp_under = rayimage::ray_read_image(tempoverlay)
    overlay_temp_under = generate_halo_underlay(
      overlay_temp_under,
      halo_expand,
      halo_offset,
      halo_color,
      halo_alpha,
      halo_blur,
      halo_edge_softness,
      halo_gap_fill,
      halo_gap_fill_alpha_threshold
    )
    overlay_temp = rayimage::render_image_overlay(
      overlay_temp_under,
      overlay_temp
    )
  }
  return(overlay_temp)
}

#' Resolve scale-bar overlay scene measurements
#'
#' @param heightmap Heightmap or spatial raster.
#' @param extent Scene extent.
#' @param caller Default `NULL`. Calling function used in errors.
#'
#' @return Physical map dimensions and CRS unit metadata.
#' @keywords internal
resolve_scalebar_overlay_scene_info = function(
  heightmap,
  extent,
  caller = NULL
) {
  extent_values = get_extent(extent)
  scene_crs = tryCatch(
    get_scene_target_crs(
      extent = extent,
      heightmap = heightmap,
      caller = caller
    ),
    error = function(error) NULL
  )
  scene_crs = try_parse_scene_crs(scene_crs)
  raw_dimensions = abs(c(
    extent_values[["xmax"]] - extent_values[["xmin"]],
    extent_values[["ymax"]] - extent_values[["ymin"]]
  ))
  if (is.null(scene_crs) || !requireNamespace("sf", quietly = TRUE)) {
    return(list(
      dimensions = raw_dimensions,
      metric = FALSE,
      crs = scene_crs,
      map_unit_meters = 1
    ))
  }

  center = c(
    mean(extent_values[c("xmin", "xmax")]),
    mean(extent_values[c("ymin", "ymax")])
  )
  dimension_points = sf::st_sfc(
    sf::st_point(c(extent_values[["xmin"]], center[[2L]])),
    sf::st_point(c(extent_values[["xmax"]], center[[2L]])),
    sf::st_point(c(center[[1L]], extent_values[["ymin"]])),
    sf::st_point(c(center[[1L]], extent_values[["ymax"]])),
    crs = scene_crs
  )
  metric_points = sf::st_transform(dimension_points, 4326)
  dimensions = c(
    as.numeric(sf::st_distance(metric_points[1], metric_points[2])),
    as.numeric(sf::st_distance(metric_points[3], metric_points[4]))
  )
  map_unit_meters = if (isTRUE(sf::st_is_longlat(scene_crs))) {
    NA_real_
  } else {
    render_scalebar_unit_meters(scene_crs$units_gdal)
  }
  list(
    dimensions = dimensions,
    metric = all(is.finite(dimensions)) && all(dimensions > 0),
    crs = scene_crs,
    map_unit_meters = map_unit_meters
  )
}

#' Resolve automatic scale-bar overlay arguments
#'
#' @param length Scale-bar length.
#' @param length_missing Whether `length` was omitted.
#' @param unit Scale-bar distance unit.
#' @param unit_missing Whether `unit` was omitted.
#' @param bearing Scale-bar bearing.
#' @param bearing_missing Whether `bearing` was omitted.
#' @param latlong Whether geometry uses longitude/latitude coordinates.
#' @param thickness Scale-bar thickness.
#' @param scene_info Scene measurement metadata.
#'
#' @return Resolved geometry and display measurements.
#' @keywords internal
resolve_scalebar_overlay_specification = function(
  length = NULL,
  length_missing = is.null(length),
  unit = NULL,
  unit_missing = is.null(unit),
  bearing = NULL,
  bearing_missing = is.null(bearing),
  latlong = FALSE,
  thickness = NA,
  scene_info
) {
  dimensions = suppressWarnings(as.numeric(scene_info$dimensions))
  if (
    length(dimensions) != 2L ||
      any(!is.finite(dimensions)) ||
      any(dimensions <= 0)
  ) {
    stop("Could not determine positive map dimensions.", call. = FALSE)
  }
  target_distance = max(dimensions) / 2
  if (isTRUE(length_missing)) {
    unit_info = resolve_render_scalebar_unit(
      label_unit = NULL,
      scene_info = scene_info,
      target_distance = target_distance
    )
    display_breaks = pretty_render_scalebar_limits(
      target_distance / unit_info$distance_per_unit
    )
    display_length = max(display_breaks)
    physical_length = display_length * unit_info$distance_per_unit
  } else {
    length = suppressWarnings(as.numeric(length)[1L])
    if (!is.finite(length) || length <= 0) {
      stop("`length` must be a positive finite number.", call. = FALSE)
    }
    if (isTRUE(unit_missing)) {
      if (!isTRUE(scene_info$metric)) {
        unit_info = list(label = "", distance_per_unit = 1)
      } else if (isTRUE(latlong)) {
        unit_info = list(label = "m", distance_per_unit = 1)
      } else {
        crs_unit = scene_info$crs$units_gdal
        unit_info = list(
          label = abbreviate_render_scalebar_unit(crs_unit),
          distance_per_unit = scene_info$map_unit_meters
        )
      }
    } else {
      unit_info = resolve_render_scalebar_unit(
        label_unit = unit,
        scene_info = scene_info,
        target_distance = target_distance
      )
    }
    if (!is.finite(unit_info$distance_per_unit)) {
      stop("Could not convert `unit` to map distance units.", call. = FALSE)
    }
    display_length = length
    physical_length = length * unit_info$distance_per_unit
  }

  geometry_length = if (!isTRUE(scene_info$metric)) {
    display_length
  } else if (isTRUE(latlong)) {
    physical_length
  } else {
    physical_length / scene_info$map_unit_meters
  }
  if (!is.finite(geometry_length) || geometry_length <= 0) {
    stop(
      "Could not convert the scale-bar length into map units.",
      call. = FALSE
    )
  }

  if (isTRUE(bearing_missing)) {
    bearing = if (dimensions[[1L]] >= dimensions[[2L]]) 90 else 0
  }
  bearing = suppressWarnings(as.numeric(bearing)[1L])
  if (!is.finite(bearing)) {
    stop("`bearing` must be a finite number.", call. = FALSE)
  }

  if (length(thickness) != 1L || is.na(thickness)) {
    geometry_thickness = geometry_length / 20
  } else {
    thickness = suppressWarnings(as.numeric(thickness)[1L])
    if (!is.finite(thickness) || thickness <= 0) {
      stop("`thickness` must be a positive finite number.", call. = FALSE)
    }
    geometry_thickness = if (isTRUE(unit_missing)) {
      thickness
    } else if (!isTRUE(scene_info$metric)) {
      thickness
    } else if (isTRUE(latlong)) {
      thickness * unit_info$distance_per_unit
    } else {
      thickness * unit_info$distance_per_unit / scene_info$map_unit_meters
    }
  }

  list(
    geometry_length = geometry_length,
    display_length = display_length,
    geometry_thickness = geometry_thickness,
    unit = unit_info$label,
    bearing = bearing,
    physical_length = physical_length
  )
}

resolve_scalebar_overlay_latlong = function(
  latlong = NA,
  extent = NULL,
  heightmap = NULL,
  caller = NULL
) {
  latlong = validate_scalebar_latlong(latlong, caller = caller)
  inferred_latlong = infer_scalebar_spatial_latlong(
    extent = extent,
    heightmap = heightmap,
    caller = caller
  )
  if (!is.na(inferred_latlong)) {
    return(inferred_latlong)
  }
  if (is.matrix(heightmap)) {
    return(isTRUE(latlong))
  }
  FALSE
}

validate_scalebar_latlong = function(latlong = NA, caller = NULL) {
  if (
    length(latlong) != 1 ||
      !(is.logical(latlong) || is.numeric(latlong))
  ) {
    stop(
      paste0(
        format_render_caller_prefix(caller),
        "`latlong` must be TRUE, FALSE, or NA."
      ),
      call. = FALSE
    )
  }
  as.logical(latlong)
}

infer_scalebar_spatial_latlong = function(
  extent = NULL,
  heightmap = NULL,
  caller = NULL
) {
  candidates = list(
    infer_scalebar_input_latlong(extent),
    infer_scalebar_input_latlong(heightmap),
    infer_scalebar_crs_latlong(tryCatch(
      get_scene_target_crs(
        extent = extent,
        heightmap = heightmap,
        caller = caller
      ),
      error = function(e) NULL
    )),
    infer_scalebar_crs_latlong(get_scene_crs(default = NULL)),
    infer_scalebar_crs_latlong(get_hillshade_crs(default = NULL))
  )
  for (candidate in candidates) {
    if (!is.na(candidate)) {
      return(candidate)
    }
  }
  NA
}

infer_scalebar_input_latlong = function(x) {
  if (is.null(x)) {
    return(NA)
  }
  if (inherits(x, "SpatRaster")) {
    is_lonlat = tryCatch(terra::is.lonlat(x), error = function(e) NA)
    if (isTRUE(is_lonlat)) {
      return(TRUE)
    }
    crs_value = tryCatch(terra::crs(x), error = function(e) NULL)
    if (!is.null(crs_value) && nzchar(trimws(as.character(crs_value)[1]))) {
      return(FALSE)
    }
    return(NA)
  }
  if (inherits(x, c("RasterLayer", "RasterBrick", "RasterStack"))) {
    is_lonlat = tryCatch(raster::isLonLat(x), error = function(e) NA)
    if (isTRUE(is_lonlat)) {
      return(TRUE)
    }
    crs_value = tryCatch(raster::projection(x), error = function(e) NULL)
    if (!is.null(crs_value) && nzchar(trimws(as.character(crs_value)[1]))) {
      return(FALSE)
    }
    return(NA)
  }
  if (
    inherits(
      x,
      c(
        "sf",
        "sfc",
        "sfg",
        "Spatial",
        "bbox",
        "SpatialPolygonsDataFrame",
        "SpatialPoints",
        "SpatialPointsDataFrame",
        "SpatialMultiPoints",
        "SpatialMultiPointsDataFrame",
        "SpatialPixels",
        "SpatialPixelsDataFrame",
        "SpatialGrid",
        "SpatialGridDataFrame",
        "SpatialLines",
        "SpatialLinesDataFrame",
        "SpatialPolygons"
      )
    )
  ) {
    return(infer_scalebar_crs_latlong(tryCatch(
      sf::st_crs(x),
      error = function(e) NULL
    )))
  }
  infer_scalebar_crs_latlong(tryCatch(
    attr(x, "crs", exact = TRUE),
    error = function(e) NULL
  ))
}

infer_scalebar_crs_latlong = function(crs) {
  if (is.null(crs) || !(length(find.package("sf", quiet = TRUE)) > 0)) {
    return(NA)
  }
  parsed_crs = try_parse_scene_crs(crs)
  if (is.null(parsed_crs)) {
    return(NA)
  }
  is_lonlat = tryCatch(sf::st_is_longlat(parsed_crs), error = function(e) NA)
  if (isTRUE(is_lonlat)) {
    return(TRUE)
  }
  if (identical(is_lonlat, FALSE)) {
    return(FALSE)
  }
  NA
}
