#'@title Render Camera
#'
#'@description Changes the position and properties of the camera around the
#'scene. `location` can move the camera's look-at target to scene x/y coordinates
#'or a spatial point. `lat` and `long` move it to a geographic point. These inputs
#'use the active scene's cached CRS, extent, heightmap, and effective z-scale as
#'needed. If no values are entered, prints and returns the current camera values.
#'
#'@param theta Defaults to current value. Rotation angle.
#'@param phi Defaults to current value. Azimuth angle. Maximum `90`.
#'@param zoom Defaults to current value. Positive value indicating camera magnification.
#'@param fov Defaults to current value. Field of view of the camera. Maximum `180`.
#'@param location Default `NULL`. A numeric `c(x, y)` pair in the scene's cached
#'extent, or a single spatial POINT to use as the camera's look-at target. Spatial
#'inputs accept `sf`, `sfc`, `sfg`, and `sp` objects and are transformed to the
#'active scene CRS. If a spatial input contains a Z coordinate or an `altitude`,
#'`alt`, `elevation`, or `z` column, that value sets the target height; otherwise,
#'the target height is sampled from the cached heightmap. Cannot be combined with
#'`lat` or `long`.
#'@param altitude Default `NULL`. Elevation of the look-at target in the scene's
#'elevation units. The value is converted to rgl coordinates using the cached
#'effective z-scale. Requires `location` or both `lat` and `long`. When supplied
#'with `location`, it overrides elevation embedded in the spatial input.
#'@param shift_vertical Default `0`. Amount to shift the viewpoint.
#'@param lat Default `NULL`. Latitude in WGS84 decimal degrees for the camera's
#'look-at target. Must be supplied with `long`.
#'@param long Default `NULL`. Longitude in WGS84 decimal degrees for the
#'camera's look-at target. Must be supplied with `lat`.
#'@param panel Default `NULL`. Facet panel identifier for scenes created with
#'[plot_gg()]. Required when the cached scene contains multiple panels.
#'@export
#'@examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#'montereybay_spatial |>
#'  sphere_shade(vertical_exaggeration = 10) |>
#'  plot_3d(vertical_exaggeration = 4, water = TRUE, waterlinecolor="white")
#'render_snapshot()
#'
#'#Shift the camera over and add a title
#'render_camera(theta = -45, phi = 45)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_bar_color = "grey50")
#'
#'#Move the camera's look-at target to Moss Landing using the cached scene CRS.
#'if (requireNamespace("sf", quietly = TRUE)) {
#'  render_camera(lat = 36.806807, long = -121.793332, altitude = 100)
#'}
#'
#'#Shift to an overhead view (and change the text/title bar color)
#'render_camera(theta = 0, phi = 89.9, zoom = 0.9)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_color = "white",
#'                title_bar_color = "darkgreen")
#'
#'#Shift to an front view and add a vignette effect
#'render_camera(theta = -90, phi = 30,zoom = 0.8)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_color = "white",
#'                title_bar_color = "blue",
#'                vignette = TRUE)
#'
#'#Change the field of view (fov) and make the title bar opaque.
#'render_camera(theta = -90, phi = 30,zoom = 0.5,fov = 130)
#'render_snapshot(title_text = "Monterey Bay, CA",
#'                title_color = "black",
#'                title_bar_alpha = 1,
#'                title_bar_color = "lightblue",
#'                vignette = TRUE)
#'
#'#Here we render a series of frames to later stitch together into a movie.
#'
#'phivec = 20 + 70 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
#'phivecfull = c(phivec, rev(phivec))
#'thetavec = 270 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
#'zoomvechalf = 0.5 + 0.5 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
#'zoomvec = c(zoomvechalf, rev(zoomvechalf))
#'
#'for(i in 1:360) {
#'  render_camera(theta = thetavec[i],phi = phivecfull[i],zoom = zoomvec[i])
#'  #uncomment the next line to save each frame to the working directory
#'  #render_snapshot(paste0("frame", i, ".png"))
#'}
#'#Run this command in the command line using ffmpeg to stitch together a video:
#'#ffmpeg -framerate 60 -i frame%d.png -vcodec libx264 raymovie.mp4
#'
#'#And run this command to convert the video to post to the web:
#'#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4
#'
#'#Or we can use render_movie() to do this all automatically with type="custom" (uncomment to run):
#'#render_movie(filename = tempfile(fileext = ".mp4"), type = "custom",
#'#             theta = thetavec, phi = phivecfull, zoom = zoomvec, fov=0)
render_camera = function(
  theta = NULL,
  phi = NULL,
  zoom = NULL,
  fov = NULL,
  location = NULL,
  altitude = NULL,
  shift_vertical = 0,
  lat = NULL,
  long = NULL,
  panel = NULL
) {
  has_location = !is.null(location)
  has_lat = !is.null(lat)
  has_long = !is.null(long)
  if (has_location && (has_lat || has_long)) {
    stop("Use either `location` or `lat` and `long`, not both.", call. = FALSE)
  }
  if (xor(has_lat, has_long)) {
    stop("`lat` and `long` must be supplied together.", call. = FALSE)
  }
  has_lookat = has_location || (has_lat && has_long)
  if (!is.null(altitude) && !has_lookat) {
    stop(
      "`altitude` requires `location` or both `lat` and `long`.",
      call. = FALSE
    )
  }
  if (
    is.null(theta) &&
      is.null(phi) &&
      is.null(zoom) &&
      is.null(fov) &&
      !has_lookat
  ) {
    allmissing = TRUE
  } else {
    allmissing = FALSE
  }
  if (rgl::cur3d() == 0) {
    stop("No rgl window currently open.")
  }
  camera_lookat = NULL
  if (has_lookat) {
    camera_lookat = resolve_render_camera_lookat(
      location = location,
      lat = lat,
      long = long,
      altitude = altitude,
      panel = panel
    )
  }
  if (is.null(fov)) {
    fov = rgl::par3d()$FOV
  }
  if (is.null(zoom)) {
    zoom = rgl::par3d()$zoom
  }
  if (is.null(phi) || is.null(theta)) {
    rotmat = rot_to_euler(rgl::par3d()$userMatrix)
    if (is.null(phi)) {
      phi = rotmat[1]
    }
    if (is.null(theta)) {
      if (0.001 > abs(abs(rotmat[3]) - 180)) {
        theta = -rotmat[2] + 180
      } else {
        theta = rotmat[2]
      }
      if (abs(phi) == 90) {
        theta = theta - 90
      }
    }
  }
  rgl::view3d(theta = theta, phi = phi, fov = fov, zoom = zoom)
  if (!is.null(camera_lookat)) {
    user_matrix = rgl::par3d("userMatrix")
    scene_bbox = rgl::par3d("bbox")
    scene_center = c(
      mean(scene_bbox[1:2]),
      mean(scene_bbox[3:4]),
      mean(scene_bbox[5:6])
    )
    scene_scale = rgl::par3d("scale")
    target_translation = user_matrix[1:3, 1:3] %*%
      ((scene_center - camera_lookat) * scene_scale)
    user_matrix[1:3, 4] = as.numeric(target_translation)
    rgl::par3d(userMatrix = user_matrix)
  }
  if (shift_vertical != 0) {
    rgl::par3d(
      userMatrix = t(rgl::translationMatrix(0, -shift_vertical, 0)) %*%
        rgl::par3d("userMatrix")
    )
  }
  if (allmissing) {
    return(c("theta" = theta, "phi" = phi, "zoom" = zoom, "fov" = fov))
  }
  invisible(NULL)
}

#' Resolve a Render Camera Look-At Target
#'
#' @param location Default `NULL`. A numeric scene `c(x, y)` pair or a single
#' spatial POINT.
#' @param lat Default `NULL`. Latitude in WGS84 decimal degrees.
#' @param long Default `NULL`. Longitude in WGS84 decimal degrees.
#' @param altitude Default `NULL`. Elevation in the scene's elevation units.
#' @param panel Default `NULL`. Facet panel identifier.
#'
#' @return A length-three numeric vector in rgl scene coordinates.
#' @keywords internal
resolve_render_camera_lookat = function(
  location = NULL,
  lat = NULL,
  long = NULL,
  altitude = NULL,
  panel = NULL
) {
  heightmap = resolve_scene_render_heightmap(caller = "render_camera")
  if (!is.null(location) && is.numeric(location)) {
    if (
      !is.null(dim(location)) ||
        length(location) != 2 ||
        any(!is.finite(location))
    ) {
      stop(
        "render_camera(): `location` must be a finite numeric `c(x, y)` pair or a single spatial POINT.",
        call. = FALSE
      )
    }
    extent = resolve_render_highquality_camera_extent(
      heightmap = heightmap,
      panel = panel,
      caller = "render_camera"
    )
    altitude = normalize_render_highquality_camera_altitude(
      altitude,
      arg_name = "location",
      caller = "render_camera"
    )
    camera_coords = transform_into_heightmap_coords(
      extent = extent,
      heightmap = heightmap,
      lat = location[2],
      long = location[1],
      altitude = altitude,
      offset = 0,
      zscale = resolve_render_highquality_camera_zscale(),
      panel = panel,
      transform_scene = FALSE,
      caller = "render_camera"
    )
    return(as.numeric(camera_coords[1, ]))
  }
  target_crs = get_scene_target_crs(
    heightmap = heightmap,
    panel = panel,
    caller = "render_camera"
  )
  if (is.null(target_crs)) {
    stop(
      "render_camera(): The active scene has no cached CRS for a geographic look-at target.",
      call. = FALSE
    )
  }
  camera_input = if (!is.null(location)) {
    list(location = location, altitude = altitude, panel = panel)
  } else {
    list(lat = lat, long = long, altitude = altitude, panel = panel)
  }
  resolve_render_highquality_camera_point(
    camera_input,
    arg_name = if (is.null(location)) "lat/long" else "location",
    bbox_center = NULL,
    caller = "render_camera"
  )
}
