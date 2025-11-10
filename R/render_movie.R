#'@title Render Movie
#'
#'@description Renders a movie using the \pkg{av} or \pkg{gifski} packages. Moves the camera around a 3D visualization
#'using either a standard orbit, or accepts vectors listing user-defined values for each camera parameter. If the latter,
#'the values must be equal in length to `frames` (or of length `1`, in which the value will be fixed).
#'
#'Additional arguments are forwarded to [render_snapshot()] for additional customization arguments (like
#' adding titles).
#'
#'@param filename Filename. If not appended with `.mp4`, it will be appended automatically. If the file extension is `gif`,
#'the \pkg{gifski} package will be used to generate the animation.
#'@param type Default `orbit`, which orbits the 3D object at the user-set camera settings `phi`, `zoom`, and `fov`.
#'Other options are `oscillate` (sine wave around `theta` value, covering 90 degrees), or `custom` (which uses the values from the
#'`theta`, `phi`, `zoom`, and `fov` vectors passed in by the user).
#'@param frames Default `360`. Number of frames to render.
#'@param fps Default `30`. Frames per second. Recommmend either 30 or 60 for web.
#'@param phi Defaults to current view. Azimuth values, in degrees.
#'@param theta Default to current view. Theta values, in degrees.
#'@param zoom Defaults to the current view. Zoom value, between `0` and `1`.
#'@param fov Defaults to the current view. Field of view values, in degrees.
#'@param width Default `NULL`, uses the window size by default. Width of the movie. Note that the frames will still
#'be captured at the resolution (and aspect ratio) of the rgl window.
#'@param height Default `NULL`, uses the window size by default. Height of the movie. Note that the frames will still
#'be captured at the resolution (and aspect ratio) of the rgl window.
#'@param audio Default `NULL`. Optional file with audio to add to the video.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'Will display a progress bar when adding an overlay or title.
#'@param ... Additional parameters to pass to [render_snapshot()].
#'@export
#'@examples
#'if(interactive()) {
#'filename_movie = tempfile()
#'
#'#By default, the function produces a 12 second orbit at 30 frames per second, at 30 degrees azimuth.
#'\donttest{
#'montereybay |>
#'  sphere_shade(texture="imhof1") |>
#'  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof1",
#'          waterlinecolor="white", waterlinealpha=0.5)
#'#Un-comment the following to run:
#'#render_movie(filename = filename_movie)
#'}
#'filename_movie = tempfile()
#'
#'#You can change to an oscillating orbit. The magnification is increased and azimuth angle set to 30.
#'#A title has also been added using the title_text argument.
#'\donttest{
#'#Un-comment the following to run:
#'#render_movie(filename = filename_movie, type = "oscillate",
#'#             frames = 60,  phi = 30, zoom = 0.8, theta = -90,
#'#             title_text = "Monterey Bay: Oscillating")
#'}
#'filename_movie = tempfile()
#'
#'#Finally, you can pass your own set of values to the
#'#camera parameters as a vector with type = "custom".
#'
#'phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
#'phivecfull = c(phivechalf, rev(phivechalf))
#'thetavec = -90 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
#'zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
#'zoomvecfull = c(zoomvec, rev(zoomvec))
#'\donttest{
#'#Un-comment the following to run
#'#render_movie(filename = filename_movie, type = "custom",
#'#             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
#'}
#'}
render_movie = function(
	filename,
	type = "orbit",
	frames = 360,
	fps = 30,
	phi = 30,
	theta = 0,
	zoom = NULL,
	fov = NULL,
	width = NULL,
	height = NULL,
	audio = NULL,
	progbar = interactive(),
	...
) {
	if (rgl::cur3d() == 0) {
		stop("No rgl window currently open.")
	}
	if (is.null(filename)) {
		stop("render_movie requires a filename")
	}
	movie_type = tools::file_ext(filename)
	use_av = TRUE
	if (movie_type %in% c("mp4", "mkv", "mov", "flv", "")) {
		if (movie_type == "") {
			filename = paste0(filename, ".mp4")
		}
		if (!(length(find.package("av", quiet = TRUE)) > 0)) {
			stop("`av` package required for render_movie()")
		}
	} else if (movie_type == "gif") {
		if (!(length(find.package("gifski", quiet = TRUE)) > 0)) {
			stop("`gifski` package required for render_movie() gifs")
		}
		use_av = FALSE
	}

	windowsize = rgl::par3d()$viewport
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
		}
	}
	png_files = file.path(tempdir(), sprintf("image%d.png", seq_len(frames)))
	on.exit(unlink(png_files))
	if (type == "orbit") {
		theta = seq(0, 360, length.out = frames + 1)[-(frames + 1)]
	} else if (type == "oscillate") {
		theta = theta +
			45 * sin(seq(0, 360, length.out = frames + 1)[-(frames + 1)] * pi / 180)
	} else if (type != "custom") {
		stop("Unknown type: ", type)
	}
	df_cam = data.frame(theta = theta, phi = phi, zoom = zoom, fov = fov)
	theta = df_cam$theta
	phi = df_cam$phi
	zoom = df_cam$zoom
	fov = df_cam$fov
	for (i in seq_len(frames)) {
		render_camera(
			theta = theta[i],
			phi = phi[i],
			zoom = zoom[i],
			fov = fov[i]
		)
		render_snapshot(filename = png_files[i], cache_scene = TRUE, ...)
	}
	temp = rayimage::ray_read_image(png_files[1])
	dimensions = dim(temp)
	if (!is.null(width)) {
		dimensions[1] = width
	}
	if (!is.null(height)) {
		dimensions[2] = height
	}
	if (dimensions[1] %% 2 != 0) {
		dimensions[1] = dimensions[1] - 1
	}
	if (use_av) {
		av::av_encode_video(
			png_files,
			output = filename,
			framerate = fps,
			vfilter = paste0("scale=", dimensions[2], ":-2"),
			audio = audio
		)
	} else {
		gifski::gifski(
			png_files = png_files,
			gif_file = filename,
			delay = 1 / fps,
			width = dimensions[1],
			height = dimensions[2]
		)
	}
}
