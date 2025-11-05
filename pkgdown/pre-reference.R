# Bail out quickly if packages or data aren't available in this environment.
ok = requireNamespace("av", quietly = TRUE) &&
	requireNamespace("rayimage", quietly = TRUE)
if (!ok) {
	quit(save = "no")
}

# Create frames into a temp dir (fast + CRAN-safe); then write a tiny GIF to man/figures.
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
tmp = tempdir()

# Recreate your original example but compact, with fewer frames & smaller size.
moss_landing_coord = c(36.806807, -121.793332)
t = seq(0, 2 * pi, length.out = 360) # fewer frames for speed
circle_coords_lat = moss_landing_coord[1] + 0.25 * sin(t)
circle_coords_long = moss_landing_coord[2] + 0.25 * cos(t)

# Minimal 3D render -> frames
extent_mb = attr(rayshader::montereybay, "extent")
rayshader::sphere_shade(rayshader::montereybay) |>
	rayshader::plot_3d(
		rayshader::montereybay,
		zscale = 50,
		water = TRUE,
		shadowcolor = "#40310a",
		background = "tan",
		theta = 210,
		phi = 22,
		zoom = 0.40,
		fov = 55
	)

rayshader::render_path(
	extent = extent_mb,
	heightmap = rayshader::montereybay,
	lat = circle_coords_lat,
	long = circle_coords_long,
	zscale = 50,
	color = "red",
	antialias = TRUE,
	offset = 500,
	linewidth = 2
)

cam = rayshader::convert_path_to_animation_coords(
	extent = extent_mb,
	heightmap = rayshader::montereybay,
	lat = circle_coords_lat,
	long = circle_coords_long,
	type = "bezier",
	damp_motion = TRUE,
	fovs = 80,
	zscale = 50,
	offset = 1000,
	frames = length(t)
)

cam = rayshader::convert_path_to_animation_coords(
	extent = extent_mb,
	heightmap = rayshader::montereybay,
	lat = circle_coords_lat,
	long = circle_coords_long,
	type = "bezier",
	damp_motion = TRUE,
	fovs = 80,
	zscale = 50,
	offset = 1000,
	frames = length(t)
)

rayshader::render_highquality(
	samples = 4,
	animation_camera_coords = cam,
	width = 200,
	height = 200,
	preview = FALSE,
	filename = file.path(tmp, "frame"),
	use_extruded_paths = TRUE
)

# Assemble frames -> GIF (pkgdown copies from man/figures)
pngs = sprintf("%s/frame%d.png", tmp, seq_along(t))
av::av_encode_video(
	pngs,
	output = "man/figures/monterey-circle.mp4",
	framerate = 24
)

follow_cam = rayshader::convert_path_to_animation_coords(
	extent = extent_mb,
	heightmap = rayshader::montereybay,
	lat = circle_coords_lat,
	long = circle_coords_long,
	type = "bezier",
	damp_motion = TRUE,
	fovs = 80,
	zscale = 50,
	follow_camera = TRUE,
	offset = 1000,
	frames = length(t)
)

rayshader::render_highquality(
	samples = 4,
	animation_camera_coords = follow_cam,
	width = 200,
	height = 200,
	preview = FALSE,
	filename = file.path(tmp, "frame"),
	use_extruded_paths = TRUE
)

# Assemble frames -> GIF (pkgdown copies from man/figures)
pngs = sprintf("%s/frame%d.png", tmp, seq_along(t))
av::av_encode_video(
	pngs,
	output = "man/figures/monterey-circle-follow.mp4",
	framerate = 24
)
