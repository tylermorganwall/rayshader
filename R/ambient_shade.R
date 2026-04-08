#'@title Calculate Ambient Occlusion Map
#'
#'@description Calculates Ambient Occlusion Shadow Map
#'
#'@param heightmap  A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param anglebreaks Default `90*cospi(seq(5, 85,by =5)/180)`. The angle(s), in degrees, as measured from the horizon from which the light originates.
#'@param sunbreaks Default `24`. Number of rays to be sent out in a circle, evenly spaced, around the point being tested.
#'@param maxsearch Default `30`. The maximum horizontal distance that the system should propogate rays to check for surface intersections.
#'@param multicore Default FALSE. If TRUE, multiple cores will be used to compute the shadow matrix. By default, this uses all cores available, unless the user has
#'set `options("cores")` in which the multicore option will only use that many cores.
#'@param zscale Default 1. The ratio between the x and y spacing (which are assumed to be equal) and the z axis.
#'@param cache_mask Default `NULL`. A matrix of 1 and 0s, indicating which points on which the raytracer will operate.
#'@param shadow_cache Default `NULL`. The shadow matrix to be updated at the points defined by the argument `cache_mask`.
#'@param progbar Default `TRUE` if interactive, `FALSE` otherwise. If `FALSE`, turns off progress bar.
#'@param ... Additional arguments to pass to the `makeCluster` function when `multicore=TRUE`.
#'@return Shaded texture map.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
#'if(run_documentation()) {
#'plot_map(ambient_shade(heightmap = montereybay))
#'}
#'
#'#We can increase the distance to look for surface intersections `maxsearch`
#'#and the density of rays sent out around the point `sunbreaks`.
#'if(run_documentation()) {
#'plot_map(ambient_shade(montereybay, sunbreaks = 24,maxsearch = 100, multicore=TRUE))
#'}
#'#Create the Red Relief Image Map (RRIM) technique using a custom texture and ambient_shade(),
#'#with an addition lambertian layer added with lamb_shade() to improve topographic clarity.
#'if(run_documentation()) {
#'bigmb = resize_matrix(montereybay, scale=2, method="cubic")
#'bigmb |>
#'  sphere_shade(zscale=3, texture = create_texture("red","red","red","red","white")) |>
#'  add_shadow(ambient_shade(bigmb, maxsearch = 100, multicore = TRUE,zscale=1),0) |>
#'  add_shadow(lamb_shade(bigmb),0.5) |>
#'  plot_map()
#'}
ambient_shade = function(
	heightmap,
	anglebreaks = 90 * cospi(seq(5, 85, by = 5) / 180),
	sunbreaks = 24,
	maxsearch = 30,
	multicore = FALSE,
	zscale = 1,
	cache_mask = NULL,
	shadow_cache = NULL,
	progbar = interactive(),
	...
) {
	heightmap_missing = missing(heightmap)
	heightmap_cache_label = format_scene_cache_label(deparse(substitute(heightmap)))
	zscale_cache_input_label = format_scene_cache_label(deparse(substitute(zscale)))
	heightmap_auto_zscale = NA_real_
	if (heightmap_missing) {
		resolved_heightmap = resolve_hillshade_heightmap(
			heightmap_missing = TRUE,
			caller = "ambient_shade"
		)
		heightmap = resolved_heightmap$heightmap
	} else {
		heightmap_info = coerce_plot_3d_heightmap(heightmap)
		heightmap = heightmap_info$heightmap
		heightmap_auto_zscale = heightmap_info$zscale
		cache_hillshade_heightmap(heightmap, label = heightmap_cache_label)
	}
	stopifnot(is.matrix(heightmap))
	resolved_zscale = resolve_hillshade_zscale(
		zscale = zscale,
		zscale_missing = missing(zscale),
		caller = "ambient_shade",
		auto_zscale = heightmap_auto_zscale
	)
	zscale = resolved_zscale$zscale
	zscale_cache_label = switch(
		resolved_zscale$source,
		explicit = zscale_cache_input_label,
		auto = format_scene_cache_label(sprintf("%s_auto_zscale", heightmap_cache_label)),
		hillshade = resolved_zscale$label,
		scene = resolved_zscale$label,
		NULL
	)
	cache_hillshade_zscale(zscale, label = zscale_cache_label)
	if (sunbreaks < 3) {
		stop("sunbreaks needs to be at least 3")
	}

	shademat = matrix(0, nrow = ncol(heightmap), ncol = nrow(heightmap))
	if (!multicore) {
		for (angle in seq(0, 360, length.out = sunbreaks + 1)[-(sunbreaks + 1)]) {
			shademat = shademat +
				ray_shade(
					heightmap,
					anglebreaks = anglebreaks,
					sunangle = angle,
					maxsearch = maxsearch,
					zscale = zscale,
					lambert = FALSE,
					cache_mask = cache_mask,
					progbar = progbar,
					...
				)
		}
	} else {
		if (is.null(options("cores")[[1]])) {
			numbercores = parallel::detectCores()
		} else {
			numbercores = options("cores")[[1]]
		}
		cl = parallel::makeCluster(numbercores, ...)
		doParallel::registerDoParallel(cl, cores = numbercores)
		shademat = tryCatch(
			{
				foreach::foreach(
					angle = seq(0, 360, length.out = sunbreaks + 1)[-(sunbreaks + 1)],
					.export = c("ray_shade"),
					.combine = "+",
					.packages = "rayshader"
				) %dopar%
					{
						ray_shade(
							heightmap,
							anglebreaks = anglebreaks,
							sunangle = angle,
							maxsearch = maxsearch,
							zscale = zscale,
							lambert = FALSE,
							cache_mask = cache_mask,
							progbar = progbar,
							...
						)
					}
			},
			finally = {
				tryCatch(
					{
						parallel::stopCluster(cl)
					},
					error = function(e) {
						print(e)
					}
				)
			}
		)
	}
	shademat = shademat / sunbreaks
	shademat = shademat
	if (!is.null(shadow_cache)) {
		cache_mask = (cache_mask)
		shadow_cache[cache_mask == 1] = shademat[cache_mask == 1]
		shademat = matrix(
			shadow_cache,
			nrow = nrow(shademat),
			ncol = ncol(shademat)
		)
	}
	return(shademat)
}
