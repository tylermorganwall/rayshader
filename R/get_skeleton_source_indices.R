#'@title Get Source Row Indices for raybevel Skeleton Objects
#'
#'@keywords internal
get_skeleton_source_indices = function(skeletons) {
	if (inherits(skeletons, "rayskeleton_list")) {
		indices = unlist(lapply(skeletons, function(x) {
			attr(x, "original_sf_row_index")
		}))
	} else if (inherits(skeletons, "rayskeleton")) {
		indices = attr(skeletons, "original_sf_row_index")
		if (is.null(indices) || !length(indices)) {
			indices = 1L
		}
	} else {
		indices = integer(0)
	}
	as.integer(indices)
}
