ray_has_tex_envir = new.env(parent = emptyenv())
ray_has_norm_envir = new.env(parent = emptyenv())
ray_cache_scene_envir = new.env(parent = emptyenv())
assign("scene_cache", NULL, envir = ray_cache_scene_envir)
