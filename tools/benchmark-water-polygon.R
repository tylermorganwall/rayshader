#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

water_polygon_benchmark_heightmap = function(n) {
  outer(
    seq_len(n),
    seq_len(n),
    function(i, j) sin(i / 18) + cos(j / 21) + 0.001 * i
  )
}

water_polygon_benchmark_masks = function(n) {
  sparse = matrix(FALSE, n, n)
  center = floor(n / 2)
  sparse[seq(center - 8, center + 7), seq(center - 8, center + 7)] = TRUE

  dense = matrix(TRUE, n, n)

  multi = matrix(FALSE, n, n)
  multi[
    seq(max(1, floor(n * 0.12)), floor(n * 0.18)),
    seq(max(1, floor(n * 0.12)), floor(n * 0.18))
  ] = TRUE
  multi[
    seq(floor(n * 0.40), floor(n * 0.47)),
    seq(floor(n * 0.70), floor(n * 0.80))
  ] = TRUE
  multi[
    seq(floor(n * 0.75), floor(n * 0.88)),
    seq(floor(n * 0.25), floor(n * 0.35))
  ] = TRUE

  list(sparse = sparse, dense = dense, multi = multi)
}

water_polygon_benchmark_alloc_mb = function(path) {
  if (!file.exists(path)) {
    return(NA_real_)
  }
  lines = readLines(path, warn = FALSE)
  bytes = suppressWarnings(as.numeric(sub(" .*", "", lines)))
  sum(bytes[is.finite(bytes)], na.rm = TRUE) / 1024^2
}

water_polygon_benchmark_case = function(n, name, mask) {
  heightmap = water_polygon_benchmark_heightmap(n)
  heightmap[mask] = heightmap[mask] - 3
  water_surface = matrix(NA_real_, n, n)
  water_surface[mask] = max(heightmap[mask], na.rm = TRUE)
  gc()
  mem_file = tempfile("water-polygon-mem-")
  Rprofmem(mem_file)
  timing = system.time({
    mesh = rayshader:::make_spatial_water_polygon_surface(
      water_surface = water_surface,
      heightmap = heightmap
    )
  })
  Rprofmem(NULL)
  alloc_mb = water_polygon_benchmark_alloc_mb(mem_file)
  unlink(mem_file)
  data.frame(
    n = n,
    mask = name,
    elapsed_sec = unname(timing[["elapsed"]]),
    allocated_mb = alloc_mb,
    vertices = nrow(mesh$vertices),
    lines = nrow(mesh$lines)
  )
}

sizes = c(256L, 512L)
results = do.call(
  rbind,
  lapply(sizes, function(n) {
    masks = water_polygon_benchmark_masks(n)
    do.call(
      rbind,
      Map(
        function(name, mask) water_polygon_benchmark_case(n, name, mask),
        names(masks),
        masks
      )
    )
  })
)

print(results, row.names = FALSE)
