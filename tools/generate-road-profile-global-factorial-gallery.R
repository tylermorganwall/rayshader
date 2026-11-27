args = commandArgs(trailingOnly = TRUE)

repo_root = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
campaign_dir = if (length(args) >= 1L) {
  args[[1L]]
} else {
  Sys.getenv(
    "RAYSHADER_SCREEN_OUTPUT_DIR",
    unset = file.path(dirname(repo_root), "road-profile-global-2x2")
  )
}
campaign_dir = normalizePath(campaign_dir, winslash = "/", mustWork = TRUE)
output_path = if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path(campaign_dir, "global-factorial-effects.html")
}

design = read.csv(
  file.path(campaign_dir, "factor-design.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
manifest = read.csv(
  file.path(campaign_dir, "render-manifest.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("NA", "")
)
cameras = read.csv(
  file.path(campaign_dir, "camera-manifest.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
configuration = read.csv(
  file.path(campaign_dir, "run-configuration.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

configuration = setNames(configuration$value, configuration$name)
if (!identical(unname(configuration[["relaxation_mode"]]), "global")) {
  stop(
    "This gallery requires a campaign rendered with global relaxation.",
    call. = FALSE
  )
}

cell_ids = c("H00", "H03", "H07", "H19")
scene_ids = c(
  "swiss_switchback",
  "houston_interchange",
  "houston_overpass"
)
scene_labels = c(
  swiss_switchback = "Swiss switchback",
  houston_interchange = "Houston interchange",
  houston_overpass = "Houston downtown overpass"
)
design = design[match(cell_ids, design$cell_id), , drop = FALSE]
if (any(is.na(design$cell_id))) {
  stop(
    "The campaign is missing one or more reduced-factorial cells.",
    call. = FALSE
  )
}

html_escape = function(value) {
  value = gsub("&", "&amp;", as.character(value), fixed = TRUE)
  value = gsub("<", "&lt;", value, fixed = TRUE)
  value = gsub(">", "&gt;", value, fixed = TRUE)
  gsub('"', "&quot;", value, fixed = TRUE)
}

relative_path = function(path) {
  prefix = paste0(campaign_dir, "/")
  if (startsWith(path, prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    path
  }
}

factor_label = function(cell) {
  sprintf(
    "%s — maximum grade %g%%; continuation tolerance %g%%",
    cell$cell_id,
    100 * cell$maximum_grade_relaxed,
    100 * cell$continuation_grade_tolerance_relaxed
  )
}

find_render = function(cell_id, scene_id, camera_id, role) {
  row = manifest[
    manifest$cell_id == cell_id &
      manifest$scene_id == scene_id &
      manifest$camera_id == camera_id &
      manifest$render_role == role,
    ,
    drop = FALSE
  ]
  if (nrow(row) != 1L || !isTRUE(row$render_succeeded)) {
    stop(
      sprintf(
        "Missing successful %s render for %s/%s/%s.",
        role,
        cell_id,
        scene_id,
        camera_id
      ),
      call. = FALSE
    )
  }
  row
}

render_card = function(cell, scene_id, camera_id) {
  role = if (identical(cell$cell_id, "H00")) "baseline" else "candidate"
  row = find_render(cell$cell_id, scene_id, camera_id, role)
  image = html_escape(relative_path(row$image_path))
  video = file.path(
    "videos",
    cell$cell_id,
    scene_id,
    paste0(camera_id, ".mp4")
  )
  video_link = if (file.exists(file.path(campaign_dir, video))) {
    sprintf('<a href="%s">Flip video</a>', html_escape(video))
  } else {
    ""
  }
  sprintf(
    paste0(
      '<article class="card"><h3>%s</h3>',
      '<a href="%s"><img src="%s" loading="lazy"></a>',
      '<p>Production render · %s</p></article>'
    ),
    html_escape(factor_label(cell)),
    image,
    image,
    video_link
  )
}

effect_card = function(cell, scene_id, camera_id) {
  if (identical(cell$cell_id, "H00")) {
    return(paste0(
      '<article class="card reference"><h3>H00 reference</h3>',
      '<p>No difference image: this is the 7% / 0% comparison baseline.</p>',
      "</article>"
    ))
  }
  row = find_render(
    cell$cell_id,
    scene_id,
    camera_id,
    "diagnostic_overlay"
  )
  image = html_escape(relative_path(row$image_path))
  sprintf(
    paste0(
      '<article class="card"><h3>%s — 32× magenta effect</h3>',
      '<a href="%s"><img src="%s" loading="lazy"></a></article>'
    ),
    html_escape(cell$cell_id),
    image,
    image
  )
}

factor_headers = vapply(
  seq_len(nrow(design)),
  function(index) {
    cell = design[index, , drop = FALSE]
    sprintf("<th>%s</th>", html_escape(factor_label(cell)))
  },
  character(1)
)

sections = character()
for (scene_id in scene_ids) {
  scene_cameras = cameras[cameras$scene_id == scene_id, , drop = FALSE]
  camera_sections = vapply(
    scene_cameras$camera_id,
    function(camera_id) {
      render_cards = vapply(
        seq_len(nrow(design)),
        function(index) {
          render_card(design[index, , drop = FALSE], scene_id, camera_id)
        },
        character(1)
      )
      effect_cards = vapply(
        seq_len(nrow(design)),
        function(index) {
          effect_card(design[index, , drop = FALSE], scene_id, camera_id)
        },
        character(1)
      )
      paste0(
        '<section class="camera"><h2>',
        html_escape(gsub("_", " ", camera_id, fixed = TRUE)),
        '</h2><h3 class="row-label">Production renders</h3>',
        '<div class="wide"><div class="matrix">',
        paste(render_cards, collapse = ""),
        '</div></div><h3 class="row-label">Change from H00</h3>',
        '<div class="wide"><div class="matrix">',
        paste(effect_cards, collapse = ""),
        "</div></div></section>"
      )
    },
    character(1)
  )
  sections = c(
    sections,
    paste0(
      '<section class="scene" id="',
      html_escape(scene_id),
      '"><h1>',
      html_escape(scene_labels[[scene_id]]),
      "</h1>",
      paste(camera_sections, collapse = ""),
      "</section>"
    )
  )
}

page = c(
  "<!doctype html><html><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>Global road-profile 2×2 effects</title>",
  paste0(
    "<style>",
    ":root{color-scheme:dark}body{font-family:system-ui;margin:0;background:#111;color:#eee}",
    "header,.scene{padding:24px}header{background:#191919;border-bottom:1px solid #444}",
    "nav a{color:#70d7ff;margin-right:20px}table{border-collapse:collapse;width:100%}",
    "th,td{border:1px solid #555;padding:10px;text-align:left}code{color:#ff9bea}",
    ".camera{border-top:2px solid #555;margin-top:32px;padding-top:12px}",
    ".row-label{color:#bbb}.wide{overflow-x:auto;padding-bottom:10px}",
    ".matrix{display:grid;grid-template-columns:repeat(4,600px);gap:14px;min-width:2442px}",
    ".card{border:1px solid #555;background:#1b1b1b;padding:12px}",
    ".card h3{min-height:48px;margin:0 0 10px}.card img{display:block;width:100%;height:auto}",
    ".card a{color:#70d7ff}.reference{display:flex;flex-direction:column;justify-content:center}",
    "</style></head><body>"
  ),
  "<header><h1>Global road-profile 2×2 experiment</h1>",
  paste0(
    "<p>The terrain-gradient smooth transition is disabled. Each maximum-grade ",
    "and continuation-tolerance value applies globally. Maximum grade-rate is ",
    "held at <code>0.001 m/m per metre</code>; terrain-reference weight is held ",
    "at <code>0.001</code>. All cells use identical terrain, cameras, lighting, ",
    "vertical exaggeration, and renderer settings.</p>"
  ),
  paste0(
    "<nav><a href='#swiss_switchback'>Swiss switchback</a>",
    "<a href='#houston_interchange'>Houston interchange</a>",
    "<a href='#houston_overpass'>Houston downtown overpass</a></nav>"
  ),
  "<h2>Factor combinations</h2><div class='wide'><table><tr>",
  factor_headers,
  "</tr></table></div>",
  paste0(
    "<p>Click any image for its native 1200×800 render. Candidate sections ",
    "show the unmodified production render first, followed by a separate 32× ",
    "magenta overlay that localizes changes relative to H00.</p></header>"
  ),
  sections,
  "</body></html>"
)

writeLines(page, output_path)
message("Wrote global factorial gallery: ", output_path)
