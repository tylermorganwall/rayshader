args = commandArgs(trailingOnly = TRUE)

repo_root = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
default_campaign_dir = file.path(
  dirname(repo_root),
  "road-profile-visual-screen-max-grade"
)
campaign_dir = if (length(args) >= 1L) {
  args[[1L]]
} else {
  Sys.getenv("RAYSHADER_SCREEN_OUTPUT_DIR", unset = default_campaign_dir)
}
campaign_dir = normalizePath(campaign_dir, winslash = "/", mustWork = TRUE)
output_path = if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path(campaign_dir, "swiss-switchback-factorial-gallery.html")
}

design_path = file.path(campaign_dir, "factor-design.csv")
manifest_path = file.path(campaign_dir, "render-manifest.csv")
if (!file.exists(design_path) || !file.exists(manifest_path)) {
  stop(
    "The campaign directory must contain factor-design.csv and render-manifest.csv.",
    call. = FALSE
  )
}

design = read.csv(design_path, stringsAsFactors = FALSE, check.names = FALSE)
manifest = read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("NA", "")
)

required_design_columns = c(
  "cell_id",
  "grade_rate_level",
  "maximum_grade_rate_relaxed",
  "maximum_grade_level",
  "maximum_grade_relaxed",
  "continuation_level",
  "continuation_grade_tolerance_relaxed",
  "terrain_level",
  "terrain_reference_weight_relaxed"
)
missing_design_columns = setdiff(required_design_columns, names(design))
if (length(missing_design_columns)) {
  stop(
    "Missing factor-design column(s): ",
    paste(missing_design_columns, collapse = ", "),
    call. = FALSE
  )
}

html_escape = function(value) {
  value = as.character(value)
  value = gsub("&", "&amp;", value, fixed = TRUE)
  value = gsub("<", "&lt;", value, fixed = TRUE)
  value = gsub(">", "&gt;", value, fixed = TRUE)
  value = gsub('"', "&quot;", value, fixed = TRUE)
  value
}

format_number = function(value) {
  if (is.infinite(value)) {
    return("unbound")
  }
  format(value, scientific = FALSE, trim = TRUE)
}

format_percent = function(value) {
  if (is.infinite(value)) {
    return("unbound")
  }
  paste0(format(100 * value, scientific = FALSE, trim = TRUE), "%")
}

relative_to_campaign = function(path) {
  path = gsub("\\\\", "/", path)
  prefix = paste0(campaign_dir, "/")
  if (startsWith(path, prefix)) {
    return(substring(path, nchar(prefix) + 1L))
  }
  path
}

latest_render_row = function(cell_id, camera_id) {
  role = if (identical(cell_id, "H00")) "baseline" else "candidate"
  rows = manifest[
    manifest$cell_id == cell_id &
      manifest$scene_id == "swiss_switchback" &
      manifest$camera_id == camera_id &
      manifest$render_role == role,
    ,
    drop = FALSE
  ]
  if (!nrow(rows)) {
    return(NULL)
  }
  succeeded = which(rows$render_succeeded %in% TRUE)
  if (length(succeeded)) {
    return(rows[succeeded[[length(succeeded)]], , drop = FALSE])
  }
  rows[nrow(rows), , drop = FALSE]
}

camera_ids = c("high_side", "oblique_overview", "reverse_longitudinal")
camera_labels = c(
  high_side = "High side",
  oblique_overview = "Oblique overview",
  reverse_longitudinal = "Reverse longitudinal"
)

design = design[
  order(
    design$maximum_grade_level,
    design$grade_rate_level,
    design$continuation_level,
    design$terrain_level
  ),
  ,
  drop = FALSE
]

lines = c(
  "<!doctype html>",
  '<html lang="en">',
  "<head>",
  '<meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  "<title>Swiss switchback nested factorial gallery</title>",
  "<style>",
  ":root{color-scheme:dark;--bg:#111315;--panel:#1a1d20;--line:#3b4248;--muted:#aab2b9;--accent:#70d6ff;--failed:#ff8e8e}",
  "*{box-sizing:border-box}",
  "html{scroll-behavior:smooth}",
  "body{margin:0;background:var(--bg);color:#f2f4f5;font-family:system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;line-height:1.45}",
  "header{padding:28px clamp(18px,4vw,64px);border-bottom:1px solid var(--line);background:#15181a;position:sticky;top:0;z-index:10}",
  "header h1{margin:0 0 8px;font-size:clamp(24px,3vw,40px)}",
  "header p{margin:5px 0;color:var(--muted);max-width:1100px}",
  "nav{display:flex;gap:10px;flex-wrap:wrap;margin-top:14px}",
  "nav a{color:#071014;background:var(--accent);text-decoration:none;padding:7px 11px;border-radius:6px;font-weight:700}",
  "main{padding:24px clamp(12px,3vw,48px) 80px}",
  ".maximum-grade-group{margin:0 auto 54px;max-width:1900px;border:2px solid #59636c;border-radius:12px;overflow:hidden;background:#141719}",
  ".maximum-grade-group>h2{margin:0;padding:18px 22px;background:#252b30;font-size:28px}",
  ".grade-rate-group{margin:22px;border:1px solid #485159;border-radius:10px;overflow:hidden}",
  ".grade-rate-group>h3{margin:0;padding:14px 18px;background:#20252a;font-size:22px}",
  ".continuation-group{padding:16px;border-top:1px solid var(--line)}",
  ".continuation-group>h4{margin:0 0 12px;font-size:18px;color:#d8e8ef}",
  ".run{margin:0 0 20px;padding:15px;border:1px solid var(--line);border-radius:8px;background:var(--panel)}",
  ".run:last-child{margin-bottom:0}",
  ".run h5{margin:0;font-size:18px;color:var(--accent)}",
  ".settings{margin:5px 0 13px;color:var(--muted);font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:13px}",
  ".images{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;align-items:start}",
  "figure{margin:0}",
  "figcaption{margin:0 0 6px;font-weight:650}",
  "figure a{display:block}",
  "img{display:block;width:100%;height:auto;aspect-ratio:3/2;object-fit:contain;background:#080909;border:1px solid #343a3f}",
  ".missing{aspect-ratio:3/2;display:grid;place-items:center;padding:18px;background:#211718;border:1px dashed #8e4f52;color:var(--failed);text-align:center}",
  ".failure{margin:10px 0 0;color:var(--failed);font-weight:650}",
  ".baseline{display:inline-block;margin-left:7px;padding:2px 7px;border:1px solid #71808b;border-radius:999px;color:#c7d0d6;font-size:12px;vertical-align:2px}",
  "@media(max-width:900px){header{position:static}.images{grid-template-columns:1fr}.grade-rate-group{margin:12px}.continuation-group{padding:12px}}",
  "</style>",
  "</head>",
  "<body>",
  "<header>",
  "<h1>Swiss switchback: nested factorial gallery</h1>",
  "<p>Ordering is maximum grade (outermost), maximum grade rate, continuation-grade tolerance, then terrain-reference weight (fastest). Each run retains its original H identifier.</p>",
  "<p>Images are the path-traced candidate renders from the production road mesh. Click any image for its native 1200 × 800 PNG.</p>",
  "<nav>",
  '<a href="#maximum-grade-0">Maximum grade 7%</a>',
  '<a href="#maximum-grade-1">Maximum grade 15%</a>',
  '<a href="#maximum-grade-2">Maximum grade unbound</a>',
  "</nav>",
  "</header>",
  "<main>"
)

for (maximum_grade_level in sort(unique(design$maximum_grade_level))) {
  maximum_grade_rows = design[
    design$maximum_grade_level == maximum_grade_level,
    ,
    drop = FALSE
  ]
  maximum_grade = maximum_grade_rows$maximum_grade_relaxed[[1L]]
  lines = c(
    lines,
    sprintf(
      '<section class="maximum-grade-group" id="maximum-grade-%d">',
      maximum_grade_level
    ),
    sprintf(
      "<h2>Maximum-grade level %d — %s</h2>",
      maximum_grade_level,
      html_escape(format_percent(maximum_grade))
    )
  )

  for (grade_rate_level in sort(unique(maximum_grade_rows$grade_rate_level))) {
    grade_rate_rows = maximum_grade_rows[
      maximum_grade_rows$grade_rate_level == grade_rate_level,
      ,
      drop = FALSE
    ]
    grade_rate = grade_rate_rows$maximum_grade_rate_relaxed[[1L]]
    lines = c(
      lines,
      '<section class="grade-rate-group">',
      sprintf(
        "<h3>Maximum grade-rate level %d — %s per metre</h3>",
        grade_rate_level,
        html_escape(format_number(grade_rate))
      )
    )

    for (continuation_level in sort(unique(
      grade_rate_rows$continuation_level
    ))) {
      continuation_rows = grade_rate_rows[
        grade_rate_rows$continuation_level == continuation_level,
        ,
        drop = FALSE
      ]
      continuation = continuation_rows$continuation_grade_tolerance_relaxed[[
        1L
      ]]
      lines = c(
        lines,
        '<section class="continuation-group">',
        sprintf(
          "<h4>Continuation-tolerance level %d — %s</h4>",
          continuation_level,
          html_escape(format_percent(continuation))
        )
      )

      for (row_index in seq_len(nrow(continuation_rows))) {
        cell = continuation_rows[row_index, , drop = FALSE]
        cell_id = cell$cell_id[[1L]]
        terrain_level = cell$terrain_level[[1L]]
        terrain_weight = cell$terrain_reference_weight_relaxed[[1L]]
        baseline_badge = if (identical(cell_id, "H00")) {
          '<span class="baseline">baseline reference</span>'
        } else {
          ""
        }
        lines = c(
          lines,
          sprintf('<article class="run" id="%s">', html_escape(cell_id)),
          sprintf(
            "<h5>%s — Terrain-reference level %d: %s %s</h5>",
            html_escape(cell_id),
            terrain_level,
            html_escape(format_number(terrain_weight)),
            baseline_badge
          ),
          sprintf(
            paste0(
              '<p class="settings">maximum grade L%d=%s · ',
              'grade rate L%d=%s/m · continuation L%d=%s · ',
              'terrain L%d=%s</p>'
            ),
            maximum_grade_level,
            html_escape(format_percent(maximum_grade)),
            grade_rate_level,
            html_escape(format_number(grade_rate)),
            continuation_level,
            html_escape(format_percent(continuation)),
            terrain_level,
            html_escape(format_number(terrain_weight))
          ),
          '<div class="images">'
        )

        errors = character()
        for (camera_id in camera_ids) {
          render_row = latest_render_row(cell_id, camera_id)
          camera_label = camera_labels[[camera_id]]
          lines = c(
            lines,
            "<figure>",
            sprintf(
              "<figcaption>%s</figcaption>",
              html_escape(camera_label)
            )
          )
          succeeded = !is.null(render_row) &&
            isTRUE(render_row$render_succeeded[[1L]]) &&
            !is.na(render_row$image_path[[1L]]) &&
            file.exists(render_row$image_path[[1L]])
          if (succeeded) {
            image_path = relative_to_campaign(render_row$image_path[[1L]])
            lines = c(
              lines,
              sprintf(
                '<a href="%s"><img src="%s" loading="lazy" alt="%s %s candidate render"></a>',
                html_escape(image_path),
                html_escape(image_path),
                html_escape(cell_id),
                html_escape(camera_label)
              )
            )
          } else {
            error_message = if (
              !is.null(render_row) &&
                !is.na(render_row$error_message[[1L]])
            ) {
              render_row$error_message[[1L]]
            } else {
              "No completed candidate render is recorded."
            }
            errors = unique(c(errors, error_message))
            lines = c(
              lines,
              sprintf(
                '<div class="missing">Render unavailable<br>%s</div>',
                html_escape(error_message)
              )
            )
          }
          lines = c(lines, "</figure>")
        }
        lines = c(lines, "</div>")
        if (length(errors)) {
          lines = c(
            lines,
            sprintf(
              '<p class="failure">Swiss solve/render failure: %s</p>',
              html_escape(paste(errors, collapse = " | "))
            )
          )
        }
        lines = c(lines, "</article>")
      }
      lines = c(lines, "</section>")
    }
    lines = c(lines, "</section>")
  }
  lines = c(lines, "</section>")
}

lines = c(lines, "</main>", "</body>", "</html>")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, output_path, useBytes = TRUE)

successful_images = sum(
  manifest$scene_id == "swiss_switchback" &
    manifest$camera_id %in% camera_ids &
    manifest$render_role %in% c("baseline", "candidate") &
    manifest$render_succeeded %in% TRUE
)
message("Wrote ", normalizePath(output_path, winslash = "/", mustWork = TRUE))
message(
  "Factorial cells: ",
  nrow(design),
  "; available requested renders: ",
  successful_images
)
