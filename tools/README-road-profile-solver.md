# Road profile solver engineering record

## Purpose

This document records which layered-road ideas have worked, which experiments
have regressed real scenes, and which acceptance checks are required before the
road profile solver is changed again.

The visual baseline is commit
`7f6581cb4cd57fd949160a7bbaab29099e802002`. The project owner identified this
as the last revision that produced generally acceptable Houston roads. It is
not perfect, but it is the recovery point. Do not use a newer revision as the
behavioral baseline merely because its engineering audit passes.

As of 2026-08-15:

- `7f6581cb4cd57fd949160a7bbaab29099e802002` is the accepted recovery baseline.
- `8eba747f` is a rejected profile experiment. Its road changes removed ground
  anchors, stopped pre-solve densification, and allowed the solve graph to
  propagate through surface continuations.
- The former uncommitted station-mode, protected-region, and terrain-contact
  work on top of `8eba747f` was rejected and removed on 2026-08-14. Only its
  lateral steep-terrain sampler remains. The full profile pipeline is not a
  safe foundation and must not be reconstructed from the cleanup backup.
- The 32-experiment campaign based at
  `b330380e2fa765ec2b814569bc66ea61708c990d` is complete and owner-reviewed.
  It does not justify integrating a behavioral profile change. RP-A06 is
  accepted as measurement infrastructure, and the existing B01 lateral
  high-side sampler is retained. All other production candidates are rejected
  or deferred as described below.
- The visual-completion record is commit `bb620a96` on
  `road-profile/visual-completion-20260815`. It uses fixed-camera
  `render_highquality()` evidence, including the Houston checkpoint centered
  at 29.762942, -95.372359.

The C++ mesh builder, RcppThread batching, mesh caching, stream work, color
handling, and rgl batching are separate from the profile regression. Do not
roll those performance and rendering improvements back when restoring the
profile behavior.

## What the Houston regression demonstrates

The failing scene is `houston-example.R`, using the same cached GeoDesk roads,
DEM, layer spacing of 5.5 m, vertical exaggeration of 1, and 1,216 rendered
paths for all three measurements below. Lane textures were disabled for the
numeric comparison because they do not affect profile heights.

| Revision | Path samples before meshing | Active fragments | Solve components | Maximum reported violation | Audit passed |
|---|---:|---:|---:|---:|:---:|
| `7f6581cb` baseline | 46,289 | 558 | 19 | 0.04897 m | yes |
| `8eba747f` no-anchor experiment | 6,850 | 1,211 | 1 | 0.04787 m | yes |
| Current station/contact experiment | 6,850 | 910 | 2 | 0.04910 m | yes |

The current station/contact diagnostics further report:

- 590 protected fragments;
- 812 fragments containing transition regions;
- 1,887 protected controls, 863 transition controls, and only 247 terrain
  controls;
- 100 ground-contact candidates but only 9 selected contacts; and
- 812 ungrounded transition fragments.

The current summary field named `ungrounded_control_count` reports zero for
this scene, despite the separate region diagnostic reporting 812 ungrounded
transitions. That field is therefore not an acceptance metric.

Exact selected road connections also regressed:

| Revision | Maximum endpoint height jump | 99th percentile | Jumps over 0.10 m | Jumps over 0.50 m | Jumps over 1 m |
|---|---:|---:|---:|---:|---:|
| `7f6581cb` baseline | 0.580 m | 0.111 m | 13 | 1 | 0 |
| `8eba747f` no-anchor experiment | approximately 0 | approximately 0 | 0 | 0 | 0 |
| Current station/contact experiment | 1.501 m | 0.339 m | 16 | 8 | 3 |

The no-anchor revision obtained perfect endpoint continuity by turning almost
the complete city network into one coupled solve. That is not success: it
moved 241 paths by more than 50 mm relative to the accepted baseline and
allowed interchange influence to propagate throughout the surface grid. The
current revision moved 300 paths by more than 50 mm, 81 by more than 250 mm,
and two by more than 1 m.

The four Houston snapshots supplied on 2026-08-14 show failure modes that the
reported engineering tolerance does not measure:

- terrain cutting rectangular holes through ordinary road decks;
- road pieces ending as vertical slabs or disconnected wedges;
- surface roads being lifted into ramps far outside the interchange core;
- abrupt profile jumps at otherwise continuous roads; and
- large interchange regions whose local layer ordering looks plausible while
  the approaches and surrounding roads are visibly wrong.

Conclusion: QP feasibility and a maximum constraint residual below 50 mm are
necessary diagnostics, not visual or geometric acceptance criteria.

## Proven core ideas

These ideas are present in or compatible with the accepted baseline and should
be retained during recovery.

### Geometry and metadata

- Transform geographic road input to an appropriate metric CRS before topology,
  width, distance, grade, or layer calculations.
- Preserve feature and fragment identity through clipping, splitting, and
  rendering.
- Interpret explicit OSM `layer`, `bridge`, `tunnel`, and `location` metadata
  conservatively. Inferred structure layers must remain distinguishable from
  explicit tags in diagnostics.
- Detect exact crossings, overlaps, and endpoint junctions before solving
  heights.
- Select through-road continuations conservatively using endpoint coincidence,
  direction, name/reference, lane evidence, and ambiguity rejection.

### Sparse local profile solving

- Seed the QP from actual layered crossings, overlaps, elevated structures,
  and explicit underground structures.
- Follow only selected physical continuations from those seeds.
- Stop propagation when a normal surface fragment supplies terminal ground
  context. An ordinary surface junction must not relay active membership into
  the surrounding street grid.
- Keep active context separate from expandable context. A fragment can supply
  a boundary height or junction equality without becoming a new source of
  graph expansion.
- Solve separate local components. A dense city should produce many small
  components, not one component containing nearly every road.
- Retain terminal surface anchor semantics: the terminal control must remain
  above terrain, the terrain-reference objective should pull it toward the
  surface, and its grade objective should approach the smoothed local terrain
  grade.

### Terrain and engineering constraints

- Densify and sample the terrain before constructing the profile reference.
  Mesh densification after the solve cannot reconstruct terrain information
  omitted from the profile.
- Keep a terrain floor for non-underground roads.
- Use support chords and terrain-relative objectives for actual elevated spans,
  not for the surrounding surface network.
- Enforce crossing clearance and layer order only where the corresponding
  physical event exists.
- Enforce longitudinal maximum grade, grade-rate/curvature, exact continuation
  height, and selected continuation grade compatibility.
- Retain continuous between-control auditing and adaptive refinement. Control
  points alone are insufficient to prove clearance or grade behavior.
- Apply effective z scaling consistently when converting between elevation
  units and scene coordinates.

### Meshing and rendering

- Build and cache road meshes during `render_roads()` so repeated
  `render_highquality()` calls do not rebuild them.
- Keep the batched C++ mesh construction and RcppThread job queue.
- Keep deterministic serial/threaded parity and source ordering.
- Keep rgl-only preview offset separate from the raytraced physical mesh
  elevation.

## Useful idea that must be isolated

The steep-terrain preprocessing added after the baseline materially improved
Swiss switchbacks. It samples several lateral positions across the road width,
chooses a continuous high-side-biased terrain trace, and smooths only difficult
runs. Preserve that algorithm as an isolated candidate preprocessor.

It is not evidence that the later topology or grounding design works. Before
reintroducing it:

1. Restore the baseline component and anchor behavior.
2. Run it only on ordinary terrain-following roads, without changing graph
   membership.
3. Confirm that easy/flat Houston paths are bit-for-bit unchanged.
4. Confirm the frozen Swiss switchback cameras improve or remain unchanged.
5. Measure the complete road footprint against terrain, not only the
   centerline.

A level cross-section is a desirable visual property, especially on steep
slopes, but setting `terrain_following = FALSE` for every layered road is not a
complete solution. A level deck needs a conservative full-width terrain
envelope or a local deck lift. Otherwise one edge of an ordinary surface road
can pierce a DEM even when the centerline is correctly placed.

The terrain-relative variable formulation introduced in `8eba747f` may still
be mathematically useful for vertical-exaggeration invariance. It was bundled
with graph expansion and anchor removal, so it has not been validated in
isolation. Treat it as unproven. Reintroduce it only after exact parity with the
absolute-height baseline on fixed problems.

## Rejected approaches

Do not repeat the following approaches without a new, isolated experiment that
first disproves the corresponding failure mechanism.

### Removing terminal ground anchors

Removing endpoint surface anchors and asking a weak terrain objective plus a
terrain floor to discover contact caused the Houston solve to absorb 1,211 of
1,216 paths into one component. A floor prevents underground placement but
does not establish where a structure ends or where its grade should return to
terrain.

### Walking every selected continuation

Allowing active membership to traverse selected continuations in both
directions through surface endpoints destroys sparsity. Continuation selection
answers “are these line fragments probably the same road?” It does not answer
“should an interchange profile solve control this entire road?” These must
remain separate decisions.

### Skipping pre-solve densification

The reduction from 46,289 terrain samples to 6,850 source vertices removed
terrain detail before the profile was formed. Densifying the finished mesh
only subdivides the already incorrect height interpolation. It does not add
the missing terrain constraints.

### Fragment-wide or heuristic protected regions

Growing a protected core through crossings, overlaps, links, handoffs, and
same-road continuations classified 590 Houston fragments as protected and 812
as transition-bearing. The result was two large components, not sparse local
interchange solves. OSM link-road membership is not a reliable boundary for a
vertical profile component.

### Post-hoc station contact search

The preliminary-solve/candidate-search/hard-contact-resolve design selected
only 9 contacts for 812 transition fragments in Houston. It also introduced a
second feasibility loop whose fallback can silently discard a contact. This is
too indirect and underconstrained to replace explicit local boundary context.

### Exact terrain caps selected by profile mode

Terrain caps are dangerous when mode classification is imperfect. A wrongly
classified station is forced into the DEM in the middle of a bridge or ramp,
while a wrongly protected station can float. The Swiss experiments exhibited
both outcomes. Ordinary roads should remain ordinary terrain paths outside the
local solver rather than become capped variables inside a large QP.

### Globally flat cross-sections from centerline height

Forcing every layered road to use a level cross-section eliminated independent
edge sampling but introduced terrain intersections where the DEM rises across
the road footprint. This is visible as holes in Houston. Flat cross-sections
need a footprint envelope and a small physical clearance policy.

### Excluding every solved fragment from endpoint repair

This assumes the profile QP enforces every connection used by mesh chaining.
The current implementation does not satisfy that assumption. Houston exact
connections now contain jumps up to 1.501 m. Protected joins must be solved
internally, but unsolved or omitted joins still require an explicit continuity
gate; they cannot simply be excluded from both mechanisms.

### Self-generated golden images

Do not generate a golden image from a new implementation and accept it in the
same validation run. A black difference image in that workflow only proves the
new image was copied successfully. It does not prove the behavior matches the
last good revision.

The first golden for a new scene must come from an independently identified
good revision, currently `7f6581cb`. Updating a golden requires human review
against that prior image and a written reason for the accepted difference.

## Recovery sequence

Use the following order. Do not attempt another comprehensive redesign before
the baseline behavior is restored.

1. Preserve the current work on a separate branch or patch for reference.
2. Restore the profile graph, anchor selection, pre-solve densification, and
   baseline C++ profile formulation from `7f6581cb` while retaining newer mesh
   caching, batching, rendering, and stream changes.
3. Re-run Houston with fixed cameras and establish baseline goldens rendered
   from `7f6581cb`.
4. Add full-width terrain-intersection tests and selected-connection continuity
   tests. Do not rely only on centerline constraints.
5. Confirm that the restored mixed old-profile/new-mesher pipeline matches the
   baseline coordinates and Houston renders.
6. Reintroduce the steep-terrain lateral sampler as one isolated change. It
   must not change easy Houston paths or active component membership.
7. Solve remaining bridge or switchback defects one local failure class at a
   time. Each experiment gets its own commit and before/after renders.

Do not restore the whole `render_roads.R` file from the old commit: that would
also discard later mesh and cache improvements. Restore or reimplement only the
profile-specific sections, with a frozen reference implementation available to
test numerical parity during the recovery.

## Mandatory acceptance gates

Every profile change must pass all of these gates before it can replace the
baseline.

### Houston: dense, mostly flat network

- Render the same four fixed interchange cameras at software-preview quality
  for routine tests and pathtraced quality for explicit review.
- No terrain may be visible through an ordinary road footprint.
- No selected physical continuation may have a visible gap or vertical end
  wall.
- No ordinary surface road may become an elevated approach merely because it
  is connected to the same named road as an interchange.
- Report active-fragment count, component count, largest-component size,
  endpoint height jumps, and full-width terrain clearance.
- Compare all metrics with `7f6581cb`, not only with the immediately preceding
  broken revision.

### Switzerland: steep terrain

- Preserve the accepted switchback shapes and level cross-sections.
- Inspect road edges for terrain clipping, not just centerlines.
- Verify explicit bridges and layered interchanges with fixed close cameras.
- Do not update the switchback goldens while changing interchange logic unless
  the switchback change is intentional and separately reviewed.

### Numerical and process gates

- Engineering audit passes at 50 mm.
- All mesh-chain connections have explicit continuity ownership: solver,
  terrain-only reconciliation, or a documented intentional separation.
- Serial and threaded meshes remain identical.
- No scene-wide solve component appears unless the input truly represents one
  inseparable structure.
- The ordinary terrain path entering the solver is sampled densely enough to
  represent DEM changes at the road-mesh resolution.
- Golden images are generated from the baseline before implementation and are
  not automatically accepted.
- A human inspects a contact sheet containing baseline, candidate, and amplified
  difference images.

## Experiment protocol for future work

For each profile experiment, append a short entry to this document containing:

- date and commit;
- one hypothesis;
- functions changed;
- Houston active/component/endpoint/terrain-clearance metrics;
- Swiss switchback and interchange results;
- links to baseline, candidate, and difference contact sheets; and
- decision: accepted, isolated for later, or rejected.

Change one profile concept per commit. A commit that simultaneously changes
terrain sampling, graph membership, constraints, and mesh interpretation cannot
identify which idea helped or failed and is not suitable for acceptance.

## Initial experiment log

### `7f6581cb` — accepted recovery baseline

- Works: local sparse components, conservative terminal surface context,
  pre-solve densification, terrain floors, structure clearance, and generally
  coherent Houston interchanges.
- Known limitations: steep terrain and switchbacks can follow the centerline DEM
  too literally; the baseline still has a small number of imperfect endpoint
  joins.
- Decision: accepted as the recovery point, not as a final solver.

### `8eba747f` — rejected

- Hypothesis: remove ground anchoring and formulate profiles relative to terrain
  so roads can return naturally under grade constraints.
- Result: 1,211 of 1,216 Houston paths entered one component; terrain sampling
  dropped from 46,289 to 6,850 path points; 241 paths moved by more than 50 mm
  from the accepted baseline.
- Decision: reject the integrated road-profile changes. Stream and mesh changes
  in the same commit must be evaluated separately and may be retained.

### 2026-08-14 station-mode/contact worktree — rejected as integrated design

- Hypothesis: protect interchange stations, create transition stations, search
  outward for feasible 50 mm terrain contacts, and use lateral terrain
  conditioning for difficult slopes.
- Result: Swiss switchbacks improved, but Houston retained 910 active fragments
  in two components; 812 transition fragments had only 9 selected contacts;
  endpoint jumps reached 1.501 m; terrain pierced road meshes; and the numerical
  audit still reported success.
- Decision: retain the lateral steep-terrain sampler as an isolated candidate.
  Reject the graph expansion, profile-mode, contact-search, global flat-deck,
  and acceptance methodology as a combined replacement for the baseline.

### 2026-08-14 cleanup — accepted cleanup, not a recovered solver

- Removed: station profile modes, protected-region growth, terrain-contact
  search and retry loops, terrain caps, post-solve endpoint reconciliation,
  native attachment auditing, Swiss self-generated goldens, and their
  experimental tools and tests.
- Retained: lateral cross-section sampling, high-side-biased path selection,
  and smoothing for paths classified as difficult.
- Isolation rule: an easy path returns its input coordinates unchanged. Only a
  difficult path opts out of independent mesh-edge terrain following.
- Houston check: 996 of 1,216 paths were classified easy and 220 difficult;
  637 final profiles remained within 1 mm of `8eba747f`. Further differences
  propagate because the committed no-anchor solver still places 1,211 paths in
  one component.
- Decision: accept this as code cleanup. It does not rehabilitate `8eba747f`;
  restoring the `7f6581cb` local graph and anchor behavior remains the next
  profile task.

### 2026-08-15 batched orthogonal campaign — owner-reviewed

The campaign tested 32 isolated ideas. Four branches changed production road
geometry: RP-B01, RP-B02, RP-B03, and RP-B05. Runtime fingerprints confirm that
each loaded a different implementation and produced road meshes different from
the campaign baseline. The other 28 experiments were intentionally
geometry-neutral diagnostics, reports, or deferred scaffolding; their exact
render parity is expected and is not evidence that the wrong branch was loaded.

The owner accepted the campaign's visual assessment:

- Switzerland is the primary behavioral test. None of the four production
  candidates materially improves the visible floating or terrain penetration
  in the Swiss scene.
- Houston is primarily a regression guard because its accepted behavior was
  already comparatively strong. Candidate changes there are neutral to
  slightly positive: some penetrations rise clear of terrain and some floating
  roads move downward. Those local improvements do not compensate for the
  Swiss, synthetic, locality, or numerical failures.
- RP-B05 is a decisive rejection. In the hidden-bump synthetic render it turns
  one continuous road deck into disconnected pieces with exposed end faces,
  even though the adaptive-station mechanism detects its narrow synthetic
  target.
- No behavioral candidate is approved for integration. RP-A06 is approved for
  integration because it corrects campaign audit units without changing
  geometry. The existing lateral high-side sampler is retained because its
  ablation makes steep terrain worse.

#### Experiment decisions

| ID | Idea tested | Result and lesson | Decision and reason |
|---|---|---|---|
| RP-A01 | Full-width road-footprint audit | Exposes terrain failures missed by centerline sampling; its original absolute values were superseded by A06's unit correction. | Retain as a diagnostic with A06 units; it measures a real acceptance condition but changes no solver behavior. |
| RP-A02 | Continuity ownership audit | Finds selected joins that are owned by neither the solver nor terrain reconciliation, including Houston and Swiss jumps over 50 mm. | Retain as a diagnostic; every mesh join needs one explicit owner before another continuity design is attempted. |
| RP-A03 | Distant locality perturbation | The selected perturbation stayed unchanged, but one negative test does not override the measured scene-scale active components. | Diagnostic only; do not claim locality from this test alone. |
| RP-A04 | Section-state metadata | Makes the current earthwork/structure classification observable, but the broken profile labels too much of the network as earthwork. | Retain observability only; exclude it as an input to terrain mutation until profile support is trustworthy. |
| RP-A05 | Component-influence measurement | Confirms that one solve can influence 154.895 km in Houston and 96.702 km in Switzerland. | Retain as a mandatory sparsity diagnostic; this identifies the dominant architectural failure. |
| RP-A06 | Physical-unit footprint audit | Corrects penetration, gap, length, and area measurements to physical metres and square metres with rendered parity. | Include in campaign infrastructure; it is geometry-neutral and makes the acceptance metrics meaningful. |
| RP-B01 | Remove the lateral high-side terrain sampler | The ablation worsens steep-terrain roads. Houston changes are not a reason to sacrifice the Swiss switchback improvement. | Reject the ablation and retain the current isolated sampler. Do not let that sampler alter graph membership or easy paths. |
| RP-B02 | Robust longitudinal terrain smoothing | Smooths its narrow target but erases real features and worsens Swiss gaps; visual Swiss behavior is materially unchanged. | Exclude; a terrain-reference filter cannot repair an overextended longitudinal solve. |
| RP-B03 | Full-width maximum terrain envelope | Reduces some penetration by raising the deck, but increases unsupported low-side gaps and moves large parts of Houston. | Exclude as a global reference. A footprint envelope may be reconsidered only inside a certified local support. |
| RP-B04 | Bounded crossfall reference | Produces plausible crossfall constraints but cannot repair incorrect longitudinal height. | Retain as a later cross-section diagnostic, not as the next solver change. |
| RP-B05 | Adaptive terrain stations | Detects a hidden synthetic bump, but worsens real profiles and visibly destroys the synthetic road mesh into slabs and gaps. | Exclude. Passing a narrow synthetic mechanism is insufficient when the production render regresses topology. |
| RP-C01 | Predict roadbed cut and fill | Quantifies the enormous terrain mutation required to conform terrain to the current bad profile. | Retain as a warning diagnostic; do not use its output to mutate terrain. |
| RP-C02 | Stamp a ground corridor under roads | Would conceal profile errors by moving terrain to globally wrong road heights. | Defer until section states and local profiles are certified; exclude from the current recovery. |
| RP-C03 | Ground crossfall comparison | Repeats B04's finding that crossfall cannot correct longitudinal placement. | Diagnostic only; no production integration. |
| RP-C04 | Lift an entire bridge path | Creates discontinuities at bridge landings because the approaches are not solved with the deck. | Reject direct whole-path lift; retain only the diagnostic demonstrating the need for joint support and landing solves. |
| RP-D01 | Finite terminal landing band | A finite return-to-ground region is preferable to a point snap, but the current compiler cannot provide certified anchors. | Include as a future design requirement; defer implementation until support selection is complete. |
| RP-D02 | Reachability-based structure support | The selector and rejection certificates work, but feasible contacts are incomplete: only 21 of 85 Houston and 50 of 124 Swiss bridge runs get both contacts. | Retain diagnostic machinery; defer behavioral use until search can cross source-chain boundaries and evaluate proposed core heights. |
| RP-D03 | Hard ground outside selected support | Works locally in synthetic cases but depends on a complete and correct support boundary. | Defer; hard grounding with an incomplete boundary recreates snaps and broken approaches. |
| RP-D04 | Explicit bridge-core support | Explicit metadata provides a sound seed, but approach contacts remain incomplete. | Retain the seed rule; defer the profile behavior until both landings are certified. |
| RP-D05 | Explicit tunnel support audit | Shows that audited tunnel paths currently pierce terrain, confirming a defect without supplying a complete correction. | Retain as a diagnostic; do not invert or ignore explicit tunnel metadata. |
| RP-D06 | Support regularization and length preference | Nearest-first selection already supplies the useful preference; regularization cannot compensate for missing feasible support. | Defer; avoid adding another objective before the support set is correct. |
| RP-D07 | Adaptive continuous profile controls | Continuous auditing remains valuable, and batching four refinement requests reduces solve passes without changing the intended result. | Retain the audit and batching optimization; it must never expand graph membership. |
| RP-D08a | Match terrain grade at a contact | Removes the tangent break at a certified point contact. | Retain as the point-boundary equation, but defer production use until the contact itself is certified. |
| RP-D08b | Damp the terrain grade | Creates a terminal kink by construction. | Exclude; damping does not represent a physically smooth landing. |
| RP-D08c | Zero relative grade | Is algebraically equivalent to terrain-grade matching under the current relative formulation. | Diagnostic notation only; do not maintain it as a separate behavioral option. |
| RP-D08d | Blend over a 20 m landing band | Is the strongest tested terminal policy and avoids an instantaneous snap. | Include in the future local-support design; defer until D02 can certify complete support and landing extent. |
| RP-E01 | Canonical endpoint height variables | Removes representational height disagreement for already selected continuations, but does not solve grade or missing ownership. | Retain as a future local formulation improvement; it must not broaden continuation topology. |
| RP-E02 | Terrain-relative parity | Confirms exact affine parity and that the equivalent formulation is already present. | Diagnostic only; no new behavior is needed. |
| RP-E03 | Lexicographic objectives | Priority ordering is meaningful only after the correct local support has been selected. | Defer; objective sophistication cannot repair the wrong solve domain. |
| RP-E04 | Vertical-exaggeration invariance | Physical centerlines remain invariant, while mesh thickness, offset, cleanup, or topology can change. | Reject the current mesh-invariance claim; retain separate centerline and physical-mesh audits. |
| RP-F01 | Enumerate local layer hypotheses | Correctly exposes ambiguous real metadata, but cannot rank hypotheses with the already-broken global profile. | Retain as diagnostic infrastructure; defer inference until local support profiles are feasible. |
| RP-F02 | Metadata-first bridge/tunnel ranking | Behaves correctly in isolation and keeps explicit tags primary, but depends on complete candidate profiles. | Retain the ranking policy; defer production inference until F01 candidates have certified local supports. |

#### Campaign lessons

- Fix solve-domain ownership before tuning terrain references. B02, B03, and
  B05 demonstrate that a locally sensible terrain algorithm becomes harmful
  when applied inside a city- or mountain-scale active component.
- A synthetic success is a mechanism check, not an acceptance result. A
  candidate must also preserve real Swiss terrain contact, Houston locality,
  connection continuity, and mesh topology.
- Penetration and low-side floating are paired footprint errors. Raising a
  centerline or using a maximum envelope may improve one while worsening the
  other. Report both in physical units.
- Numerical feasibility and a maximum constraint residual below 50 mm do not
  establish visual correctness. Slabs, wedges, gaps, topology loss, and distant
  road lifting require actual three-dimensional inspection.
- Every behavioral comparison must prove both code and geometry provenance.
  Record the loaded implementation fingerprint and cached road-mesh
  fingerprint; fail the harness when a production experiment loads baseline
  code or produces no changed mesh.
- Separate behavioral experiments from geometry-neutral diagnostics. Exact
  black differences are expected for the latter only after independent
  baseline/candidate construction and provenance validation.
- Use fixed-camera `render_highquality()` scenes with real terrain and the
  production `render_roads()` mesh. Houston comparison renders should omit lane
  textures so markings do not dominate pixel differences.
- Use full-resolution baseline, candidate, 32x magenta difference, and overlay
  panels plus hard-cut MP4 flips. Contact sheets are indexes, not substitutes
  for opening the source images.
- Keep the requested Houston regression checkpoint centered at 29.762942,
  -95.372359 with about a 500 m buffer. Keep Switzerland as the primary
  behavioral target and Houston as the guard against regressions.
- In the current rgl/rayshader camera convention, dividing the stored zoom by
  two zooms in. Do not “correct” `zoom / 2` by reversing it; camera changes must
  not be mixed with geometry comparisons.

#### Campaign evidence

- [Visual completion report](../road-profile-evidence/campaign-20260815/summary/visual-completion-report.md)
- [Implementation and road-mesh audit](../road-profile-evidence/campaign-20260815/summary/implementation-mesh-audit.csv)
- [Readable rendered contact sheets](../road-profile-evidence/campaign-20260815/summary/rendered-contact-sheet-index.md)
- [Baseline/candidate flip videos](../road-profile-evidence/campaign-20260815/summary/flip-video-index.md)
- [Complete numerical campaign report](../road-profile-evidence/campaign-20260815/summary/campaign-report.md)

These decisions supersede the preliminary recommendations in individual
experiment packets where their wording differs. They do not change the
recovery baseline: `7f6581cb` remains the last accepted road-profile behavior.

### 2026-08-15 terrain-adaptive maximum-grade experiments — rejected as standalone fixes

**Owner disposition:** recorded as excluded. The 12%, 30%, and terrain-following
maximum-grade policies are useful diagnostic mechanisms, but none is a viable
production change. Do not retry higher caps or an uncapped terrain-following
grade as a standalone Swiss fix.

Three isolated branches tested whether the 7% longitudinal grade limit is the
dominant cause of floating roads on steep terrain:

| ID | Behavior commit | Grade policy |
|---|---|---|
| RP-G01 | `0c00fcee` | 7% through a 7% terrain gradient, then a smoothstep increase to 12% at a 30% terrain gradient |
| RP-G02 | `2efcc506` | 7% through a 7% terrain gradient, then a smoothstep increase to 30% at a 30% terrain gradient |
| RP-G03 | `c5963d90` | 7% through a 7% terrain gradient, a smooth transition through 9%, then the physical terrain-gradient magnitude with no upper cap |

The common infrastructure at `cf074529` computes the physical DEM-gradient
magnitude at each profile station and compiles a local grade bound into the
native QP. It is independent of vertical exaggeration. The fixed 7% policy has
exact mesh parity with the pre-experiment Swiss, Houston interchange, and
Houston problem-overpass scenes. Grade-rate and curvature limits were left
unchanged so the experiment isolated the maximum-grade bound.

#### Results

- On a controlled 25% terrain plane, RP-G01 reduces but cannot eliminate the
  visible road-to-terrain gap. RP-G02 and RP-G03 place the ordinary surface road
  onto the plane while retaining clearance for the perpendicular bridge. The
  local grade-bound mechanism therefore works as designed.
- At the Houston regression checkpoint centered at 29.762942, -95.372359 with
  a 500 m buffer, all three policies are visually neutral. Lane textures were
  disabled. The 95th-percentile mesh displacement is approximately 0.0084
  render-coordinate units, the maximum is approximately 0.0639, and no
  horizontal coordinates change. No new gap, slab, wedge, penetration, or
  structure-order regression is visible.
- Switzerland is materially changed but not fixed. RP-G01 changes 72.25% of
  matched vertices, with a mean vertical movement of -0.893 and a maximum
  absolute movement of 6.988 render-coordinate units in the VE2 audit. RP-G02
  changes 72.49%, with mean -1.013 and maximum 8.628. RP-G03 changes 72.49%,
  with mean -1.005 and maximum 8.628.
- All six Swiss cameras show some roads moving closer to terrain, but the
  low-side, high-side, longitudinal, reverse-longitudinal, and worst-station
  views retain conspicuous detached shadows and unsupported road sections.
  RP-G02 and RP-G03 are visually very similar; removing the grade cap does not
  unlock a qualitatively better profile.

#### Lessons and decision

- The maximum-grade constraint is active, but it is not the primary limiting
  factor in the Swiss failure. Relaxing it alone cannot establish terrain
  contact while grade-rate, curvature, topology coupling, and the terrain
  objective continue to determine the profile.
- A terrain-gradient magnitude measured in any direction also loosens the
  longitudinal road grade on a strong cross-slope. This matches the experiment
  request, but it can grant freedom where the terrain slope along the road is
  mild and contributes to broad, nonlocal Swiss movement.
- The synthetic result is a mechanism pass, not a production acceptance result.
  Houston stability is necessary but Switzerland remains the primary behavioral
  target.
- Reject RP-G01, RP-G02, and RP-G03 as standalone production fixes. Preserve
  the branches and the local-bound infrastructure as diagnostic evidence. If
  this direction is revisited, test a tangent-projected terrain gradient only
  after solve-domain ownership and a reliable terrain-contact objective are in
  place.

Each branch contains a packet under
`road-profile-evidence/campaign-20260815/packet-G/<experiment-id>/` with ten
full-resolution baseline/candidate/magenta-difference comparisons, ten MP4
flip videos, native-resolution contact sheets, a render manifest, mesh metrics,
and the completed visual inspection. The final evidence tips are `c8cfc936`
(RP-G01), `b0bf4dc5` (RP-G02), and `03ae83e1` (RP-G03).

### 2026-08-16 four-factor terrain-adaptive relaxation screen — numerical pre-screen only

RP-H01 tested the four remaining profile inputs identified after the
maximum-grade campaign: maximum grade rate, curvature regularization,
continuation-grade coupling, and terrain-reference weight. Maximum road grade
remained fixed at 7% so the screen isolated these factors. Each relaxation was
inactive through a 7% physical terrain gradient and reached its selected target
at 30% using a quintic smoothstep.

The user described the screen as 4^3: four factors at three levels. The complete
factorial is therefore 3^4 = 81 cells.

| Factor | Baseline | Moderate relaxation | Strong relaxation |
|---|---:|---:|---:|
| maximum grade rate | 0.001/m | 0.005/m | 0.020/m |
| curvature weight | 100 | 10 | 0 |
| continuation grade tolerance | 0 | 0.03 | 0.14 |
| terrain-reference weight | 0.001 | 0.03 | 1 |

The behavior commit is `4463f2e6`; the preserved preliminary evidence tip is
`81e52f08` on `road-profile/RP-H01-four-factor-screen`. Baseline settings have
exact coordinate and mesh parity with the campaign base. All 81 cells were
evaluated numerically through the production profile and road-mesh path on the
frozen Swiss switchback and Houston problem-overpass crops at vertical
exaggeration 2.

#### Numerical pre-screen results

- The campaign attempted 162 scene solves. 116 succeeded and 46 failed; only
  37 cells succeeded in both scenes.
- No successful cell reduced the Swiss p90 low-side gap. Every successful gap
  ratio remained 1.000000 within numerical noise.
- Only the baseline and two curvature-only cells passed the Houston displacement
  guard. The strongest safe curvature-only cell, H10, has a Swiss score of
  1.000000 and Houston p95/maximum displacement of 0.0026/0.0104 m.
- Moderate grade-rate relaxation alone, H04, changes the profiles but not in the
  desired direction: Swiss p90 gap ratio stays 1.000000, penetration ratio rises
  to 1.0538, penetration-area ratio rises to 1.1545, and Houston p95/maximum
  displacement reaches 0.190/0.434 m.
- Moderate and strong continuation-grade relaxation both worsen the best
  attainable Swiss score and eliminate every Houston guard pass.
- Strengthening the terrain-reference objective is numerically unstable. At the
  strong level, 0 of 27 Swiss cells solve; at the moderate level, only 10 of 27
  solve. In total the Swiss crop accounts for 44 of the 46 failures, primarily
  OSQP infeasibility/inaccurate-infeasibility statuses and iteration limits.
- The numerical ranking did not identify a cell that both improved its Swiss
  metrics and passed the Houston displacement guard.

These statements describe numerical metrics and solver outcomes only. They are
not a visual rejection of the 81 cells. In particular, a numerically weak cell
may still improve an important visible region, while a numerically strong cell
may introduce a visible slab, wedge, gap, penetration, or distant regression.

#### Incomplete visual pilot

Only H00, H10, and H04 were independently rendered through
`render_highquality()` using production terrain and road meshes. The pilot
opened 24 full-resolution source renders covering two of the 80 candidate
cells. It therefore cannot support a visual conclusion about 81 factorial
cells, and it does not satisfy the campaign's visual-evidence rules.

- H10 is genuinely neutral, not an accidental duplicate: Swiss and Houston
  road-mesh fingerprints differ from H00, while the synthetic fingerprint is
  identical. The Swiss difference contains only sparse, subpixel road-edge
  movement and does not close visible low-side gaps.
- H04 visibly moves broad parts of Houston. Its magenta difference outlines
  nearly the entire road network, while the Swiss low-side views retain the
  same unsupported sections. This agrees with the displacement and footprint
  audits.
- These observations apply only to H10 and H04. They must not be generalized to
  the unrendered cells.

#### Invalidated conclusion and completion requirement

- The prior label "rejected" and the prior instruction not to search these
  combinations were unsupported. They are withdrawn.
- Numerical solve failures remain real evidence, and a cell that cannot build a
  scene may be recorded as `blocked_before_render`. Numerical failure is not a
  substitute for rendering every cell that can construct a scene.
- No factor, interaction, or exclusion boundary from this factorial may be
  accepted until the complete fixed-camera visual sensitivity campaign is
  rendered and reviewed.
- Switzerland remains the primary target. Houston interchange and the Houston
  problem overpass are regression controls, not substitutes for inspecting
  Swiss low-side, high-side, longitudinal, and worst-station views.
- The successful lateral terrain sampling remains independent accepted work;
  this incomplete factorial neither validates nor invalidates it.

The preliminary packet is stored on the experiment branch under
`road-profile-evidence/campaign-20260815/packet-H/RP-H01-four-factor-screen/`.
Its factor design, numerical scene results, main-effect tables, and failure
summary remain useful. Its render and visual-inspection records cover only the
H10 and H04 pilot and are explicitly incomplete.

The completion driver is
[`run-road-profile-visual-screen.R`](run-road-profile-visual-screen.R), with
operating instructions in
[`README-road-profile-visual-screen.md`](README-road-profile-visual-screen.md).
It loads the working package once, parameterizes all four inputs at runtime,
renders all buildable cells with fixed production scenes and cameras, writes
32x magenta differences and flip MP4s, and leaves the visual verdict to the
reviewer. Until that visual campaign is complete, RP-H01 is `inconclusive`, not
rejected or recommended.

### 2026-08-16 revised visual screen — absolute grade replaces curvature

The visual-completion design was revised before its full render campaign. The
original RP-H01 evidence above remains preserved as a numerical pre-screen; it
must not be mixed with the revised design. The key defect in RP-H01 was that
maximum absolute road grade remained fixed at 7% in all 81 cells. Varying grade
rate could let the profile change slope more quickly, but it could never follow
terrain whose required longitudinal grade exceeded that hard bound.

The revised screen holds curvature weight at the production value of 100 and
replaces curvature relaxation with a steep-terrain maximum-grade factor:

| Factor | Baseline | Moderate relaxation | Strong relaxation |
|---|---:|---:|---:|
| maximum grade rate | 0.001/m | 0.005/m | 0.020/m |
| maximum absolute grade | 7% | 15% | unbounded |
| continuation grade tolerance | 0 | 0.03 | 0.14 |
| terrain-reference weight | 0.001 | 0.03 | 1 |

All four factors retain the existing terrain-gradient activation: baseline
settings apply through a 7% physical terrain gradient, and the selected target
is fully active at 30%, with quintic smoothstep interpolation between them. A
finite maximum-grade target is interpolated directly. The unbounded target
smoothly relaxes reciprocal constraint strength, reaching no hard grade bound
at full relaxation. Height continuity, grade-rate constraints, curvature
regularization, and the terrain floor remain active independently.

This is a new 3^4 design and requires a new campaign output directory. No
result from the incomplete RP-H01 visual pilot is attributed to a revised
cell. The revised screen remains `inconclusive` until its buildable cells have
been rendered and visually reviewed under the fixed Swiss and Houston cameras.

### 2026-08-17 revised visual screen — H19 accepted

**Owner disposition:** accept H19 as the production default. The complete
nested factorial gallery was reviewed as three-dimensional rendered evidence,
with Switzerland as the primary behavior scene and Houston as the regression
guard.

H19 uses:

| Setting | Level | Accepted value |
|---|---:|---:|
| steep-terrain maximum grade rate | 0 | 0.001/m |
| steep-terrain maximum absolute grade | 1 | 15% |
| steep-terrain continuation grade tolerance | 2 | 14% |
| steep-terrain terrain-reference weight | 0 | 0.001 |

#### Visual result

- A 7% maximum grade leaves substantial Swiss road sections floating. The
  low-resolution DEM omits important terrain features that are visible in
  three-dimensional satellite imagery, so a road that follows its real-world
  alignment may need a steeper profile than the raster alone suggests.
- A 15% steep-terrain maximum grade is a large visual improvement. It gives the
  profile enough freedom to follow the plausible real road position without
  giving the coarse DEM complete control over the road shape.
- An unbounded maximum grade is too permissive. Roads can bend back toward
  individual coarse terrain samples, producing visibly less plausible
  profiles even when the measured terrain separation becomes smaller.
- A 14% continuation-grade tolerance is the other clear winner. In combination
  with the 15% maximum grade, it avoids forcing adjacent steep-terrain road
  fragments to share an overly restrictive tangent while retaining exact
  continuation height. The resulting switchbacks remain much closer to the
  terrain.
- The accepted grade-rate and terrain-reference values remain at 0.001. The
  visual screen does not justify increasing either one.
- The reviewed Houston scenes are neutral to slightly positive and show no
  material regression that outweighs the Swiss improvement.

#### Decision and implementation rule

- Use a 15% maximum grade and 14% continuation-grade tolerance at full
  steep-terrain relaxation. Keep the 7% ordinary-road maximum and exact
  continuation grade through a 7% physical terrain gradient, then interpolate
  smoothly to the accepted targets at a 30% terrain gradient.
- Keep maximum grade rate at 0.001/m and terrain-reference weight at 0.001.
- Expose all four relaxed targets in `render_roads()` so users can tune scenes
  whose DEM resolution or road standards differ. The runtime screening option
  remains an experiment-harness override, not the public interface.
- Do not interpret minimum separation from a coarse DEM as ground truth. Visual
  plausibility must account for terrain features absent from the raster; an
  unbounded solver can overfit the same missing-data problem in the opposite
  direction.

This decision supersedes the revised screen's `inconclusive` status. It does
not invalidate the earlier finding that maximum-grade relaxation alone was
insufficient: the accepted behavior depends on its interaction with relaxed
continuation-grade compatibility.

### 2026-08-17 global 2x2 confirmation — accepted and simplified

**Owner disposition:** use a global 15% maximum grade and a global 14%
continuation-grade tolerance. This result supersedes the smooth terrain-gradient
activation described in the H19 section above.

The confirmation campaign removed the smooth change with terrain slope and
rendered the complete two-factor design:

| Cell | Maximum grade | Continuation-grade tolerance |
|---|---:|---:|
| H00 | 7% | 0% |
| H03 | 15% | 0% |
| H07 | 7% | 14% |
| H19 | 15% | 14% |

Maximum grade rate and terrain-reference weight remained fixed at their
established `0.001` values. The campaign generated 56 production
`render_highquality()` scenes across the Swiss switchback, Houston interchange,
and Houston problem-overpass checkpoints. It used fixed cameras, lane-free
Houston roads, magenta differences, and baseline/candidate flip videos. The
nested comparison is recorded in
`/Users/tyler/Desktop/R/road-profile-global-2x2/global-factorial-effects.html`.

#### Visual result

- The owner found no regression in either Houston checkpoint. Houston remains
  the regression guard because its layered roads were already generally sound.
- The global H19 combination is a large improvement in the Swiss switchback
  scene. It follows the plausible real road alignment much more closely where
  the coarse DEM omits important terrain features.
- The 7% bound still leaves Swiss roads visibly detached from terrain. A global
  15% bound supplies the needed longitudinal freedom without the visibly
  implausible bending observed in the earlier unbounded experiment.
- Allowing a 14% grade mismatch at selected continuations complements the 15%
  grade bound. Continuation height remains exact, but adjacent fragments are no
  longer forced to share an unrealistic tangent on coarse mountainous terrain.
- Smooth activation by DEM-gradient magnitude did not add a visible benefit in
  the accepted scenes. It also relaxed longitudinal behavior in response to
  cross-slope, which is not necessary for the accepted global result.
- The earlier grade-rate, curvature, and terrain-reference factor levels did not
  produce a meaningful visual improvement. They remain fixed solver policies,
  not public road-rendering controls.

#### Implementation rule

- Apply `maximum_grade = 0.15` globally by default.
- Apply `continuation_grade_tolerance = 0.14` globally by default while keeping
  continuation height exact.
- Expose only these two supported controls in `render_roads()`. Users may select
  stricter values for a particular scene, but the unproductive grade-rate and
  terrain-reference variants are not public options.
- Remove the terrain-gradient calculation, quintic smoothstep, per-station
  relaxation vector, relaxed-setting aliases, and native setting interpolation.
  They are excluded because the accepted visual result does not depend on them.
- Retain the independently accepted lateral high-side terrain sampling. It is
  responsible for the switchback cross-slope improvement and is not part of the
  removed longitudinal relaxation machinery.

This is the current production decision. Future changes must compare against
the global H19 behavior in Switzerland and must continue to show no Houston
regression through actual three-dimensional renders.
