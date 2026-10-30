# plate_picker_core.js is the browser's copy of the package's rules. These tests
# run it in Node and compare it with the R functions, so the two cannot drift.

core <- function(code) {
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  path <- system.file("htmlwidgets/lib/plate_picker/plate_picker_core.js", package = "mtpR")
  skip_if(!nzchar(path), "core file not found")
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  writeLines(c(sprintf("const core = require(%s);", jsonlite::toJSON(path, auto_unbox = TRUE)),
               sprintf("console.log(JSON.stringify((() => { %s })()));", code)), script)
  out <- system2("node", script, stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = TRUE)
}
js <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))

test_that("plate sizes and row labels match platesize_check()", {
  for (plate in c(6, 12, 24, 48, 96, 384, 1536)) {
    info <- platesize_check(plate)
    out <- core(sprintf("return core.plateInfo(%d);", plate))
    expect_equal(out$rows, info$rows)
    expect_equal(c(out$rowmax, out$colmax), c(info$rowmax, info$colmax))
    expect_equal(core(sprintf("return core.wellNames(core.plateInfo(%d));", plate)),
                 convert_well(seq_len(plate), "to_well", plate = plate))
  }
})

test_that("wells are read by the common rule of clean_wells()", {
  input <- c("A1", "a01", "A001", " A1", "A1 ", "A 1", "A-1", "A_1", "A:1", "H12", "h-12",
             "A13", "I1", "A0", "AA1", "A1B2", "1A", "R1C1", "Well A1", "A--1", "A1.0", "", "A")
  ours <- vapply(input, function(w) {
    tryCatch(clean_wells(w, plate = 96), error = function(e) NA_character_)
  }, character(1), USE.NAMES = FALSE)
  theirs <- core(sprintf("const i = core.plateInfo(96); return %s.map(w => core.cleanWell(w, i));", js(input)))
  expect_equal(theirs, ours)
  expect_equal(core("return core.cleanWell('AA1', core.plateInfo(1536));"), "AA01")
})

test_that("positions match convert_well() in both fill directions", {
  for (wise in c("row", "col")) {
    wells <- convert_well(1:96, "to_well", plate = 96)
    expect_equal(
      core(sprintf("const i = core.plateInfo(96); return Array.from({length: 96}, (_, n) => core.positionOf(n, i, '%s'));", wise)),
      convert_well(wells, "to_num", plate = 96, wise = wise)
    )
  }
})

test_that("sequence layouts match replicate_reactions()", {
  grid <- expand.grid(pattern = c("grouped", "dealt"), reps = c(1, 2, 3), inter = c(1, 2, 3),
                      intra = c(1, 2), wise = c("row", "col"), start = c("A01", "B03"),
                      reserved = c(FALSE, TRUE), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    reserved <- if (g$reserved) c("A02", "B04", "C01") else NULL
    r <- replicate_reactions(data.frame(rxn = 1:4), "rxn", num_replicates = g$reps,
                             pattern = g$pattern, inter_spacing = g$inter, intra_spacing = g$intra,
                             start_position = g$start, plate = 96, fill_wise = g$wise,
                             reserved = reserved)
    r <- r[order(r$rxn, r$replicate), ]
    expect_lte(max(r$Well.Index), 96)
    opts <- list(pattern = g$pattern, num_replicates = g$reps, inter_spacing = g$inter,
                 intra_spacing = g$intra, fill_wise = g$wise)
    wells <- js(as.list(c(g$start, reserved)))
    out <- core(sprintf(
      "const i = core.plateInfo(96), w = %s.map(x => core.wellIndex(x, i)), o = %s;
       o.start_position = w[0]; o.reserved = w.slice(1);
       const p = core.sequenceLayout(4, o, i).placed;
       p.sort((a, b) => a.reaction - b.reaction || a.replicate - b.replicate);
       return p.map(x => x.position);", wells, js(opts)))
    expect_equal(out, r$Well.Index, info = paste(g, collapse = " "))
  }
})

test_that("the R call the widget shows runs and gives the same wells", {
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  call <- core("const i = core.plateInfo(96);
    return core.sequenceCall(['S2', 'S10'], {pattern: 'dealt', num_replicates: 2, inter_spacing: 1,
      intra_spacing: 2, fill_wise: 'col', start_position: 9, reserved: [1]}, i);")
  r <- eval(parse(text = call))
  expect_equal(r$label[order(r$Well.Index)][1:2], c("S2", "S10"))
})

test_that("default colours match R", {
  feats <- list("a", "a", "a", NA, "b", NA, "a", "b")
  expect_equal(core(sprintf("return core.defaultColours(%s);", js(lapply(feats, function(g) if (is.na(g)) NULL else g)))),
               unname(picker_default_colours(unlist(feats))))
  expect_equal(core("return [0, 7, 8, 16, 23].map(core.paletteColour);"),
               vapply(c(1, 8, 9, 17, 24), picker_palette, character(1)))
})

test_that("tables are parsed, and a well column makes every other column a feature", {
  text <- "\ufeffWell,Control,Role,Well.Index,conc uM,replicate\nA1,\"a, b\",control,1,10,1\nb2,c,control,2,,1\n"
  out <- core(sprintf("const t = core.parseTable(%s); return {h: t.header, r: t.rows, roles: core.columnRoles(t)};", js(text)))
  expect_equal(out$h, c("Well", "Control", "Role", "Well.Index", "conc uM", "replicate"))
  expect_equal(out$r[1, 2], "a, b")
  expect_equal(out$roles, c("well", "feature", "feature", "ignore", "feature", "ignore"))
  expect_equal(core("return core.parseTable('well\\tlabel\\nA1\\tx\\n').rows[0];"), c("A1", "x"))

  plate <- place_controls(96, wells = "A01", name = "buffer")
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(plate, csv, row.names = FALSE)
  roles <- core(sprintf("return core.columnRoles(core.parseTable(%s));",
                        js(paste(readLines(csv), collapse = "\n"))))
  expect_equal(roles[names(plate) == "Well"], "well")
  expect_equal(roles[names(plate) == "Control"], "feature")

  roles <- core("return core.columnRoles(core.parseTable('label,feature,conc,lot\\nS1,std,1,x\\n'));")
  expect_equal(roles, c("label", "featurename", "number", "text"))
})

test_that("importing a layout gives each well one value per feature", {
  text <- "well,cell_line,treatment\na1,HeLa,Drug A\nA02,HeLa,\nI13,HEK,Drug B\nA03,HEK,Drug B\nA03,HEK,Drug C\nA04,,\n"
  out <- core(sprintf(
    "const t = core.parseTable(%s), r = core.columnRoles(t);
     const p = core.planImport(t, r, core.plateInfo(96));
     return {wells: p.placed.map(x => x.well), first: p.placed[0].values, second: p.placed[1].values,
             checks: p.checks.map(c => c.text).join(' | '), labels: p.labels.map(l => l.feature + ':' + l.name), ok: p.ok};", js(text)))
  expect_equal(out$wells, c("A01", "A02", "A03"))
  expect_equal(out$first, list(cell_line = "HeLa", treatment = "Drug A"))
  expect_equal(out$second, list(cell_line = "HeLa"))
  expect_equal(out$labels, c("cell_line:HeLa", "treatment:Drug A", "cell_line:HEK", "treatment:Drug B"))
  expect_match(out$checks, "I13")
  expect_match(out$checks, "later rows were skipped for: A03")
  expect_match(out$checks, "1 rows have no values")
  expect_true(out$ok)
})

test_that("importing a list of labels gives labels with attributes", {
  text <- "label,feature,lot\nHeLa,cell_line,7\nDrug A,treatment,\nDrug A,treatment,9\nNone,,\n"
  out <- core(sprintf(
    "const t = core.parseTable(%s), r = core.columnRoles(t);
     const p = core.planImport(t, r, core.plateInfo(96));
     return {kind: p.kind, labels: p.labels.map(l => l.feature + ':' + l.name), lot: p.labels[0].attrs.lot,
             checks: p.checks.map(c => c.text).join(' | ')};", js(text)))
  expect_equal(out$kind, "labels")
  expect_equal(out$labels, c("cell_line:HeLa", "treatment:Drug A", "label:None"))
  expect_equal(out$lot, 7)
  expect_match(out$checks, "first row was kept: Drug A")
})

test_that("fill follows one feature, and wells without it are NA", {
  out <- core("const labels = [
      {name: 'HeLa', feature: 'line', colour: '#0072B2', attrs: {lot: 3}},
      {name: 'Drug A', feature: 'drug', colour: '#E69F00', attrs: {lot: null}},
      {name: '100', feature: 'conc', colour: '#111111', attrs: {lot: null}},
      {name: '10', feature: 'conc', colour: '#222222', attrs: {lot: null}}];
    const features = {line: {}, drug: {}, conc: {}};
    const m = f => ({labels, features, attrTypes: {lot: 'number'}, fill: f});
    const line = core.fillScale(m('line')), conc = core.fillScale(m('conc')), lot = core.fillScale(m('lot'));
    const flat = core.fillScale({labels, features: {line: {}, drug: {}, conc: {scale: 'discrete'}}, attrTypes: {}, fill: 'conc'});
    return {pickLine: core.wellPick([0, 1], line), noLine: core.wellPick([1], line),
            pickDrug: core.wellPick([0, 1], core.fillScale(m('drug'))), concType: conc.type, low: conc.low,
            flatType: flat.type, flatColour: flat.colour[2], lot: core.wellPick([1, 0], lot)};")
  expect_equal(out$pickLine, 0)
  expect_equal(out$noLine, -1)
  expect_equal(out$pickDrug, 1)
  expect_equal(out$concType, "continuous")
  expect_equal(out$low, 10)
  expect_equal(out$flatType, "discrete")
  expect_equal(out$flatColour, "#111111")
  expect_equal(out$lot, 0)
})

test_that("changing the plate moves wells", {
  out <- core("const a = core.plateInfo(96), b = core.plateInfo(24), c = core.plateInfo(384);
    const w = Array.from({length: 96}, () => []); w[0] = [0]; w[6] = [1]; w[12] = [2];
    const pos = x => x.assign.map((v, i) => v.length ? i : -1).filter(i => i >= 0);
    const keep = core.movePlate(w, a, b, 'keep', 'row'), fill = core.movePlate(w, a, b, 'fill', 'row');
    const q = core.movePlate(w, a, c, 'quadrants', 'row');
    return {keep: pos(keep), lost: keep.lost, fill: pos(fill), q: pos(q), ok: core.canQuadrant(a, c), no: core.canQuadrant(a, b)};")
  expect_equal(out$keep, c(0, 6))   # A01 and B01; A07 is not on a 24-well plate
  expect_equal(out$lost, 6)
  expect_equal(out$fill, 0:2)
  expect_equal(out$q, sort(c(0, 1, 24, 25, 12, 13, 36, 37, 48, 49, 72, 73)))
  expect_true(out$ok); expect_false(out$no)
})

test_that("the CSV has one row per well and reads back into mtpR", {
  text <- core("return core.csvText({wells: ['A01', 'A02', 'B01'], assign: [[0, 2], [1], []],
    features: {cell_line: {}, treatment: {}},
    labels: [{name: 'HeLa', feature: 'cell_line', attrs: {lot: 1.5}}, {name: 'a, b', feature: 'cell_line', attrs: {lot: null}},
             {name: 'Drug A', feature: 'treatment', attrs: {lot: null}}],
    attrNames: ['lot']});")
  f <- tempfile(fileext = ".csv")
  writeLines(text, f, sep = "")
  d <- read_plate_picker(f)
  expect_equal(names(d), c("well", "cell_line", "treatment", "lot"))
  expect_equal(d$well, c("A01", "A02"))
  expect_equal(d$cell_line, c("HeLa", "a, b"))
  expect_equal(d$treatment, c("Drug A", NA))
  expect_equal(d$lot, c(1.5, NA))
  expect_s3_class(plot_plate(d, fill = "cell_line", well_id = "well", plate = 96), "ggplot")
  expect_equal(as.character(plate_picker(96, value = d)$x$value$treatment), c("Drug A", NA))
})

test_that("the image is one standalone SVG with no outside references", {
  svg <- core("return core.plateSVG({info: core.plateInfo(96), plate: 96, fill: 'conc', features: {x: {}}, attrTypes: {conc: 'number'},
      assign: Array.from({length: 96}, (_, i) => i < 2 ? [i] : []),
      labels: [{name: 'A<b', colour: '#0072B2', feature: 'x', attrs: {conc: 1}}, {name: 'y', colour: '#E69F00', feature: 'x', attrs: {conc: 8}}]},
      {title: 'T&T', legend: true, text: true, headers: true, width: 1200}).svg;")
  expect_match(svg, "^<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1200\"")
  expect_match(svg, "T&amp;T", fixed = TRUE)
  expect_false(grepl("href|<script|class=", svg))
  expect_true(grepl("#F4EFE6", svg, fixed = TRUE) && grepl("#2B7A8C", svg, fixed = TRUE))
})
