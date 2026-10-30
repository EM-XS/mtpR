/* mtpR plate_picker core: the rules the widget shares with the R functions.
 * No DOM here, so tests/testthat/test-plate_picker_core.R can run it in Node and
 * compare it with platesize_check(), clean_wells() and replicate_reactions(). */
(function (root, factory) {
  var core = factory();
  if (typeof module !== 'undefined' && module.exports) module.exports = core;
  else root.mtprPlateCore = core;
})(typeof self !== 'undefined' ? self : this, function () {
  'use strict';

  // As in platesize_check().
  var SIZES = { 6: [2, 3], 12: [3, 4], 24: [4, 6], 48: [6, 8], 96: [8, 12], 384: [16, 24], 1536: [32, 48] };
  // As in .picker_palette (R/plate_picker.R).
  var PALETTE = ['#0072B2', '#E69F00', '#009E73', '#D55E00', '#56B4E9', '#CC79A7', '#F0E442', '#7F7F7F'];
  // As in plot_plate(): the continuous scale and its NA colour (grey70).
  var FILL_LOW = '#F4EFE6', FILL_HIGH = '#2B7A8C', FILL_NA = '#B3B3B3';
  var EMPTY_FILL = '#F6F7F9', EMPTY_EDGE = '#D5D9DF', INK = '#1C1F24';
  var CELL = 30, HEAD = 22;

  // ---- plates and wells ---------------------------------------------------
  function rowLabels(n) {
    var out = [];
    for (var i = 0; i < n; i++) {
      out.push(i < 26 ? String.fromCharCode(65 + i)
        : String.fromCharCode(65 + Math.floor(i / 26) - 1) + String.fromCharCode(65 + i % 26));
    }
    return out;
  }
  function plateInfo(plate) {
    var d = SIZES[plate];
    if (!d) throw new Error('Unsupported plate size: ' + plate);
    return { plate: +plate, rows: rowLabels(d[0]), rowmax: d[0], colmax: d[1] };
  }
  function pad2(n) { return n < 10 ? '0' + n : '' + n; }
  function wellNames(info) {
    var out = [];
    for (var r = 0; r < info.rowmax; r++) for (var c = 0; c < info.colmax; c++) out.push(info.rows[r] + pad2(c + 1));
    return out;
  }
  // The "common" rule of clean_wells(): parse_wells() in R/aaa-utils.R.
  function parseWell(s) {
    var m = /^\s*([A-Za-z]+)[\s_:-]?([0-9]+)\s*$/.exec(String(s));
    return m ? { row: m[1].toUpperCase(), col: Number(m[2]) } : null;
  }
  // Index on the plate (row by row), or -1 when the label is not on it.
  function wellIndex(s, info) {
    var p = parseWell(s);
    if (!p) return -1;
    var r = info.rows.indexOf(p.row);
    if (r < 0 || p.col < 1 || p.col > info.colmax) return -1;
    return r * info.colmax + p.col - 1;
  }
  // clean_wells(mode = "common") for one label; null when it is not a well on the plate.
  function cleanWell(s, info) {
    var i = wellIndex(s, info);
    return i < 0 ? null : info.rows[Math.floor(i / info.colmax)] + pad2(i % info.colmax + 1);
  }
  // convert_well(): 1-based position in `wise` order <-> row-wise well index.
  function positionOf(i, info, wise) {
    if (wise === 'col') return (i % info.colmax) * info.rowmax + Math.floor(i / info.colmax) + 1;
    return i + 1;
  }
  function indexOfPosition(p, info, wise) {
    var q = p - 1;
    if (wise === 'col') return (q % info.rowmax) * info.colmax + Math.floor(q / info.rowmax);
    return q;
  }

  // ---- sequence layout ----------------------------------------------------
  // offsets_grouped() and offsets_dealt() in R/assign_reaction_positions.R, for
  // one replicate count. `reaction` and `replicate` are 0-based.
  function offsets(n, opts) {
    var out = [], cur = 0, reps = opts.num_replicates, rx, k;
    if (opts.pattern === 'dealt') {
      for (k = 0; k < reps; k++) {
        for (rx = 0; rx < n; rx++) { out.push({ reaction: rx, replicate: k, offset: cur }); cur += opts.inter_spacing; }
        cur += opts.intra_spacing - opts.inter_spacing;
      }
    } else {
      for (rx = 0; rx < n; rx++) {
        for (k = 0; k < reps; k++) { out.push({ reaction: rx, replicate: k, offset: cur }); cur += opts.intra_spacing; }
        cur += opts.inter_spacing - opts.intra_spacing;
      }
    }
    return out;
  }
  // What replicate_reactions() places on one plate: `opts` holds pattern,
  // num_replicates, inter_spacing, intra_spacing, start_position (a well index),
  // fill_wise and reserved (well indices). Wells past the end of the plate do
  // not move on to a next plate; they are counted in `lost`.
  function sequenceLayout(n, opts, info) {
    var size = info.rowmax * info.colmax, reserved = {};
    (opts.reserved || []).forEach(function (i) { reserved[positionOf(i, info, opts.fill_wise)] = true; });
    var start = positionOf(opts.start_position, info, opts.fill_wise), free = [];
    for (var p = start; p <= size; p++) if (!reserved[p]) free.push(p);
    var placed = [], lost = 0;
    offsets(n, opts).forEach(function (o) {
      if (o.offset >= free.length) { lost++; return; }
      var position = free[o.offset];
      placed.push({ reaction: o.reaction, replicate: o.replicate, position: position,
                    index: indexOfPosition(position, info, opts.fill_wise) });
    });
    return { placed: placed, lost: lost };
  }
  // The same layout as an R call, using replicate_reactions()'s own argument names.
  function sequenceCall(names, opts, info) {
    var quote = function (s) { return JSON.stringify(String(s)); };
    var well = function (i) { return quote(cleanWell(wellNames(info)[i], info)); };
    var args = [
      'data.frame(rxn = 1:' + names.length + ', label = c(' + names.map(quote).join(', ') + '))', '"rxn"',
      'num_replicates = ' + opts.num_replicates, 'pattern = ' + quote(opts.pattern),
      'inter_spacing = ' + opts.inter_spacing, 'intra_spacing = ' + opts.intra_spacing,
      'start_position = ' + well(opts.start_position), 'plate = ' + info.plate,
      'fill_wise = ' + quote(opts.fill_wise)
    ];
    if (opts.reserved && opts.reserved.length) args.push('reserved = c(' + opts.reserved.map(well).join(', ') + ')');
    return 'replicate_reactions(' + args.join(', ') + ')';
  }

  // ---- changing the plate -------------------------------------------------
  // True when each well can become a 2 x 2 square, as in well_quadrant().
  function canQuadrant(from, to) {
    return to.rowmax === 2 * from.rowmax && to.colmax === 2 * from.colmax;
  }
  // Moves `assign` (one array per well) from one plate to another. Methods:
  // "keep" keeps row and column, "fill" packs wells from the start of the plate
  // in `wise` order, "quadrants" turns each well into a 2 x 2 square.
  function movePlate(assign, from, to, method, wise) {
    var size = to.rowmax * to.colmax, out = [], lost = [], i;
    for (i = 0; i < size; i++) out.push([]);
    var used = [];
    for (i = 0; i < assign.length; i++) if (assign[i].length) used.push(i);
    if (method === 'fill') {
      used.sort(function (a, b) { return positionOf(a, from, wise) - positionOf(b, from, wise); });
      used.forEach(function (w, n) {
        if (n < size) out[indexOfPosition(n + 1, to, wise)] = assign[w].slice(); else lost.push(w);
      });
    } else {
      used.forEach(function (w) {
        var r = Math.floor(w / from.colmax), c = w % from.colmax;
        if (method === 'quadrants') {
          [[0, 0], [0, 1], [1, 0], [1, 1]].forEach(function (d) {
            out[(2 * r + d[0]) * to.colmax + 2 * c + d[1]] = assign[w].slice();
          });
        } else if (r < to.rowmax && c < to.colmax) out[r * to.colmax + c] = assign[w].slice();
        else lost.push(w);
      });
    }
    return { assign: out, lost: lost };
  }

  // ---- colours ------------------------------------------------------------
  function rgbOf(hex) { var n = parseInt(hex.slice(1), 16); return [n >> 16, (n >> 8) & 255, n & 255]; }
  function hexOf(rgb) {
    return '#' + rgb.map(function (v) {
      var s = Math.floor(v + 0.5).toString(16).toUpperCase();
      return s.length < 2 ? '0' + s : s;
    }).join('');
  }
  function mix(a, b, t) {
    var p = rgbOf(a), q = rgbOf(b);
    return hexOf([0, 1, 2].map(function (i) { return p[i] + (q[i] - p[i]) * t; }));
  }
  function inkFor(hex) {
    var c = rgbOf(hex);
    return 0.299 * c[0] + 0.587 * c[1] + 0.114 * c[2] > 140 ? INK : '#FFFFFF';
  }
  // The palette, then ever lighter rounds of it. picker_palette() in R.
  function paletteColour(i) {
    return mix(PALETTE[i % PALETTE.length], '#FFFFFF', 1 - Math.pow(0.65, Math.floor(i / PALETTE.length)));
  }
  // A feature takes one palette colour, and its labels run dark to light through
  // it. Labels with no feature (null) each take their own palette colour.
  // picker_default_colours() in R.
  function defaultColours(features) {
    var units = [], where = {}, out = [];
    var keyOf = function (f, k) { return f == null || f === '' ? 'label:' + k : 'feature:' + f; };
    features.forEach(function (f, k) {
      var key = keyOf(f, k);
      if (where[key] == null) { where[key] = units.length; units.push(0); }
      units[where[key]]++;
    });
    var seen = units.map(function () { return 0; });
    features.forEach(function (f, k) {
      var u = where[keyOf(f, k)];
      var base = paletteColour(u), m = units[u], j = seen[u]++, t = m > 1 ? j / (m - 1) : 0.5;
      out.push(t < 0.5 ? mix(base, '#000000', 0.45 * (1 - 2 * t)) : mix(base, '#FFFFFF', 0.75 * (2 * t - 1)));
    });
    return out;
  }
  function nextColour(used) {
    for (var i = 0; i < 1000; i++) {
      var c = paletteColour(i);
      if (used.indexOf(c) < 0) return c;
    }
    return PALETTE[0];
  }

  function isNumber(s) { return s !== '' && s != null && isFinite(Number(s)); }
  // What each label is drawn in, like the `fill` and `scale` of plot_plate().
  // model: labels (records with name, feature, colour, attrs), features (settings
  // by name), attrTypes, fill, log. `fill` is a feature or an attribute.
  function fillScale(model) {
    var labels = model.labels, fill = model.fill, isFeature = !!model.features[fill];
    var out = { fill: fill, isFeature: isFeature, type: 'discrete', keys: [] };
    var value = function (l) {
      var v = isFeature ? (l.feature === fill ? l.name : null) : l.attrs[fill];
      return v == null || v === '' ? null : v;
    };
    var values = labels.map(value), present = values.filter(function (v) { return v != null; });
    var numeric = isFeature
      ? model.features[fill].scale !== 'discrete' && present.length > 0 && present.every(isNumber)
      : model.attrTypes[fill] === 'number';
    if (numeric && present.length) {
      var nums = present.map(Number);
      var lo = Math.min.apply(null, nums), hi = Math.max.apply(null, nums);
      var log = !!model.log && lo > 0;
      var at = function (v) {
        if (hi === lo) return 0.5;
        return log ? (Math.log(v) - Math.log(lo)) / (Math.log(hi) - Math.log(lo)) : (v - lo) / (hi - lo);
      };
      out.type = 'continuous'; out.low = lo; out.high = hi; out.log = log;
      out.colour = values.map(function (v) { return v == null ? null : mix(FILL_LOW, FILL_HIGH, at(Number(v))); });
      out.text = values.map(function (v) { return v == null ? '' : String(v); });
      return out;
    }
    out.text = labels.map(function (l, k) { return values[k] == null ? '' : l.name; });
    if (isFeature) {
      out.colour = labels.map(function (l, k) { return values[k] == null ? null : l.colour; });
      labels.forEach(function (l, k) { if (values[k] != null) out.keys.push({ name: l.name, colour: l.colour, k: k }); });
      return out;
    }
    var keys = [];
    present.forEach(function (v) { if (keys.indexOf(String(v)) < 0) keys.push(String(v)); });
    out.keys = keys.map(function (name, i) { return { name: name, colour: paletteColour(i) }; });
    out.colour = values.map(function (v) { return v == null ? null : paletteColour(keys.indexOf(String(v))); });
    return out;
  }
  // The label that sets a well's colour, or -1 when a painted well has no value
  // for `fill` (drawn in the NA colour, as plot_plate() would draw it).
  function wellPick(a, scale) {
    for (var n = 0; n < a.length; n++) if (scale.colour[a[n]] != null) return a[n];
    return -1;
  }

  // ---- reading tables -----------------------------------------------------
  function parseTable(text) {
    text = String(text).replace(/^﻿/, '');
    var first = text.split(/\r?\n/, 1)[0], delim = ',', best = 0;
    [',', '\t', ';'].forEach(function (d) {
      var n = first.split(d).length - 1;
      if (n > best) { best = n; delim = d; }
    });
    var rows = [], row = [], cell = '', quoted = false;
    function endCell() { row.push(cell); cell = ''; }
    function endRow() { endCell(); rows.push(row); row = []; }
    for (var i = 0; i < text.length; i++) {
      var ch = text[i];
      if (quoted) {
        if (ch === '"') { if (text[i + 1] === '"') { cell += '"'; i++; } else quoted = false; } else cell += ch;
      } else if (ch === '"' && cell === '') quoted = true;
      else if (ch === delim) endCell();
      else if (ch === '\r') { if (text[i + 1] === '\n') i++; endRow(); }
      else if (ch === '\n') endRow();
      else cell += ch;
    }
    if (cell !== '' || row.length) endRow();
    rows = rows.filter(function (r) { return r.some(function (c) { return c.trim() !== ''; }); });
    if (!rows.length) return { header: [], rows: [] };
    var header = rows[0].map(function (c) { return c.trim(); });
    return {
      header: header,
      rows: rows.slice(1).map(function (r) { return header.map(function (_, j) { return r[j] == null ? '' : r[j].trim(); }); })
    };
  }
  // Column names used across mtpR: plate_picker exports, place_controls() and
  // plate_roles() (Well), design_plate() (Well.Position) and replicate_reactions()
  // (replicate, position, which describe the layout, not a well's contents).
  var WELL_NAMES = ['well', 'wellposition', 'wellid'];
  var LAYOUT_NAMES = ['row', 'column', 'wellindex', 'position', 'replicate', 'plateindex', 'quadrant'];
  var LABEL_NAMES = ['label', 'control', 'reaction', 'rxn'];
  var FEATURE_NAMES = ['feature'];
  // With a well column every other column is a feature, one value per well.
  // Without one the table is a list of labels, and the rest are their attributes.
  function columnRoles(table) {
    var keys = table.header.map(function (name) { return name.toLowerCase().replace(/[^a-z0-9]/g, ''); });
    var wellAt = -1, labelAt = -1, featureAt = -1;
    keys.forEach(function (key, j) {
      if (wellAt < 0 && WELL_NAMES.indexOf(key) >= 0) wellAt = j;
      if (labelAt < 0 && LABEL_NAMES.indexOf(key) >= 0) labelAt = j;
      if (featureAt < 0 && FEATURE_NAMES.indexOf(key) >= 0) featureAt = j;
    });
    return keys.map(function (key, j) {
      if (wellAt >= 0) return j === wellAt ? 'well' : LAYOUT_NAMES.indexOf(key) >= 0 ? 'ignore' : 'feature';
      if (j === labelAt) return 'label';
      if (j === featureAt) return 'featurename';
      if (LAYOUT_NAMES.indexOf(key) >= 0) return 'ignore';
      var cells = table.rows.map(function (r) { return r[j]; }).filter(function (c) { return c !== ''; });
      return cells.length && cells.every(isNumber) ? 'number' : 'text';
    });
  }
  function listWells(xs) {
    var u = xs.filter(function (x, i) { return xs.indexOf(x) === i; });
    return u.slice(0, 5).join(', ') + (u.length > 5 ? ', ...' : '');
  }
  // Works out what loading a table would do: a layout has one value per feature
  // for each well, and a list of labels has attributes for each label.
  function planImport(table, roles, info) {
    var wellCol = roles.indexOf('well'), plan = { kind: wellCol >= 0 ? 'layout' : 'labels', labels: [], placed: [], features: [], attrs: [], checks: [], ok: false, badRows: [] };
    var seen = {};
    var labelOf = function (name, feature, attrs) {
      var key = JSON.stringify([feature, name]);
      if (!seen[key]) {
        seen[key] = { name: name, feature: feature, attrs: attrs || {} };
        plan.labels.push(seen[key]);
        if (plan.features.indexOf(feature) < 0) plan.features.push(feature);
        return true;
      }
      return false;
    };
    if (plan.kind === 'layout') {
      var cols = [];
      roles.forEach(function (r, j) { if (r === 'feature') cols.push(j); });
      if (!cols.length) { plan.checks.push({ level: 'Problem', text: 'Choose at least one column to use as a feature.' }); return plan; }
      var changed = 0, blank = 0, offPlate = [], again = [], taken = {};
      table.rows.forEach(function (row, n) {
        var cleaned = cleanWell(row[wellCol], info);
        if (cleaned == null) { offPlate.push(row[wellCol] || '(blank)'); plan.badRows.push(n); return; }
        if (cleaned !== row[wellCol]) changed++;
        var values = {}, any = false;
        cols.forEach(function (j) { if (row[j] !== '') { values[table.header[j]] = row[j]; any = true; } });
        if (!any) { blank++; return; }
        if (taken[cleaned]) { again.push(cleaned); plan.badRows.push(n); return; }
        taken[cleaned] = true;
        Object.keys(values).forEach(function (f) { labelOf(values[f], f); });
        plan.placed.push({ well: cleaned, values: values });
      });
      plan.checks.push({ level: 'OK', text: plan.placed.length + ' wells on this ' + info.plate + '-well plate, with ' + cols.length + (cols.length === 1 ? ' feature: ' : ' features: ') + cols.map(function (j) { return table.header[j]; }).join(', ') + '.' });
      if (changed) plan.checks.push({ level: 'Changed', text: changed + ' well labels were standardised, as clean_wells() does.' });
      if (blank) plan.checks.push({ level: 'Note', text: blank + ' rows have no values and were left out.' });
      if (offPlate.length) plan.checks.push({ level: 'Problem', text: 'Skipped ' + offPlate.length + ' rows that are not wells on this plate: ' + listWells(offPlate) + '.' });
      if (again.length) plan.checks.push({ level: 'Problem', text: 'A well has one row, so later rows were skipped for: ' + listWells(again) + '.' });
      plan.ok = plan.placed.length > 0;
      return plan;
    }
    var labelCol = roles.indexOf('label'), featureCol = roles.indexOf('featurename');
    roles.forEach(function (r, j) { if (r === 'number' || r === 'text') plan.attrs.push({ name: table.header[j], col: j, type: r }); });
    if (labelCol < 0) { plan.checks.push({ level: 'Problem', text: 'Choose the column to use as label, or the column to use as well.' }); return plan; }
    var empty = 0, repeats = [];
    table.rows.forEach(function (row) {
      var name = row[labelCol];
      if (name === '') { empty++; return; }
      var attrs = {};
      plan.attrs.forEach(function (a) { var v = row[a.col]; attrs[a.name] = v === '' ? null : a.type === 'number' ? Number(v) : v; });
      if (!labelOf(name, featureCol >= 0 && row[featureCol] !== '' ? row[featureCol] : 'label', attrs)) repeats.push(name);
    });
    plan.checks.push({ level: 'OK', text: plan.labels.length + ' labels in ' + plan.features.length + (plan.features.length === 1 ? ' feature' : ' features') + '. Nothing is placed on the plate until you paint it.' });
    if (empty) plan.checks.push({ level: 'Note', text: empty + ' rows have no label and were left out.' });
    if (repeats.length) plan.checks.push({ level: 'Problem', text: 'Labels appear more than once, so the first row was kept: ' + listWells(repeats) + '.' });
    plan.ok = plan.labels.length > 0;
    return plan;
  }

  // ---- writing tables -----------------------------------------------------
  function csvCell(v) {
    var s = v == null ? '' : String(v);
    return /[",\n\r]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
  }
  // One row per painted well, one column per feature, then the attributes of the
  // well's labels. model: wells, assign, labels, features, attrNames.
  function valueRows(model) {
    var names = Object.keys(model.features), rows = [];
    model.assign.forEach(function (a, i) {
      if (!a.length) return;
      var row = { well: model.wells[i] };
      names.forEach(function (f) { row[f] = null; });
      model.attrNames.forEach(function (name) { row[name] = null; });
      a.forEach(function (k) {
        var l = model.labels[k];
        row[l.feature] = l.name;
        model.attrNames.forEach(function (name) { if (row[name] == null && l.attrs[name] != null) row[name] = l.attrs[name]; });
      });
      rows.push(row);
    });
    return { rows: rows, columns: ['well'].concat(names, model.attrNames) };
  }
  function csvText(model) {
    var v = valueRows(model);
    return [v.columns.map(csvCell).join(',')].concat(v.rows.map(function (r) {
      return v.columns.map(function (c) { return csvCell(r[c]); }).join(',');
    })).join('\n') + '\n';
  }

  // ---- image export -------------------------------------------------------
  function xml(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  }
  function shortText(s) { s = String(s); return s.length > 5 ? s.slice(0, 4) + '…' : s; }
  function round(v) { return Math.round(v * 100) / 100; }
  // A plate as one standalone SVG, with its styles written on each element, so
  // the same text works as a file, a preview and the source of a PNG.
  // opts: title, legend, text, headers, width, background, marks (well indices).
  function plateSVG(model, opts) {
    var info = model.info, scale = fillScale(model), parts = [], marks = {};
    (opts.marks || []).forEach(function (i) { marks[i] = true; });
    var rowW = opts.headers ? 12 + 9 * info.rows[info.rows.length - 1].length : 0;
    var top = (opts.title ? 34 : 0) + (opts.headers ? HEAD : 0), pad = 8;
    var plateW = info.colmax * CELL, plateH = info.rowmax * CELL;
    var font = "font-family=\"system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif\"";
    if (opts.title) parts.push('<text x="' + pad + '" y="' + (pad + 16) + '" font-size="18" font-weight="600" fill="' + INK + '">' + xml(opts.title) + '</text>');
    var x0 = pad + rowW, y0 = pad + top;
    if (opts.headers) {
      var every = info.plate >= 1536 ? 4 : 1;
      for (var c = 0; c < info.colmax; c++) {
        if ((c + 1) % every && c) continue;
        parts.push('<text x="' + (x0 + c * CELL + CELL / 2) + '" y="' + (y0 - 7) + '" font-size="12" text-anchor="middle" fill="#4F5662">' + (c + 1) + '</text>');
      }
      for (var r = 0; r < info.rowmax; r++) {
        if (r % every) continue;
        parts.push('<text x="' + (x0 - 6) + '" y="' + (y0 + r * CELL + CELL / 2 + 4) + '" font-size="12" text-anchor="end" fill="#4F5662">' + info.rows[r] + '</text>');
      }
    }
    var used = {};
    model.assign.forEach(function (a, i) {
      var cx = x0 + (i % info.colmax) * CELL + 1.5, cy = y0 + Math.floor(i / info.colmax) * CELL + 1.5, sz = CELL - 3;
      if (!a.length) {
        parts.push('<rect x="' + cx + '" y="' + cy + '" width="' + sz + '" height="' + sz + '" rx="5" fill="' + EMPTY_FILL + '" stroke="' + EMPTY_EDGE + '"/>');
        return;
      }
      var k = wellPick(a, scale);
      var fill = k < 0 ? FILL_NA : scale.colour[k];
      if (k >= 0) used[k] = true;
      parts.push('<rect x="' + cx + '" y="' + cy + '" width="' + sz + '" height="' + sz + '" rx="5" fill="' + fill + '" stroke="rgba(0,0,0,.35)"/>');
      if (opts.text && info.plate <= 384 && k >= 0 && scale.text[k]) {
        parts.push('<text x="' + (cx + sz / 2) + '" y="' + (cy + sz / 2 + 3.5) + '" font-size="10" font-weight="600" text-anchor="middle" fill="' + inkFor(fill) + '">' + xml(shortText(scale.text[k])) + '</text>');
      }
      if (marks[i]) parts.push('<text x="' + (cx + sz / 2) + '" y="' + (cy + sz / 2 + 7) + '" font-size="20" font-weight="700" text-anchor="middle" fill="' + inkFor(fill) + '">\u00D7</text>');
    });
    var width = x0 + plateW + pad, height = y0 + plateH + pad;
    if (opts.legend) {
      var lx = x0 + plateW + 24, ly = y0 + 12;
      parts.push('<text x="' + lx + '" y="' + ly + '" font-size="13" font-weight="600" fill="' + INK + '">' + xml(model.fill) + '</text>');
      ly += 10;
      if (scale.type === 'continuous') {
        var gid = 'mtpr-ramp-' + model.fill.replace(/[^\w]/g, '');
        parts.push('<defs><linearGradient id="' + gid + '" x1="0" x2="0" y1="1" y2="0"><stop offset="0" stop-color="' + FILL_LOW + '"/><stop offset="1" stop-color="' + FILL_HIGH + '"/></linearGradient></defs>');
        parts.push('<rect x="' + lx + '" y="' + ly + '" width="18" height="120" fill="url(#' + gid + ')" stroke="rgba(0,0,0,.35)"/>');
        parts.push('<text x="' + (lx + 24) + '" y="' + (ly + 10) + '" font-size="12" fill="' + INK + '">' + scale.high + '</text>');
        parts.push('<text x="' + (lx + 24) + '" y="' + (ly + 120) + '" font-size="12" fill="' + INK + '">' + scale.low + (scale.log ? ' (log)' : '') + '</text>');
        width = Math.max(width, lx + 90);
        height = Math.max(height, ly + 136);
      } else {
        var keys = scale.keys.filter(function (key) { return key.k == null || used[key.k]; });
        var longest = keys.reduce(function (m, key) { return Math.max(m, key.name.length); }, 4);
        var colW = 26 + longest * 7, perCol = Math.max(4, Math.floor((plateH - 10) / 19));
        keys.forEach(function (key, n) {
          var kx = lx + Math.floor(n / perCol) * colW, ky = ly + (n % perCol) * 19;
          parts.push('<rect x="' + kx + '" y="' + ky + '" width="13" height="13" rx="3" fill="' + key.colour + '" stroke="rgba(0,0,0,.55)"/>');
          parts.push('<text x="' + (kx + 19) + '" y="' + (ky + 11) + '" font-size="12" fill="' + INK + '">' + xml(key.name) + '</text>');
        });
        width = Math.max(width, lx + Math.ceil(Math.max(keys.length, 1) / perCol) * colW);
      }
    }
    var pxW = opts.width || width, pxH = Math.round(pxW * height / width);
    var back = opts.background === 'transparent' ? '' : '<rect width="' + width + '" height="' + height + '" fill="#FFFFFF"/>';
    return {
      width: pxW, height: pxH,
      svg: '<svg xmlns="http://www.w3.org/2000/svg" width="' + pxW + '" height="' + pxH + '" viewBox="0 0 ' + width + ' ' + height + '" ' + font + '>' + back + parts.join('') + '</svg>'
    };
  }

  return {
    SIZES: SIZES, PALETTE: PALETTE, FILL_NA: FILL_NA,
    rowLabels: rowLabels, plateInfo: plateInfo, wellNames: wellNames,
    parseWell: parseWell, wellIndex: wellIndex, cleanWell: cleanWell,
    positionOf: positionOf, indexOfPosition: indexOfPosition,
    sequenceLayout: sequenceLayout, sequenceCall: sequenceCall,
    canQuadrant: canQuadrant, movePlate: movePlate,
    mix: mix, inkFor: inkFor, paletteColour: paletteColour, defaultColours: defaultColours, nextColour: nextColour,
    fillScale: fillScale, wellPick: wellPick,
    parseTable: parseTable, columnRoles: columnRoles, planImport: planImport,
    valueRows: valueRows, csvText: csvText, plateSVG: plateSVG
  };
});
