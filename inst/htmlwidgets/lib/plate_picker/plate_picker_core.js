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
  // A group takes one palette colour, and its labels run dark to light through
  // it. picker_default_colours() in R.
  function defaultColours(groups) {
    var units = [], where = {}, out = [];
    groups.forEach(function (g, k) {
      var key = g == null || g === '' ? 'label:' + k : 'group:' + g;
      if (where[key] == null) { where[key] = units.length; units.push(0); }
      units[where[key]]++;
    });
    var seen = units.map(function () { return 0; });
    groups.forEach(function (g, k) {
      var u = where[g == null || g === '' ? 'label:' + k : 'group:' + g];
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

  // What each label is drawn in, for plot_plate()-style `fill` and `scale`.
  // model: labels (records with name, group, colour, attrs), attrTypes, fill, log.
  function fillScale(model) {
    var labels = model.labels, fill = model.fill || 'label', out = { fill: fill, type: 'discrete', keys: [] };
    if (fill === 'label') {
      out.colour = labels.map(function (l) { return l.colour; });
      out.text = labels.map(function (l) { return l.name; });
      out.keys = labels.map(function (l, k) { return { name: l.name, colour: l.colour, k: k }; });
      return out;
    }
    var value = function (l) {
      var v = fill === 'group' ? l.group : l.attrs[fill];
      return v == null || v === '' ? null : v;
    };
    var values = labels.map(value);
    if (fill !== 'group' && model.attrTypes[fill] === 'number') {
      var nums = values.filter(function (v) { return v != null; }).map(Number);
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
    var keys = [];
    values.forEach(function (v) { if (v != null && keys.indexOf(String(v)) < 0) keys.push(String(v)); });
    out.keys = keys.map(function (name, i) { return { name: name, colour: paletteColour(i) }; });
    out.colour = values.map(function (v) { return v == null ? null : paletteColour(keys.indexOf(String(v))); });
    out.text = labels.map(function (l) { return l.name; });
    return out;
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
  function isNumber(s) { return s !== '' && isFinite(Number(s)); }
  // Column names used across mtpR: plate_picker exports, place_controls() and
  // plate_roles() (Well, Control, Role), design_plate() (Well.Position) and
  // replicate_reactions() (reaction, replicate, position).
  var ROLE_NAMES = {
    well: ['well', 'wellposition', 'wellid'],
    label: ['label', 'control', 'reaction', 'rxn'],
    group: ['group', 'role'],
    ignore: ['row', 'column', 'wellindex', 'position', 'replicate', 'plateindex', 'quadrant']
  };
  function columnRoles(table) {
    var taken = {};
    return table.header.map(function (name, j) {
      var key = name.toLowerCase().replace(/[^a-z0-9]/g, '');
      for (var role in ROLE_NAMES) {
        if (ROLE_NAMES[role].indexOf(key) >= 0) {
          if (role === 'ignore') return role;
          if (!taken[role]) { taken[role] = true; return role; }
        }
      }
      var cells = table.rows.map(function (r) { return r[j]; }).filter(function (c) { return c !== ''; });
      return cells.length && cells.every(isNumber) ? 'number' : 'text';
    });
  }
  function listWells(xs) {
    var u = xs.filter(function (x, i) { return xs.indexOf(x) === i; });
    return u.slice(0, 5).join(', ') + (u.length > 5 ? ', ...' : '');
  }
  // Works out what loading a table would do. Attributes belong to a label, the
  // way replicate_reactions() joins `rxn_df` to each reaction's wells.
  function planImport(table, roles, info, multiple) {
    var find = function (role) { return roles.indexOf(role); };
    var wellCol = find('well'), labelCol = find('label'), groupCol = find('group');
    var attrs = [];
    roles.forEach(function (r, j) { if (r === 'number' || r === 'text') attrs.push({ name: table.header[j], col: j, type: r }); });
    var plan = { kind: wellCol >= 0 ? 'layout' : 'labels', labels: [], placed: [], attrs: attrs, checks: [], ok: false, badRows: [] };
    if (labelCol < 0) {
      plan.checks.push({ level: 'Problem', text: 'Choose the column to use as Label.' });
      return plan;
    }
    var byName = {}, changed = 0, blank = 0, offPlate = [], taken = {}, clashes = [], again = [];
    table.rows.forEach(function (row, n) {
      var name = row[labelCol];
      if (name === '') { blank++; return; }
      var cleaned = null;
      if (wellCol >= 0) {
        cleaned = cleanWell(row[wellCol], info);
        if (cleaned == null) { offPlate.push(row[wellCol] || '(blank)'); plan.badRows.push(n); return; }
        if (cleaned !== row[wellCol]) changed++;
      }
      var rec = byName[name];
      if (!rec) {
        rec = byName[name] = { name: name, group: groupCol >= 0 && row[groupCol] !== '' ? row[groupCol] : null, attrs: {} };
        attrs.forEach(function (a) { var v = row[a.col]; rec.attrs[a.name] = v === '' ? null : a.type === 'number' ? Number(v) : v; });
        plan.labels.push(rec);
      } else {
        attrs.forEach(function (a) {
          var v = row[a.col], old = rec.attrs[a.name];
          if (v !== '' && old != null && String(a.type === 'number' ? Number(v) : v) !== String(old) && clashes.indexOf(name) < 0) clashes.push(name);
        });
      }
      if (cleaned != null) {
        if (taken[cleaned] && (!multiple || taken[cleaned].indexOf(name) >= 0)) { again.push(cleaned); plan.badRows.push(n); return; }
        taken[cleaned] = (taken[cleaned] || []).concat(name);
        plan.placed.push({ well: cleaned, label: name });
      }
    });
    if (wellCol >= 0) {
      plan.checks.push({ level: 'OK', text: plan.placed.length + ' rows place a label on this ' + info.plate + '-well plate.' });
      if (changed) plan.checks.push({ level: 'Changed', text: changed + ' well labels were standardised, as clean_wells() does.' });
      if (offPlate.length) plan.checks.push({ level: 'Problem', text: 'Skipped ' + offPlate.length + ' rows that are not wells on this plate: ' + listWells(offPlate) + '.' });
      if (again.length) plan.checks.push({ level: 'Problem', text: 'Skipped rows for wells already used: ' + listWells(again) + '.' });
    } else {
      plan.checks.push({ level: 'OK', text: plan.labels.length + ' labels. Nothing is placed on the plate until you paint it.' });
    }
    if (blank) plan.checks.push({ level: 'Note', text: blank + ' rows have no label and were left out.' });
    if (clashes.length) plan.checks.push({ level: 'Problem', text: 'Attributes belong to a label, but these differ between rows, so the first row was kept: ' + listWells(clashes) + '.' });
    plan.ok = plan.labels.length > 0;
    return plan;
  }

  // ---- writing tables -----------------------------------------------------
  function csvCell(v) {
    var s = v == null ? '' : String(v);
    return /[",\n\r]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
  }
  // One row per well and label. model: wells, assign, labels, attrNames.
  function valueRows(model) {
    var rows = [], grouped = model.labels.some(function (l) { return l.group != null; });
    model.assign.forEach(function (a, i) {
      a.forEach(function (k) {
        var l = model.labels[k], row = { well: model.wells[i], label: l.name };
        if (grouped) row.group = l.group;
        model.attrNames.forEach(function (name) { row[name] = l.attrs[name] == null ? null : l.attrs[name]; });
        rows.push(row);
      });
    });
    return { rows: rows, columns: ['well', 'label'].concat(grouped ? ['group'] : [], model.attrNames) };
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
      var band = sz / a.length;
      a.forEach(function (k, n) {
        used[k] = true;
        var fill = scale.colour[k] || FILL_NA;
        parts.push('<rect x="' + cx + '" y="' + round(cy + n * band) + '" width="' + sz + '" height="' + round(band) + '" rx="' + (a.length > 1 ? 1 : 5) + '" fill="' + fill + '" stroke="rgba(0,0,0,.35)"/>');
      });
      if (opts.text && info.plate <= 384 && a.length === 1) {
        var t = scale.text[a[0]];
        if (t) parts.push('<text x="' + (cx + sz / 2) + '" y="' + (cy + sz / 2 + 3.5) + '" font-size="10" font-weight="600" text-anchor="middle" fill="' + inkFor(scale.colour[a[0]] || FILL_NA) + '">' + xml(shortText(t)) + '</text>');
      }
      if (marks[i]) parts.push('<text x="' + (cx + sz / 2) + '" y="' + (cy + sz / 2 + 7) + '" font-size="20" font-weight="700" text-anchor="middle" fill="' + inkFor(scale.colour[a[0]] || FILL_NA) + '">×</text>');
    });
    var width = x0 + plateW + pad, height = y0 + plateH + pad;
    if (opts.legend) {
      var lx = x0 + plateW + 24, ly = y0 + 12, title = model.fill === 'label' || !model.fill ? 'label' : model.fill;
      parts.push('<text x="' + lx + '" y="' + ly + '" font-size="13" font-weight="600" fill="' + INK + '">' + xml(title) + '</text>');
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
    fillScale: fillScale,
    parseTable: parseTable, columnRoles: columnRoles, planImport: planImport,
    valueRows: valueRows, csvText: csvText, plateSVG: plateSVG
  };
});
