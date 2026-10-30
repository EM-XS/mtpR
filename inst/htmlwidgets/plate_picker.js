/* mtpR plate_picker: paint wells of a microtiter plate with labels.
 * Plain SVG, pointer events, no dependencies beyond htmlwidgets. The rules shared
 * with the R functions are in lib/plate_picker/plate_picker_core.js. */
(function () {
  'use strict';

  var core = window.mtprPlateCore;
  var SVGNS = 'http://www.w3.org/2000/svg';
  var CELL = 30;          // cell size in viewBox units
  var HDR_H = 22;         // column header band
  var PAD = 3;            // room for focus ring and borders
  var MAX_CELL_PX = 46;   // never draw wells bigger than this
  var HISTORY = 100;
  var STATE_ID = 'mtpr-picker-state';

  var registry = {};      // element id -> picker
  var pending = {};       // element id -> queued Shiny messages
  var uid = 0;

  function arr(v) { return v == null ? [] : Array.isArray(v) ? v : [v]; }
  function sv(name, attrs, parent) {
    var e = document.createElementNS(SVGNS, name);
    for (var k in attrs) if (Object.prototype.hasOwnProperty.call(attrs, k)) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e);
    return e;
  }
  function h(name, attrs, parent, text) {
    var e = document.createElement(name);
    for (var k in attrs) if (Object.prototype.hasOwnProperty.call(attrs, k)) e.setAttribute(k, attrs[k]);
    if (text != null) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function inShiny() { return !!(window.Shiny && window.Shiny.setInputValue); }
  function clone(v) { return JSON.parse(JSON.stringify(v)); }
  function slug(s) { return String(s).trim().replace(/[^\w.-]+/g, '-').replace(/^-+|-+$/g, '').toLowerCase(); }

  function createPicker(el, x) {
    var id = 'mtprp' + (++uid);
    var multiple = !!x.multiple;
    var standalone = !inShiny();
    var showExport = x.exportButtons === 'auto' || x.exportButtons == null ? standalone : !!x.exportButtons;
    var editable = x.editable !== false && x.toolbar !== false;

    // ---- state -----------------------------------------------------------
    var plate, info, nrow, ncol, N, wells, index;
    function setPlateSize(p) {
      plate = +p; info = core.plateInfo(plate);
      nrow = info.rowmax; ncol = info.colmax; N = nrow * ncol;
      wells = core.wellNames(info);
      index = {}; wells.forEach(function (w, i) { index[w] = i; });
    }
    setPlateSize(x.plate);

    var labels = [], attrNames = [], attrTypes = {}, groupSettings = {};
    var fill = 'label', logScale = false, wellText = true;
    var assign = [], locked = [], bgFill = [], bgText = [], bgTip = [], bgData = null;
    var active = 0, activeGroup = null, eraser = false;
    var undoStack = [], redoStack = [];
    var drag = null;
    var focusIdx = 0;
    var title = x.title || null;
    var lastCall = '';

    function newGroup() { return { sequence: false, pattern: 'grouped', num_replicates: 1, inter_spacing: 1, intra_spacing: 1, fill_wise: 'row' }; }
    function syncGroups() { labels.forEach(function (l) { if (l.group != null && !groupSettings[l.group]) groupSettings[l.group] = newGroup(); }); }
    // `d` has parallel `labels`, `colours` and `groups`, and `attributes`: one
    // array per attribute, in label order.
    function setLabels(d) {
      var names = arr(d.labels), cols = arr(d.colours), groups = arr(d.groups), at = d.attributes || {};
      attrNames = Object.keys(at);
      attrTypes = d.attributeTypes ? clone(d.attributeTypes) : {};
      attrNames.forEach(function (a) {
        if (!attrTypes[a]) attrTypes[a] = arr(at[a]).some(function (v) { return v != null && typeof v !== 'number'; }) ? 'text' : 'number';
      });
      labels = names.map(function (name, k) {
        var attrs = {};
        attrNames.forEach(function (a) { var v = arr(at[a])[k]; attrs[a] = v == null ? null : v; });
        return { name: String(name), colour: cols[k] || core.nextColour(cols), group: groups[k] == null || groups[k] === '' ? null : String(groups[k]), attrs: attrs };
      });
      syncGroups();
    }
    function labelIndex(name) {
      for (var k = 0; k < labels.length; k++) if (labels[k].name === name) return k;
      return -1;
    }
    function emptyAssign() { var a = []; for (var i = 0; i < N; i++) a.push([]); return a; }
    function setValue(v) {
      assign = emptyAssign();
      var ws = arr(v && v.well), ls = arr(v && v.label);
      for (var j = 0; j < ws.length; j++) {
        var wi = index[ws[j]], k = labelIndex(ls[j]);
        if (wi != null && k >= 0 && assign[wi].indexOf(k) < 0) assign[wi].push(k);
      }
      assign.forEach(function (a) { a.sort(function (p, q) { return p - q; }); if (!multiple && a.length > 1) a.length = 1; });
    }
    function setLocked(v) {
      locked = [];
      for (var i = 0; i < N; i++) locked.push(false);
      arr(v).forEach(function (w) { if (index[w] != null) locked[index[w]] = true; });
    }
    function lockedWells() { return wells.filter(function (w, i) { return locked[i]; }); }
    function setBackground(b) {
      bgData = b; bgFill = []; bgText = []; bgTip = [];
      for (var i = 0; i < N; i++) { bgFill.push(null); bgText.push(null); bgTip.push(null); }
      if (!b) return;
      var ws = arr(b.well), f = arr(b.fill), t = arr(b.text), tp = arr(b.tooltip);
      for (var j = 0; j < ws.length; j++) {
        var wi = index[ws[j]]; if (wi == null) continue;
        bgFill[wi] = f[j] || null;
        bgText[wi] = t[j] != null && t[j] !== '' ? String(t[j]) : null;
        bgTip[wi] = tp[j] != null && tp[j] !== '' ? String(tp[j]) : null;
      }
    }

    // A page saved by the widget keeps its painting, labels and plate in a JSON
    // block. Version 1 blocks hold only the painting.
    setLabels(x);
    fill = x.fill || 'label';
    var initial = x.value, initialLocked = x.locked;
    var stateEl = document.getElementById(STATE_ID);
    if (standalone && stateEl) {
      try {
        var st = JSON.parse(stateEl.textContent);
        if (st && st.version >= 2) {
          setPlateSize(st.plate); setLabels(st);
          groupSettings = st.groupSettings || {}; syncGroups();
          fill = st.fill || 'label'; logScale = !!st.log;
          initial = st.assignments; initialLocked = st.locked;
        } else if (st && st.plate === x.plate && st.assignments) initial = st.assignments;
        if (st && st.title && !title) title = st.title;
      } catch (e) { /* keep the widget data */ }
    }
    if (fill !== 'label' && fill !== 'group' && attrNames.indexOf(fill) < 0) fill = 'label';
    active = Math.max(0, labelIndex(x.active));
    setValue(initial); setLocked(initialLocked); setBackground(x.background);

    // Undo covers the painting, the labels and the plate.
    function snapshot() {
      return { plate: plate, assign: clone(assign), locked: locked.slice(), labels: clone(labels), attrNames: attrNames.slice(), attrTypes: clone(attrTypes), groupSettings: clone(groupSettings) };
    }
    function sameSnap(a, b) { return JSON.stringify(a) === JSON.stringify(b); }
    function restore(s) {
      var replot = s.plate !== plate;
      if (replot) setPlateSize(s.plate);
      assign = clone(s.assign); locked = s.locked.slice(); labels = clone(s.labels);
      attrNames = s.attrNames.slice(); attrTypes = clone(s.attrTypes); groupSettings = clone(s.groupSettings);
      active = Math.min(active, labels.length - 1);
      if (activeGroup != null && !groupSettings[activeGroup]) activeGroup = null;
      if (attrNames.indexOf(fill) < 0 && fill !== 'label' && fill !== 'group') fill = 'label';
      if (replot) { setBackground(bgData); buildGrid(); }
      redraw();
    }

    // ---- editing primitives ---------------------------------------------
    function has(i, k) { return assign[i].indexOf(k) >= 0; }
    function add(i, k) {
      if (multiple) { if (!has(i, k)) { assign[i].push(k); assign[i].sort(function (p, q) { return p - q; }); return true; } return false; }
      if (assign[i].length === 1 && assign[i][0] === k) return false;
      assign[i] = [k]; return true;
    }
    function remove(i, k) { var n = assign[i].length; assign[i] = assign[i].filter(function (v) { return v !== k; }); return assign[i].length !== n; }
    function clearWell(i) { if (!assign[i].length) return false; assign[i] = []; return true; }
    function applyMode(i, mode) {
      if (i < 0 || locked[i]) return false;
      var ch = mode === 'add' ? add(i, active) : mode === 'remove' ? remove(i, active) : clearWell(i);
      if (ch) renderCell(i);
      return ch;
    }
    function rcOf(i) { return [Math.floor(i / ncol), i % ncol]; }
    function rectIdx(a, b) {
      var p = rcOf(a), q = rcOf(b), out = [];
      for (var r = Math.min(p[0], q[0]); r <= Math.max(p[0], q[0]); r++)
        for (var c = Math.min(p[1], q[1]); c <= Math.max(p[1], q[1]); c++) out.push(r * ncol + c);
      return out;
    }
    function lineIdx(a, b) {   // cells between two cells, so quick strokes leave no gaps
      var p = rcOf(a), q = rcOf(b), out = [];
      var dr = Math.abs(q[0] - p[0]), dc = Math.abs(q[1] - p[1]);
      var sr = p[0] < q[0] ? 1 : -1, sc = p[1] < q[1] ? 1 : -1, err = dc - dr, r = p[0], c = p[1];
      for (;;) {
        out.push(r * ncol + c);
        if (r === q[0] && c === q[1]) break;
        var e2 = 2 * err;
        if (e2 > -dr) { err -= dr; c += sc; }
        if (e2 < dc) { err += dc; r += sr; }
      }
      return out;
    }
    function modeFor(i) { return eraser ? 'erase' : has(i, active) ? 'remove' : 'add'; }

    // ---- DOM -------------------------------------------------------------
    el.innerHTML = '';
    el.classList.add('mtpr-picker');
    el.setAttribute('data-theme', x.theme || 'auto');
    el.setAttribute('lang', el.getAttribute('lang') || 'en');

    var toolbar = null, side = null, panel = null, plateSelect = null, fillSelect = null, logBtn = null, textBtn = null;
    var eraserBtn, undoBtn, redoBtn, clearBtn, fileInput = null;
    var countEls = {}, rowBtns = {};
    if (x.toolbar !== false) {
      toolbar = h('div', { 'class': 'mtpr-toolbar', role: 'toolbar', 'aria-label': 'Plate picker tools' }, el);
      if (editable) {
        var plateLabel = h('label', { 'class': 'mtpr-field-inline' }, toolbar, 'plate ');
        plateSelect = h('select', { 'class': 'mtpr-select', 'aria-label': 'Plate size' }, plateLabel);
        Object.keys(core.SIZES).forEach(function (p) { h('option', { value: p }, plateSelect, p); });
        plateSelect.addEventListener('change', function () { askPlate(+plateSelect.value); });
        h('span', { 'class': 'mtpr-sep' }, toolbar);
      }
      eraserBtn = h('button', { type: 'button', 'class': 'mtpr-btn', 'aria-pressed': 'false', title: 'Eraser (E)' }, toolbar, 'Eraser');
      eraserBtn.addEventListener('click', function () { eraser = !eraser; updateToolbar(); });
      undoBtn = h('button', { type: 'button', 'class': 'mtpr-btn', title: 'Undo (Ctrl/Cmd+Z)' }, toolbar, 'Undo');
      redoBtn = h('button', { type: 'button', 'class': 'mtpr-btn', title: 'Redo (Shift+Ctrl/Cmd+Z)' }, toolbar, 'Redo');
      clearBtn = h('button', { type: 'button', 'class': 'mtpr-btn' }, toolbar, 'Clear');
      undoBtn.addEventListener('click', undo);
      redoBtn.addEventListener('click', redo);
      clearBtn.addEventListener('click', function () {
        var before = snapshot(), any = false;
        for (var i = 0; i < N; i++) if (!locked[i] && assign[i].length) { assign[i] = []; any = true; }
        if (any) { renderAll(); commit(before); }
      });
      h('span', { 'class': 'mtpr-sep' }, toolbar);
      var fillLabel = h('label', { 'class': 'mtpr-field-inline' }, toolbar, 'fill ');
      fillSelect = h('select', { 'class': 'mtpr-select', 'aria-label': 'Fill wells by' }, fillLabel);
      fillSelect.addEventListener('change', function () { fill = fillSelect.value; redraw(); });
      logBtn = h('button', { type: 'button', 'class': 'mtpr-btn', 'aria-pressed': 'false' }, toolbar, 'Log scale');
      logBtn.addEventListener('click', function () { logScale = !logScale; redraw(); });
      textBtn = h('button', { type: 'button', 'class': 'mtpr-btn', 'aria-pressed': 'true' }, toolbar, 'Well text');
      textBtn.addEventListener('click', function () { wellText = !wellText; redraw(); });
      h('span', { 'class': 'mtpr-grow' }, toolbar);
      if (editable) {
        fileInput = h('input', { type: 'file', accept: '.csv,.tsv,.txt,text/csv', 'class': 'mtpr-hide', 'aria-label': 'CSV file' }, toolbar);
        fileInput.addEventListener('change', readFile);
        h('button', { type: 'button', 'class': 'mtpr-btn' }, toolbar, 'Load CSV').addEventListener('click', function () { fileInput.value = ''; fileInput.click(); });
      }
      if (showExport) {
        h('button', { type: 'button', 'class': 'mtpr-btn' }, toolbar, 'Export').addEventListener('click', openExport);
        h('button', { type: 'button', 'class': 'mtpr-btn' }, toolbar, 'Save page').addEventListener('click', savePage);
      }
      panel = h('div', { 'class': 'mtpr-panel mtpr-hide' }, el);
    }

    var body = h('div', { 'class': 'mtpr-body' }, el);
    if (x.toolbar !== false) side = h('div', { 'class': 'mtpr-side' }, body);
    var wrap = h('div', { 'class': 'mtpr-wrap', style: 'position:relative' }, body);
    var svg = sv('svg', { 'class': 'mtpr-grid', role: 'grid', tabindex: '0', 'aria-multiselectable': 'true' }, wrap);
    var tip = h('div', { 'class': 'mtpr-tip', role: 'tooltip' }, wrap);
    var legendEl = h('div', { 'class': 'mtpr-legend' }, wrap);
    var status = h('div', { 'class': 'mtpr-status', 'aria-live': 'polite' }, el);

    var rowW, HDR_W, viewW, viewH, colHdrs, rowHdrs, cellEls, chipLayers, bases, previewEl, focusEl;
    function buildGrid() {
      while (svg.firstChild) svg.removeChild(svg.firstChild);
      rowW = Math.max.apply(null, info.rows.map(function (s) { return s.length; }));
      HDR_W = 12 + rowW * 9;
      viewW = HDR_W + ncol * CELL + 2 * PAD; viewH = HDR_H + nrow * CELL + 2 * PAD;
      svg.setAttribute('viewBox', '0 0 ' + viewW + ' ' + viewH);
      svg.setAttribute('aria-label', 'Plate map, ' + plate + ' wells. Arrow keys move, Space paints.');
      svg.setAttribute('aria-rowcount', nrow + 1); svg.setAttribute('aria-colcount', ncol);
      svg.style.maxWidth = Math.round(viewW * MAX_CELL_PX / CELL) + 'px';
      svg.style.aspectRatio = viewW + ' / ' + viewH;
      var defs = sv('defs', {}, svg);
      var pat = sv('pattern', { id: 'mtpr-hatch', width: 8, height: 8, patternUnits: 'userSpaceOnUse', patternTransform: 'rotate(45)' }, defs);
      sv('line', { x1: 0, y1: 0, x2: 0, y2: 8, 'class': 'mtpr-hatch-line' }, pat);
      var clip = sv('clipPath', { id: id + '-clip', clipPathUnits: 'objectBoundingBox' }, defs);
      sv('rect', { width: 1, height: 1, rx: 0.22, ry: 0.22 }, clip);
      colHdrs = []; rowHdrs = []; cellEls = []; chipLayers = []; bases = [];
      var hrow = sv('g', { role: 'row', 'aria-rowindex': 1 }, svg);
      for (var cc = 0; cc < ncol; cc++) {
        var cg = sv('g', { role: 'columnheader', 'aria-colindex': cc + 1, 'aria-label': 'Column ' + (cc + 1) }, hrow);
        sv('rect', { 'class': 'mtpr-hdhit', 'data-col': cc, x: PAD + HDR_W + cc * CELL, y: PAD, width: CELL, height: HDR_H }, cg);
        var ct = sv('text', { 'class': 'mtpr-hd', x: PAD + HDR_W + cc * CELL + CELL / 2, y: PAD + HDR_H / 2 + 1 }, cg);
        ct.textContent = String(cc + 1); colHdrs.push(ct);
      }
      for (var rr = 0; rr < nrow; rr++) {
        var rg = sv('g', { role: 'row', 'aria-rowindex': rr + 2 }, svg);
        var rh = sv('g', { role: 'rowheader', 'aria-label': 'Row ' + info.rows[rr] }, rg);
        sv('rect', { 'class': 'mtpr-hdhit', 'data-row': rr, x: PAD, y: PAD + HDR_H + rr * CELL, width: HDR_W, height: CELL }, rh);
        var rt = sv('text', { 'class': 'mtpr-hd mtpr-rhd', x: PAD + HDR_W - 6, y: PAD + HDR_H + rr * CELL + CELL / 2 + 1 }, rh);
        rt.textContent = info.rows[rr]; rowHdrs.push(rt);
        for (var c2 = 0; c2 < ncol; c2++) {
          var ii = rr * ncol + c2;
          var g = sv('g', { 'class': 'mtpr-cell', role: 'gridcell', id: id + '-c' + ii, 'data-i': ii, 'aria-colindex': c2 + 1, 'aria-selected': 'false' }, rg);
          var x0 = PAD + HDR_W + c2 * CELL, y0 = PAD + HDR_H + rr * CELL;
          bases.push(sv('rect', { 'class': 'mtpr-base', x: x0, y: y0, width: CELL, height: CELL }, g));
          chipLayers.push(sv('g', { 'class': 'mtpr-chips' }, g));
          cellEls.push(g);
        }
      }
      previewEl = sv('rect', { 'class': 'mtpr-preview', display: 'none' }, svg);
      focusEl = sv('rect', { 'class': 'mtpr-focus', width: CELL, height: CELL }, svg);
      setFocus(Math.min(focusIdx, N - 1));
      if (plateSelect) plateSelect.value = String(plate);
    }

    // ---- rendering -------------------------------------------------------
    var scale = 1, bgTextOn = true, fillNow = null;
    function model() {
      return { info: info, plate: plate, wells: wells, assign: assign, labels: labels, attrNames: attrNames, attrTypes: attrTypes, fill: fill, log: logScale };
    }
    function cellPos(i) { var p = rcOf(i); return [PAD + HDR_W + p[1] * CELL, PAD + HDR_H + p[0] * CELL]; }
    function labelNames(i) { return assign[i].map(function (k) { return labels[k].name; }).join(' + '); }
    function describe(i) {
      var s = wells[i];
      if (assign[i].length) s += ', ' + labelNames(i);
      if (bgTip[i]) s += ', ' + bgTip[i];
      if (locked[i]) s += ', locked';
      return s;
    }
    function shortText(s) { s = String(s); return s.length > 5 ? s.slice(0, 4) + '\u2026' : s; }
    function renderCell(i) {
      var g = cellEls[i], layer = chipLayers[i], p = cellPos(i);
      while (layer.firstChild) layer.removeChild(layer.firstChild);
      if (bgFill[i]) bases[i].style.fill = bgFill[i]; else bases[i].style.fill = '';
      g.classList.toggle('mtpr-locked', locked[i]);
      g.setAttribute('aria-selected', assign[i].length ? 'true' : 'false');
      if (locked[i]) g.setAttribute('aria-disabled', 'true'); else g.removeAttribute('aria-disabled');
      g.setAttribute('aria-label', describe(i));
      var ins = CELL * (fill === 'label' ? 0.2 : 0.08), sz = CELL - 2 * ins, cx = p[0] + ins, cy = p[1] + ins, a = assign[i];
      var colour = function (k) { return fillNow.colour[k] || core.FILL_NA; };
      if (locked[i]) sv('rect', { 'class': 'mtpr-lockfill', x: p[0], y: p[1], width: CELL, height: CELL }, layer);
      if (a.length) {
        sv('rect', { 'class': 'mtpr-halo', x: cx - 1.5, y: cy - 1.5, width: sz + 3, height: sz + 3, rx: 7, ry: 7 }, layer);
        if (a.length === 1) {
          sv('rect', { 'class': 'mtpr-chip', x: cx, y: cy, width: sz, height: sz, rx: 6, ry: 6, fill: colour(a[0]) }, layer);
        } else {
          var cg2 = sv('g', { 'clip-path': 'url(#' + id + '-clip)' }, layer);
          var bh = sz / a.length;
          a.forEach(function (k, n) { sv('rect', { x: cx, y: cy + n * bh, width: sz, height: bh + 0.3, fill: colour(k) }, cg2); });
          sv('rect', { 'class': 'mtpr-chip', x: cx, y: cy, width: sz, height: sz, rx: 6, ry: 6, fill: 'none' }, layer);
        }
      }
      var text = bgText[i] || (wellText && a.length === 1 ? shortText(fillNow.text[a[0]]) : null);
      if (text) {
        var t = sv('text', { 'class': 'mtpr-bgtext', x: p[0] + CELL / 2, y: p[1] + CELL / 2 + 0.5 }, layer);
        t.textContent = text;
        t.setAttribute('font-size', Math.min(CELL * 0.32, 11 / scale));
        if (!bgTextOn) t.setAttribute('display', 'none');
      }
    }
    function renderAll() { for (var i = 0; i < N; i++) renderCell(i); updateToolbar(); }
    // After anything that changes the colours or the label list.
    function redraw() {
      fillNow = core.fillScale(model());
      renderSide(); renderFillControls(); renderLegend(); renderAll(); layout();
    }

    function layout() {
      var w = svg.getBoundingClientRect().width;
      if (!w) return;
      scale = w / viewW;
      var cellPx = CELL * scale, fs = Math.min(11 / scale, (HDR_W - 8) / (rowW * 0.62));
      var cs = 1; while (cs * cellPx < 15 && cs < 16) cs *= 2;
      var rs = 1; while (rs * cellPx < 13 && rs < 16) rs *= 2;
      colHdrs.forEach(function (t, c) { t.setAttribute('font-size', fs); t.setAttribute('display', c % cs === 0 ? '' : 'none'); });
      rowHdrs.forEach(function (t, r) { t.setAttribute('font-size', fs); t.setAttribute('display', r % rs === 0 ? '' : 'none'); });
      var on = cellPx >= 24;
      svg.querySelectorAll('.mtpr-bgtext').forEach(function (t) {
        t.setAttribute('font-size', Math.min(CELL * 0.32, 11 / scale)); t.setAttribute('display', on ? '' : 'none');
      });
      bgTextOn = on;
    }

    function renderFillControls() {
      if (!fillSelect) return;
      while (fillSelect.firstChild) fillSelect.removeChild(fillSelect.firstChild);
      var options = ['label'].concat(Object.keys(groupSettings).length ? ['group'] : [], attrNames);
      options.forEach(function (o) { h('option', { value: o }, fillSelect, o); });
      fillSelect.value = fill;
      var numeric = fillNow.type === 'continuous';
      logBtn.classList.toggle('mtpr-hide', !numeric);
      logBtn.setAttribute('aria-pressed', logScale ? 'true' : 'false');
      textBtn.setAttribute('aria-pressed', wellText ? 'true' : 'false');
    }
    function renderLegend() {
      while (legendEl.firstChild) legendEl.removeChild(legendEl.firstChild);
      if (fill === 'label') return;
      h('strong', {}, legendEl, fill);
      if (fillNow.type === 'continuous') {
        h('span', {}, legendEl, String(fillNow.low));
        h('span', { 'class': 'mtpr-ramp' }, legendEl);
        h('span', {}, legendEl, fillNow.high + (fillNow.log ? ' (log)' : ''));
      } else {
        fillNow.keys.forEach(function (key) {
          var item = h('span', { 'class': 'mtpr-legend-key' }, legendEl);
          h('span', { 'class': 'mtpr-swatch' }, item).style.background = key.colour;
          h('span', {}, item, key.name);
        });
      }
      if (fillNow.colour.indexOf(null) >= 0) {
        var na = h('span', { 'class': 'mtpr-legend-key' }, legendEl);
        h('span', { 'class': 'mtpr-swatch' }, na).style.background = core.FILL_NA;
        h('span', {}, na, 'NA');
      }
    }

    // ---- labels and groups -----------------------------------------------
    var editing = null;   // label index or group name being edited
    function textInput(parent, name, value, onChange) {
      var lab = h('label', { 'class': 'mtpr-field' }, parent, name);
      var input = h('input', { type: 'text', 'class': 'mtpr-input' }, lab);
      input.value = value == null ? '' : value;
      input.addEventListener('change', function () { onChange(input.value.trim(), input); });
      return input;
    }
    function changeModel(fn) { var before = snapshot(); fn(); redraw(); commit(before); }
    function renderLabelRow(parent, k, n) {
      var l = labels[k], row = h('div', { 'class': 'mtpr-row' }, parent);
      var b = h('button', { type: 'button', 'class': 'mtpr-label', 'aria-pressed': 'false' }, row);
      h('span', { 'class': 'mtpr-swatch' }, b).style.background = l.colour;
      h('span', { 'class': 'mtpr-name' }, b, l.name);
      countEls[k] = h('span', { 'class': 'mtpr-count' }, b, '0');
      if (n < 9) h('span', { 'class': 'mtpr-key' }, b, String(n + 1));
      b.addEventListener('click', function () { setActive(k); });
      rowBtns[k] = b;
      if (editable) {
        h('button', { type: 'button', 'class': 'mtpr-edit', 'aria-label': 'Edit ' + l.name }, row, 'Edit').addEventListener('click', function () {
          editing = editing === k ? null : k; renderSide(); updateToolbar();
        });
      }
      if (editing === k) renderLabelEditor(parent, k);
    }
    function renderLabelEditor(parent, k) {
      var l = labels[k], box = h('div', { 'class': 'mtpr-editor' }, parent);
      textInput(box, 'label', l.name, function (v, input) {
        if (!v || (v !== l.name && labelIndex(v) >= 0)) { input.value = l.name; say('Labels must be unique and not empty.'); return; }
        changeModel(function () { l.name = v; });
      });
      var groupInput = textInput(box, 'group', l.group, function (v) { changeModel(function () { l.group = v || null; syncGroups(); }); });
      var listId = id + '-groups';
      groupInput.setAttribute('list', listId);
      var list = h('datalist', { id: listId }, box);
      Object.keys(groupSettings).forEach(function (g) { h('option', { value: g }, list); });
      var colLab = h('label', { 'class': 'mtpr-field' }, box, 'colour');
      var colInput = h('input', { type: 'color', 'class': 'mtpr-colour' }, colLab);
      colInput.value = l.colour.toLowerCase();
      colInput.addEventListener('change', function () { changeModel(function () { l.colour = colInput.value.toUpperCase(); }); });
      attrNames.forEach(function (a) {
        textInput(box, a, l.attrs[a], function (v) {
          changeModel(function () { l.attrs[a] = v === '' ? null : attrTypes[a] === 'number' && isFinite(Number(v)) ? Number(v) : v; if (v !== '' && !isFinite(Number(v))) attrTypes[a] = 'text'; });
        });
      });
      var actions = h('div', { 'class': 'mtpr-actions' }, box);
      h('button', { type: 'button', 'class': 'mtpr-btn' }, actions, 'Add attribute').addEventListener('click', function () {
        var input = h('input', { type: 'text', 'class': 'mtpr-input', 'aria-label': 'Attribute name', placeholder: 'attribute name' }, box);
        input.focus();
        input.addEventListener('change', function () {
          var v = input.value.trim();
          if (!v || attrNames.indexOf(v) >= 0 || ['well', 'label', 'group'].indexOf(v) >= 0) { say('Attribute names must be new, and not well, label or group.'); return; }
          changeModel(function () { attrNames.push(v); attrTypes[v] = 'number'; labels.forEach(function (o) { o.attrs[v] = null; }); });
        });
      });
      h('button', { type: 'button', 'class': 'mtpr-btn' }, actions, 'Delete label').addEventListener('click', function () { deleteLabel(k); });
    }
    function deleteLabel(k) {
      if (labels.length < 2) { say('Keep at least one label.'); return; }
      var name = labels[k].name;
      changeModel(function () {
        labels.splice(k, 1);
        assign = assign.map(function (a) {
          return a.filter(function (v) { return v !== k; }).map(function (v) { return v > k ? v - 1 : v; });
        });
        if (active >= labels.length) active = labels.length - 1;
        else if (active > k) active--;
        editing = null;
      });
      say('Deleted ' + name + '. Undo brings it back.');
    }
    function addLabel(group) {
      var n = labels.length + 1, name = 'Label ' + n;
      while (labelIndex(name) >= 0) name = 'Label ' + (++n);
      changeModel(function () {
        var attrs = {}; attrNames.forEach(function (a) { attrs[a] = null; });
        labels.push({ name: name, colour: core.nextColour(labels.map(function (l) { return l.colour; })), group: group || null, attrs: attrs });
        active = labels.length - 1; activeGroup = null; eraser = false; editing = active;
      });
    }
    function addGroup() {
      var n = Object.keys(groupSettings).length + 1, name = 'Group ' + n;
      while (groupSettings[name]) name = 'Group ' + (++n);
      changeModel(function () { groupSettings[name] = newGroup(); editing = 'group:' + name; });
    }
    function renameGroup(oldName, v, input) {
      if (!v || (v !== oldName && groupSettings[v])) { input.value = oldName; say('Group names must be unique and not empty.'); return; }
      changeModel(function () {
        var next = {};
        Object.keys(groupSettings).forEach(function (g) { next[g === oldName ? v : g] = groupSettings[g]; });
        groupSettings = next;
        labels.forEach(function (l) { if (l.group === oldName) l.group = v; });
        if (activeGroup === oldName) activeGroup = v;
        editing = 'group:' + v;
      });
    }
    function deleteGroup(g) {
      changeModel(function () {
        delete groupSettings[g];
        labels.forEach(function (l) { if (l.group === g) l.group = null; });
        if (activeGroup === g) activeGroup = null;
        editing = null;
      });
    }
    function numberField(parent, name, g, key) {
      var lab = h('label', { 'class': 'mtpr-field' }, parent, name);
      var input = h('input', { type: 'number', min: 1, step: 1, 'class': 'mtpr-input' }, lab);
      input.value = g[key];
      input.addEventListener('change', function () {
        var v = Math.floor(Number(input.value));
        if (!(v >= 1)) { input.value = g[key]; return; }
        changeModel(function () { g[key] = v; });
      });
    }
    function selectField(parent, name, g, key, values) {
      var lab = h('label', { 'class': 'mtpr-field' }, parent, name);
      var select = h('select', { 'class': 'mtpr-select' }, lab);
      values.forEach(function (v) { h('option', { value: v }, select, v); });
      select.value = g[key];
      select.addEventListener('change', function () { changeModel(function () { g[key] = select.value; }); });
    }
    function renderGroupBox(parent, name, order) {
      var g = groupSettings[name], box = h('div', { 'class': 'mtpr-group' + (activeGroup === name ? ' mtpr-group-on' : '') }, parent);
      var head = h('div', { 'class': 'mtpr-row' }, box);
      h('span', { 'class': 'mtpr-group-name' }, head, name);
      if (editable) {
        var seq = h('button', { type: 'button', 'class': 'mtpr-edit', 'aria-pressed': g.sequence ? 'true' : 'false' }, head, 'In sequence: ' + (g.sequence ? 'on' : 'off'));
        seq.addEventListener('click', function () {
          changeModel(function () { g.sequence = !g.sequence; activeGroup = g.sequence ? name : activeGroup === name ? null : activeGroup; eraser = false; });
        });
        h('button', { type: 'button', 'class': 'mtpr-edit', 'aria-label': 'Edit group ' + name }, head, 'Edit').addEventListener('click', function () {
          editing = editing === 'group:' + name ? null : 'group:' + name; renderSide(); updateToolbar();
        });
      }
      if (editing === 'group:' + name) {
        var ed = h('div', { 'class': 'mtpr-editor' }, box);
        textInput(ed, 'group', name, function (v, input) { renameGroup(name, v, input); });
        var act = h('div', { 'class': 'mtpr-actions' }, ed);
        h('button', { type: 'button', 'class': 'mtpr-btn' }, act, 'Add label').addEventListener('click', function () { addLabel(name); });
        h('button', { type: 'button', 'class': 'mtpr-btn' }, act, 'Delete group').addEventListener('click', function () { deleteGroup(name); });
      }
      labels.forEach(function (l, k) { if (l.group === name) renderLabelRow(box, k, order.indexOf(k)); });
      if (g.sequence) {
        var ctl = h('div', { 'class': 'mtpr-editor' }, box);
        if (activeGroup !== name) {
          h('button', { type: 'button', 'class': 'mtpr-btn' }, ctl, 'Paint this group').addEventListener('click', function () { activeGroup = name; eraser = false; renderSide(); updateToolbar(); });
        }
        selectField(ctl, 'pattern', g, 'pattern', ['grouped', 'dealt']);
        numberField(ctl, 'num_replicates', g, 'num_replicates');
        numberField(ctl, 'inter_spacing', g, 'inter_spacing');
        numberField(ctl, 'intra_spacing', g, 'intra_spacing');
        selectField(ctl, 'fill_wise', g, 'fill_wise', ['row', 'col']);
        h('p', { 'class': 'mtpr-hint' }, ctl, 'Click a well to set start_position. Locked wells are reserved.');
        if (lastCall && activeGroup === name) h('code', { 'class': 'mtpr-call' }, ctl, lastCall);
      }
    }
    function renderSide() {
      if (!side) return;
      while (side.firstChild) side.removeChild(side.firstChild);
      countEls = {}; rowBtns = {};
      if (typeof editing === 'number' && !labels[editing]) editing = null;
      var head = h('div', { 'class': 'mtpr-row' }, side);
      h('h3', { 'class': 'mtpr-heading' }, head, 'labels');
      if (editable) {
        h('button', { type: 'button', 'class': 'mtpr-btn' }, head, 'Add label').addEventListener('click', function () { addLabel(null); });
        h('button', { type: 'button', 'class': 'mtpr-btn' }, head, 'Add group').addEventListener('click', addGroup);
      }
      var order = labelOrder();
      Object.keys(groupSettings).forEach(function (g) { renderGroupBox(side, g, order); });
      labels.forEach(function (l, k) { if (l.group == null) renderLabelRow(side, k, order.indexOf(k)); });
    }
    // Labels in the order the list shows them, which is what keys 1-9 follow.
    function labelOrder() {
      var out = [];
      Object.keys(groupSettings).forEach(function (g) { labels.forEach(function (l, k) { if (l.group === g) out.push(k); }); });
      labels.forEach(function (l, k) { if (l.group == null) out.push(k); });
      return out;
    }

    function updateToolbar() {
      var cnt = labels.map(function () { return 0; });
      assign.forEach(function (a) { a.forEach(function (k) { cnt[k]++; }); });
      if (!toolbar) return;
      labels.forEach(function (l, k) {
        if (!rowBtns[k]) return;
        rowBtns[k].setAttribute('aria-pressed', !eraser && activeGroup == null && k === active ? 'true' : 'false');
        countEls[k].textContent = cnt[k];
      });
      eraserBtn.setAttribute('aria-pressed', eraser ? 'true' : 'false');
      undoBtn.disabled = !undoStack.length;
      redoBtn.disabled = !redoStack.length;
    }

    function setFocus(i) {
      focusIdx = Math.max(0, Math.min(N - 1, i));
      var p = cellPos(focusIdx);
      focusEl.setAttribute('x', p[0]); focusEl.setAttribute('y', p[1]);
      svg.setAttribute('aria-activedescendant', id + '-c' + focusIdx);
    }
    function say(msg) { status.textContent = msg; }

    // ---- reporting -------------------------------------------------------
    // Column-wise, so plate_picker_value() can build a data frame: `group` and
    // `attributes` are there only when the labels have them.
    function valueObj() {
      var v = core.valueRows(model()), out = { well: [], label: [] };
      var grouped = v.columns.indexOf('group') >= 0;
      if (grouped) out.group = [];
      if (attrNames.length) { out.attributes = {}; attrNames.forEach(function (a) { out.attributes[a] = []; }); }
      v.rows.forEach(function (r) {
        out.well.push(r.well); out.label.push(r.label);
        if (grouped) out.group.push(r.group);
        attrNames.forEach(function (a) { out.attributes[a].push(r[a]); });
      });
      return out;
    }
    function emit() {
      var v = valueObj();
      if (inShiny() && el.id) {
        try { window.Shiny.setInputValue(el.id, v); } catch (e) { /* not connected yet */ }
      }
      el.dispatchEvent(new CustomEvent('mtpr:change', { detail: v, bubbles: true }));
    }
    function emitActive() {
      if (inShiny() && el.id) {
        try { window.Shiny.setInputValue(el.id + '_active', labels[active].name); } catch (e) { /* ignore */ }
      }
    }
    // `before` is the snapshot taken before the change; push it unless nothing changed.
    function commit(before) {
      var now = snapshot();
      if (before && sameSnap(before, now)) { updateToolbar(); return false; }
      if (before) { undoStack.push(before); if (undoStack.length > HISTORY) undoStack.shift(); redoStack = []; }
      updateToolbar(); emit(); return true;
    }
    function undo() {
      if (!undoStack.length) return;
      redoStack.push(snapshot()); var p = plate; restore(undoStack.pop()); if (p !== plate) emitPlate(); emit();
    }
    function redo() {
      if (!redoStack.length) return;
      undoStack.push(snapshot()); var p = plate; restore(redoStack.pop()); if (p !== plate) emitPlate(); emit();
    }
    function emitPlate() {
      if (inShiny() && el.id) {
        try { window.Shiny.setInputValue(el.id + '_plate', plate); } catch (e) { /* ignore */ }
      }
    }
    function setActive(k) {
      if (k < 0 || k >= labels.length) return;
      eraser = false; activeGroup = null;
      var changed = k !== active; active = k;
      updateToolbar(); if (changed) emitActive();
    }

    // ---- exports (outside Shiny) ----------------------------------------
    function rows() { var v = valueObj(); return v.well.map(function (w, n) { return [w, v.label[n]]; }); }
    function csvText() { return core.csvText(model()); }
    function download(name, text, type) {
      var blob = text instanceof Blob ? text : new Blob([text], { type: type });
      var a = document.createElement('a');
      a.href = URL.createObjectURL(blob); a.download = name; a.style.display = 'none';
      document.body.appendChild(a); a.click();
      setTimeout(function () { URL.revokeObjectURL(a.href); document.body.removeChild(a); }, 1000);
    }
    function copyRows() {
      var text = 'Well\tLabel\n' + rows().map(function (r) { return r[0] + '\t' + r[1]; }).join('\n');
      var n = rows().length;
      function fallback() {
        var ta = document.createElement('textarea'); ta.value = text; ta.style.position = 'fixed'; ta.style.opacity = '0';
        document.body.appendChild(ta); ta.select();
        var ok = false; try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
        document.body.removeChild(ta); say(ok ? 'Copied ' + n + ' rows.' : 'Could not copy.');
      }
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(function () { say('Copied ' + n + ' rows.'); }, fallback);
      } else fallback();
    }
    function baseName() { return title ? slug(title) || 'plate-picker' : 'plate-picker'; }
    function stateJSON() {
      var at = {};
      attrNames.forEach(function (a) { at[a] = labels.map(function (l) { return l.attrs[a]; }); });
      return JSON.stringify({
        version: 2, plate: plate, title: title,
        labels: labels.map(function (l) { return l.name; }),
        colours: labels.map(function (l) { return l.colour; }),
        groups: labels.map(function (l) { return l.group; }),
        attributes: at, attributeTypes: attrTypes, groupSettings: groupSettings,
        fill: fill, log: logScale, locked: lockedWells(), assignments: valueObj()
      }).replace(/</g, '\\u003c');
    }
    function pageHTML() {
      var clone = document.documentElement.cloneNode(true);
      var root = clone.querySelector('#' + (window.CSS && CSS.escape ? CSS.escape(el.id) : el.id));
      if (root) { root.innerHTML = ''; root.classList.remove('html-widget-static-bound', 'mtpr-picker'); root.removeAttribute('data-theme'); }
      var block = clone.querySelector('#' + STATE_ID);
      if (!block) {
        block = document.createElement('script');
        block.setAttribute('type', 'application/json'); block.setAttribute('id', STATE_ID);
        clone.querySelector('body').appendChild(block);
      }
      block.textContent = stateJSON();
      return '<!DOCTYPE html>\n' + clone.outerHTML;
    }
    function savePage() { download(baseName() + '.html', pageHTML(), 'text/html;charset=utf-8'); say('Saved a copy of this page with the painting.'); }

    // ---- panels ------------------------------------------------------------
    function closePanel() {
      if (!panel) return;
      panel.classList.add('mtpr-hide');
      while (panel.firstChild) panel.removeChild(panel.firstChild);
      if (plateSelect) plateSelect.value = String(plate);
    }
    function openPanel(heading) {
      closePanel();
      panel.classList.remove('mtpr-hide');
      var head = h('div', { 'class': 'mtpr-row' }, panel);
      var hd = h('h3', { 'class': 'mtpr-heading', tabindex: '-1' }, head, heading);
      h('button', { type: 'button', 'class': 'mtpr-btn' }, head, 'Close').addEventListener('click', closePanel);
      hd.focus({ preventScroll: true });
      return h('div', { 'class': 'mtpr-panel-body' }, panel);
    }
    function pickButtons(parent, name, values, current, onPick) {
      var box = h('div', { 'class': 'mtpr-choice', role: 'group', 'aria-label': name }, parent);
      h('span', { 'class': 'mtpr-choice-name' }, box, name);
      values.forEach(function (v) {
        h('button', { type: 'button', 'class': 'mtpr-btn', 'aria-pressed': String(v[0]) === String(current) ? 'true' : 'false' }, box, v[1]).addEventListener('click', function () { onPick(v[0]); });
      });
    }

    // Changing the plate.
    function setPlateTo(next, method, wise) {
      var moved = core.movePlate(assign, info, core.plateInfo(next), method, wise);
      var keepLocked = lockedWells();
      setPlateSize(next); assign = moved.assign; setLocked(keepLocked); setBackground(bgData);
      buildGrid(); redraw(); emitPlate();
      return moved;
    }
    function askPlate(next) {
      if (next === plate) return;
      if (!assign.some(function (a) { return a.length; })) {
        changePlate(next, 'keep', 'row');
        return;
      }
      var method = 'keep', wise = 'row';
      var to = core.plateInfo(next);
      var draw = function () {
        var body2 = openPanel('Change plate: ' + plate + ' to ' + next + ' wells');
        if (plateSelect) plateSelect.value = String(next);
        var moved = core.movePlate(assign, info, to, method, wise);
        var views = h('div', { 'class': 'mtpr-compare' }, body2);
        var opts = { headers: true, legend: false, text: false, title: null };
        var before = h('figure', { 'class': 'mtpr-figure' }, views);
        h('figcaption', {}, before, 'Now: ' + plate + ' wells');
        h('div', {}, before).innerHTML = core.plateSVG(model(), Object.assign({ marks: moved.lost, width: 340 }, opts)).svg;
        var after = h('figure', { 'class': 'mtpr-figure' }, views);
        h('figcaption', {}, after, 'After: ' + next + ' wells');
        var nextModel = Object.assign(model(), { info: to, plate: next, assign: moved.assign });
        h('div', {}, after).innerHTML = core.plateSVG(nextModel, Object.assign({ width: 340 }, opts)).svg;
        var methods = [['keep', 'Keep positions'], ['fill', 'Fill in order']];
        if (core.canQuadrant(info, to)) methods.push(['quadrants', 'quadrants: one well becomes four']);
        var side2 = h('div', { 'class': 'mtpr-compare-side' }, views);
        pickButtons(side2, 'wells', methods, method, function (m) { method = m; draw(); });
        if (method === 'fill') pickButtons(side2, 'fill_wise', [['row', 'row'], ['col', 'col']], wise, function (w) { wise = w; draw(); });
        var count = assign.filter(function (a) { return a.length; }).length;
        h('p', { 'class': 'mtpr-result' }, side2, moved.lost.length ? moved.lost.length + ' of ' + count + ' painted wells will be removed. Undo brings them back.' : 'All ' + count + ' painted wells fit.');
        var act = h('div', { 'class': 'mtpr-actions' }, side2);
        h('button', { type: 'button', 'class': 'mtpr-btn' }, act, 'Cancel').addEventListener('click', closePanel);
        h('button', { type: 'button', 'class': 'mtpr-btn mtpr-primary' }, act, 'Change plate').addEventListener('click', function () { changePlate(next, method, wise); });
      };
      draw();
    }
    function changePlate(next, method, wise) {
      var before = snapshot();
      var moved = setPlateTo(next, method, wise);
      closePanel();
      commit(before);
      say('Now a ' + next + '-well plate.' + (moved.lost.length ? ' Removed ' + moved.lost.length + ' wells.' : ''));
    }

    // Loading a CSV.
    function readFile() {
      var file = fileInput.files && fileInput.files[0];
      if (!file) return;
      var reader = new FileReader();
      reader.onload = function () { openImport(file.name, core.parseTable(reader.result)); };
      reader.onerror = function () { say('Could not read ' + file.name + '.'); };
      reader.readAsText(file);
    }
    function openImport(name, table) {
      if (!table.header.length) { say(name + ' has no rows.'); return; }
      var roles = core.columnRoles(table);
      var choices = [['well', 'well'], ['label', 'label'], ['group', 'group'], ['number', 'number attribute'], ['text', 'text attribute'], ['ignore', 'ignore']];
      var draw = function () {
        var body2 = openPanel('Load ' + name);
        var plan = core.planImport(table, roles, info, multiple);
        h('p', { 'class': 'mtpr-hint' }, body2, table.rows.length + ' rows. ' + (plan.kind === 'layout' ? 'A finished layout: it replaces the painting on this plate.' : 'A list of labels to paint with.'));
        var scroll = h('div', { 'class': 'mtpr-scroll' }, body2);
        var tbl = h('table', { 'class': 'mtpr-table' }, scroll);
        var headRow = h('tr', {}, h('thead', {}, tbl));
        table.header.forEach(function (col, j) {
          var th = h('th', { scope: 'col' }, headRow, col);
          var select = h('select', { 'class': 'mtpr-select', 'aria-label': 'Use ' + col + ' as' }, th);
          choices.forEach(function (c) { h('option', { value: c[0] }, select, c[1]); });
          select.value = roles[j];
          select.addEventListener('change', function () { roles[j] = select.value; draw(); });
        });
        var tb = h('tbody', {}, tbl);
        table.rows.slice(0, 6).forEach(function (r, n) {
          var tr = h('tr', { 'class': plan.badRows.indexOf(n) >= 0 ? 'mtpr-badrow' : '' }, tb);
          r.forEach(function (c) { h('td', {}, tr, c); });
        });
        plan.checks.forEach(function (c) {
          var row = h('div', { 'class': 'mtpr-check' }, body2);
          h('strong', { 'class': 'mtpr-level-' + c.level.toLowerCase() }, row, c.level);
          h('span', {}, row, c.text);
        });
        var act = h('div', { 'class': 'mtpr-actions' }, body2);
        h('button', { type: 'button', 'class': 'mtpr-btn' }, act, 'Cancel').addEventListener('click', closePanel);
        var go = h('button', { type: 'button', 'class': 'mtpr-btn mtpr-primary' }, act, plan.kind === 'layout' ? 'Load ' + plan.placed.length + ' wells' : 'Add ' + plan.labels.length + ' labels');
        go.disabled = !plan.ok;
        go.addEventListener('click', function () { applyImport(plan); });
      };
      draw();
    }
    function applyImport(plan) {
      closePanel();
      var skipped = 0;
      changeModel(function () {
        // The plain "selected" label makes way for real ones.
        if (labels.length === 1 && labels[0].name === 'selected' && !assign.some(function (a) { return a.length; })) labels = [];
        plan.attrs.forEach(function (a) {
          if (attrNames.indexOf(a.name) < 0) {
            attrNames.push(a.name);
            labels.forEach(function (l) { l.attrs[a.name] = null; });
          }
          attrTypes[a.name] = a.type;
        });
        var fresh = [];
        plan.labels.forEach(function (rec) {
          var k = labelIndex(rec.name);
          if (k < 0) {
            var attrs = {}; attrNames.forEach(function (a) { attrs[a] = null; });
            labels.push({ name: rec.name, colour: null, group: rec.group, attrs: attrs });
            k = labels.length - 1; fresh.push(k);
          } else if (rec.group != null) labels[k].group = rec.group;
          Object.keys(rec.attrs).forEach(function (a) { labels[k].attrs[a] = rec.attrs[a]; });
        });
        var cols = core.defaultColours(labels.map(function (l) { return l.group; }));
        fresh.forEach(function (k) { labels[k].colour = cols[k]; });
        syncGroups();
        if (plan.kind === 'layout') {
          for (var i = 0; i < N; i++) if (!locked[i]) assign[i] = [];
          plan.placed.forEach(function (p) {
            var w = index[p.well];
            if (locked[w]) { skipped++; return; }
            assign[w].push(labelIndex(p.label));
          });
          assign.forEach(function (a) { a.sort(function (p, q) { return p - q; }); });
        }
        active = Math.max(0, Math.min(active, labels.length - 1));
      });
      say('Loaded ' + (plan.kind === 'layout' ? plan.placed.length - skipped + ' wells' : plan.labels.length + ' labels') + (skipped ? '; ' + skipped + ' locked wells were skipped.' : '.'));
    }

    // Exporting.
    var exportOpts = { format: 'PNG', width: 2400, title: true, legend: true, text: true, headers: true, background: 'white' };
    function imageOpts(o) {
      return { title: o.title ? title || 'Plate map' : null, legend: o.legend, text: o.text, headers: o.headers, width: o.width, background: o.background };
    }
    function pngBlob(o) {
      return new Promise(function (resolve, reject) {
        var pic = core.plateSVG(model(), imageOpts(o));
        var img = new Image();
        img.onload = function () {
          var canvas = document.createElement('canvas');
          canvas.width = pic.width; canvas.height = pic.height;
          canvas.getContext('2d').drawImage(img, 0, 0, pic.width, pic.height);
          try { canvas.toBlob(function (b) { if (b) resolve(b); else reject(new Error('empty image')); }, 'image/png'); } catch (e) { reject(e); }
        };
        img.onerror = function () { reject(new Error('the image could not be drawn')); };
        img.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(pic.svg);
      });
    }
    function openExport() {
      var o = exportOpts;
      var draw = function () {
        var body2 = openPanel('Export');
        var views = h('div', { 'class': 'mtpr-compare' }, body2);
        var side2 = h('div', { 'class': 'mtpr-compare-side' }, views);
        pickButtons(side2, 'format', [['PNG', 'PNG'], ['SVG', 'SVG'], ['CSV', 'CSV']], o.format, function (v) { o.format = v; draw(); });
        if (o.format === 'PNG') pickButtons(side2, 'width', [[1200, '1200 px'], [2400, '2400 px'], [4800, '4800 px']], o.width, function (v) { o.width = v; draw(); });
        if (o.format !== 'CSV') {
          [['title', 'title'], ['legend', 'legend'], ['text', 'well text'], ['headers', 'row and column labels']].forEach(function (t) {
            pickButtons(side2, t[1], [[true, 'on'], [false, 'off']], o[t[0]], function (v) { o[t[0]] = v; draw(); });
          });
          pickButtons(side2, 'background', [['white', 'white'], ['transparent', 'transparent']], o.background, function (v) { o.background = v; draw(); });
          var fig = h('figure', { 'class': 'mtpr-figure mtpr-export-preview' }, views);
          h('figcaption', {}, fig, 'Preview');
          h('div', {}, fig).innerHTML = core.plateSVG(model(), Object.assign(imageOpts(o), { width: 520 })).svg;
        } else {
          h('p', { 'class': 'mtpr-hint' }, side2, 'One row per well: well, label, group and each attribute. Read it with read_plate_picker().');
        }
        var act = h('div', { 'class': 'mtpr-actions' }, side2);
        if (o.format === 'CSV') h('button', { type: 'button', 'class': 'mtpr-btn' }, act, 'Copy rows').addEventListener('click', copyRows);
        h('button', { type: 'button', 'class': 'mtpr-btn mtpr-primary' }, act, 'Download ' + baseName() + '.' + o.format.toLowerCase()).addEventListener('click', function () { runExport(o); });
      };
      draw();
    }
    function runExport(o) {
      var file = baseName() + '.' + o.format.toLowerCase();
      if (o.format === 'CSV') { download(file, csvText(), 'text/csv;charset=utf-8'); say('Downloaded the CSV.'); return; }
      if (o.format === 'SVG') { download(file, core.plateSVG(model(), imageOpts(o)).svg, 'image/svg+xml;charset=utf-8'); say('Downloaded the SVG.'); return; }
      pngBlob(o).then(function (b) { download(file, b, 'image/png'); say('Downloaded the PNG.'); }, function (e) {
        say('Could not make a PNG here (' + e.message + '). The SVG export still works.');
      });
    }

    // ---- pointer interaction ---------------------------------------------
    function cellAt(ev) {
      var e = document.elementFromPoint(ev.clientX, ev.clientY);
      var g = e && e.closest ? e.closest('.mtpr-cell') : null;
      return g && svg.contains(g) ? +g.getAttribute('data-i') : -1;
    }
    function showPreview(a, b) {
      var p = rcOf(a), q = rcOf(b);
      var r0 = Math.min(p[0], q[0]), r1 = Math.max(p[0], q[0]), c0 = Math.min(p[1], q[1]), c1 = Math.max(p[1], q[1]);
      previewEl.setAttribute('x', PAD + HDR_W + c0 * CELL); previewEl.setAttribute('y', PAD + HDR_H + r0 * CELL);
      previewEl.setAttribute('width', (c1 - c0 + 1) * CELL); previewEl.setAttribute('height', (r1 - r0 + 1) * CELL);
      previewEl.removeAttribute('display');
    }
    function endDrag() {
      if (drag && drag.pid != null) { try { svg.releasePointerCapture(drag.pid); } catch (e) { /* already released */ } }
      previewEl.setAttribute('display', 'none');
      drag = null;
    }
    function lineAction(el2) {
      var idxs = [], r = el2.getAttribute('data-row'), c = el2.getAttribute('data-col');
      if (r != null) for (var cc2 = 0; cc2 < ncol; cc2++) idxs.push(+r * ncol + cc2);
      else for (var r2 = 0; r2 < nrow; r2++) idxs.push(r2 * ncol + +c);
      idxs = idxs.filter(function (i) { return !locked[i]; });
      if (!idxs.length) return;
      var before = snapshot();
      var allHave = !eraser && idxs.every(function (i) { return has(i, active); });
      var mode = eraser ? 'erase' : allHave ? 'remove' : 'add';
      idxs.forEach(function (i) { applyMode(i, mode); });
      commit(before);
    }

    // One click lays out the whole group from that well, like replicate_reactions().
    function placeSequence(start) {
      var g = groupSettings[activeGroup], members = [];
      labels.forEach(function (l, k) { if (l.group === activeGroup) members.push(k); });
      if (!members.length) { say('The group ' + activeGroup + ' has no labels.'); return; }
      var reserved = [];
      locked.forEach(function (on, i) { if (on) reserved.push(i); });
      var opts = { pattern: g.pattern, num_replicates: g.num_replicates, inter_spacing: g.inter_spacing, intra_spacing: g.intra_spacing, fill_wise: g.fill_wise, start_position: start, reserved: reserved };
      var res = core.sequenceLayout(members.length, opts, info);
      var before = snapshot();
      res.placed.forEach(function (p) { add(p.index, members[p.reaction]); });
      lastCall = core.sequenceCall(members.map(function (k) { return labels[k].name; }), opts, info);
      renderAll(); renderSide(); updateToolbar(); commit(before);
      say(res.placed.length + ' wells placed from ' + wells[start] + (res.lost ? '; ' + res.lost + ' did not fit.' : '.'));
    }

    svg.addEventListener('pointerdown', function (ev) {
      if (ev.pointerType === 'mouse' && ev.button !== 0) return;
      var hd = ev.target.closest ? ev.target.closest('.mtpr-hdhit') : null;
      if (hd) { ev.preventDefault(); svg.focus({ preventScroll: true }); lineAction(hd); return; }
      var i = cellAt(ev);
      if (i < 0) return;
      ev.preventDefault();
      svg.focus({ preventScroll: true });
      setFocus(i);
      if (locked[i]) { say(wells[i] + ' is locked.'); return; }
      if (activeGroup != null && !eraser) { placeSequence(i); return; }
      drag = { start: i, last: i, shift: ev.shiftKey, before: snapshot(), mode: modeFor(i), pid: ev.pointerId };
      try { svg.setPointerCapture(ev.pointerId); } catch (e) { /* synthetic events */ }
      if (drag.shift) showPreview(i, i); else applyMode(i, drag.mode);
      hideTip();
    });
    svg.addEventListener('pointermove', function (ev) {
      if (!drag) { hoverTip(ev); return; }
      var i = cellAt(ev);
      if (i < 0 || i === drag.last) return;
      if (drag.shift) { showPreview(drag.start, i); drag.last = i; return; }
      lineIdx(drag.last, i).forEach(function (j) { applyMode(j, drag.mode); });
      drag.last = i; setFocus(i);
    });
    svg.addEventListener('pointerup', function (ev) {
      if (!drag) return;
      var d = drag;
      if (d.shift) {
        var i = cellAt(ev); if (i < 0) i = d.last;
        rectIdx(d.start, i).forEach(function (j) { applyMode(j, d.mode); });
      }
      endDrag(); updateToolbar(); commit(d.before);
    });
    svg.addEventListener('pointercancel', function () { cancelDrag(); });
    svg.addEventListener('lostpointercapture', function () { if (drag) cancelDrag(); });
    function cancelDrag() {
      if (!drag) return;
      var d = drag; endDrag(); restore(d.before); renderAll(); say('Stroke cancelled.');
    }

    // ---- tooltip ---------------------------------------------------------
    function hideTip() { tip.style.display = 'none'; }
    function hoverTip(ev) {
      if (ev.pointerType && ev.pointerType !== 'mouse' && ev.pointerType !== 'pen') return;
      var i = cellAt(ev);
      if (i < 0) { hideTip(); return; }
      var lines = [wells[i] + (assign[i].length ? ': ' + labelNames(i) : '')];
      if (assign[i].length === 1) {
        var l = labels[assign[i][0]];
        if (l.group != null) lines.push('group: ' + l.group);
        attrNames.forEach(function (a) { if (l.attrs[a] != null) lines.push(a + ': ' + l.attrs[a]); });
      }
      if (bgTip[i]) lines.push(bgTip[i]);
      if (locked[i]) lines.push('locked');
      tip.textContent = lines.join('\n');
      tip.style.display = 'block';
      var wr = wrap.getBoundingClientRect();
      var left = ev.clientX - wr.left + 12, top = ev.clientY - wr.top + 14;
      if (left + tip.offsetWidth > wr.width) left = ev.clientX - wr.left - tip.offsetWidth - 8;
      tip.style.left = Math.max(0, left) + 'px'; tip.style.top = top + 'px';
    }
    svg.addEventListener('pointerleave', hideTip);

    // ---- keyboard --------------------------------------------------------
    function onKeydown(ev) {
      var tag = ev.target && ev.target.tagName;
      if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
      var mod = ev.ctrlKey || ev.metaKey;
      if (ev.key === 'Escape') {
        if (drag) { ev.preventDefault(); cancelDrag(); } else if (panel && !panel.classList.contains('mtpr-hide')) { ev.preventDefault(); closePanel(); }
        return;
      }
      if (mod && (ev.key === 'z' || ev.key === 'Z')) { ev.preventDefault(); if (ev.shiftKey) redo(); else undo(); return; }
      if (mod && (ev.key === 'y' || ev.key === 'Y')) { ev.preventDefault(); redo(); return; }
      if (mod || ev.altKey) return;
      if (/^[1-9]$/.test(ev.key)) { var ord = labelOrder(); if (ord[+ev.key - 1] != null) setActive(ord[+ev.key - 1]); return; }
      if (ev.key === 'e' || ev.key === 'E') { if (tag !== 'BUTTON' || toolbar) { eraser = !eraser; updateToolbar(); } return; }
      if (ev.target !== svg) return;
      var r = Math.floor(focusIdx / ncol), c = focusIdx % ncol, handled = true;
      switch (ev.key) {
        case 'ArrowLeft': c = Math.max(0, c - 1); break;
        case 'ArrowRight': c = Math.min(ncol - 1, c + 1); break;
        case 'ArrowUp': r = Math.max(0, r - 1); break;
        case 'ArrowDown': r = Math.min(nrow - 1, r + 1); break;
        case 'Home': c = 0; break;
        case 'End': c = ncol - 1; break;
        case ' ': case 'Enter': case 'Spacebar':
          if (locked[focusIdx]) { say(wells[focusIdx] + ' is locked.'); break; }
          var before = snapshot(); applyMode(focusIdx, modeFor(focusIdx)); commit(before);
          say(describe(focusIdx)); ev.preventDefault(); return;
        default: handled = false;
      }
      if (handled) { ev.preventDefault(); setFocus(r * ncol + c); }
    }
    el.addEventListener('keydown', onKeydown);

    // ---- updates from Shiny ----------------------------------------------
    function update(msg) {
      var before = snapshot(), moved = false;
      if (msg.labels != null) {
        var painted = valueObj(), oldGroups = groupSettings;
        groupSettings = {};
        setLabels(msg.labels);
        Object.keys(groupSettings).forEach(function (g) { if (oldGroups[g]) groupSettings[g] = oldGroups[g]; });
        active = Math.min(active, labels.length - 1);
        if (activeGroup != null && !groupSettings[activeGroup]) activeGroup = null;
        if (fill !== 'label' && fill !== 'group' && attrNames.indexOf(fill) < 0) fill = 'label';
        setValue(painted); moved = true;
      }
      if (msg.plate != null && +msg.plate !== plate) { setPlateTo(+msg.plate, 'keep', 'row'); moved = true; }
      if (msg.locked != null) { setLocked(msg.locked); moved = true; }
      if (msg.background != null) { setBackground(msg.background); moved = true; }
      if (msg.value != null) { setValue(msg.value); moved = true; }
      if (msg.fill != null) { fill = msg.fill; moved = true; }
      if (msg.active != null) { var k = labelIndex(msg.active); if (k >= 0) { eraser = false; activeGroup = null; var ch = k !== active; active = k; if (ch) emitActive(); } }
      if (moved) redraw();
      if (msg.value != null || msg.plate != null || msg.labels != null) commit(before); else updateToolbar();
    }

    // ---- start -----------------------------------------------------------
    buildGrid();
    redraw();
    var ro = null;
    if (window.ResizeObserver) { ro = new ResizeObserver(function () { layout(); }); ro.observe(el); }
    if (inShiny() && el.id) { emit(); emitActive(); }

    var picker = {
      update: update,
      value: valueObj,
      csv: csvText,
      pageHTML: pageHTML,
      svg: function (o) { return core.plateSVG(model(), o || {}).svg; },
      png: function (o) { return pngBlob(Object.assign({}, exportOpts, o || {})); },
      labels: function () { return clone(labels); },
      plate: function () { return plate; },
      importText: function (name, text) { openImport(name, core.parseTable(text)); },
      undo: undo, redo: redo,
      layout: layout,
      // `el` outlives a re-render, so its listener must not.
      destroy: function () { if (ro) ro.disconnect(); el.removeEventListener('keydown', onKeydown); }
    };
    return picker;
  }

  HTMLWidgets.widget({
    name: 'plate_picker',
    type: 'output',
    factory: function (el, width, height) {
      var inst = null;
      return {
        renderValue: function (x) {
          if (inst) inst.destroy();
          inst = createPicker(el, x);
          if (el.id) {
            registry[el.id] = inst;
            (pending[el.id] || []).forEach(function (m) { inst.update(m); });
            delete pending[el.id];
          }
        },
        resize: function () { if (inst) inst.layout(); },
        picker: function () { return inst; }
      };
    }
  });

  // Messages from update_plate_picker().
  var handlerSet = false;
  function registerHandler() {
    if (handlerSet || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    handlerSet = true;
    window.Shiny.addCustomMessageHandler('mtpr_plate_picker', function (msg) {
      var p = registry[msg.id];
      if (p) p.update(msg); else (pending[msg.id] = pending[msg.id] || []).push(msg);
    });
  }
  registerHandler();
  document.addEventListener('DOMContentLoaded', registerHandler);
  document.addEventListener('shiny:connected', registerHandler);
})();
