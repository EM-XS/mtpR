/* mtpR plate_picker: paint wells of a microtiter plate with labels.
 * Plain SVG, pointer events, no dependencies beyond htmlwidgets. */
(function () {
  'use strict';

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
  function pad2(n) { return n < 10 ? '0' + n : '' + n; }
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
  function slug(s) { return String(s).trim().replace(/[^\w.-]+/g, '-').replace(/^-+|-+$/g, '').toLowerCase(); }

  function createPicker(el, x) {
    var id = 'mtprp' + (++uid);
    var nrow = x.nrow, ncol = x.ncol, N = nrow * ncol;
    var rowLabels = arr(x.rowLabels);
    var labels = arr(x.labels);
    var colours = arr(x.colours);
    var multiple = !!x.multiple;
    var standalone = !inShiny();
    var showExport = x.exportButtons === 'auto' || x.exportButtons == null ? standalone : !!x.exportButtons;

    var wells = [], index = {};
    var rowW = Math.max.apply(null, rowLabels.map(function (s) { return s.length; }));
    var HDR_W = 12 + rowW * 9;
    for (var r = 0; r < nrow; r++) for (var c = 0; c < ncol; c++) {
      var name = rowLabels[r] + pad2(c + 1);
      index[name] = wells.length; wells.push(name);
    }

    // ---- state -----------------------------------------------------------
    var assign = [], locked = [], bgFill = [], bgText = [], bgTip = [];
    for (var i = 0; i < N; i++) { assign.push([]); locked.push(false); bgFill.push(null); bgText.push(null); bgTip.push(null); }
    var active = Math.max(0, labels.indexOf(x.active));
    var eraser = false;
    var undoStack = [], redoStack = [];
    var drag = null;
    var focusIdx = 0;
    var title = x.title || null;

    function setValue(v) {
      for (var i = 0; i < N; i++) assign[i] = [];
      var ws = arr(v && v.well), ls = arr(v && v.label);
      for (var j = 0; j < ws.length; j++) {
        var wi = index[ws[j]], k = labels.indexOf(ls[j]);
        if (wi != null && k >= 0 && assign[wi].indexOf(k) < 0) assign[wi].push(k);
      }
      assign.forEach(function (a) { a.sort(function (p, q) { return p - q; }); if (!multiple && a.length > 1) a.length = 1; });
    }
    function setLocked(v) {
      for (var i = 0; i < N; i++) locked[i] = false;
      arr(v).forEach(function (w) { if (index[w] != null) locked[index[w]] = true; });
    }
    function setBackground(b) {
      for (var i = 0; i < N; i++) { bgFill[i] = null; bgText[i] = null; bgTip[i] = null; }
      if (!b) return;
      var ws = arr(b.well), f = arr(b.fill), t = arr(b.text), tp = arr(b.tooltip);
      for (var j = 0; j < ws.length; j++) {
        var wi = index[ws[j]]; if (wi == null) continue;
        bgFill[wi] = f[j] || null;
        bgText[wi] = t[j] != null && t[j] !== '' ? String(t[j]) : null;
        bgTip[wi] = tp[j] != null && tp[j] !== '' ? String(tp[j]) : null;
      }
    }

    // A page saved by the widget keeps its painting in a JSON block.
    var initial = x.value;
    var stateEl = document.getElementById(STATE_ID);
    if (standalone && stateEl) {
      try {
        var st = JSON.parse(stateEl.textContent);
        if (st && st.plate === x.plate && st.assignments) initial = st.assignments;
        if (st && st.title && !title) title = st.title;
      } catch (e) { /* keep the widget data */ }
    }
    setValue(initial); setLocked(x.locked); setBackground(x.background);

    function snapshot() { return assign.map(function (a) { return a.slice(); }); }
    function sameSnap(a, b) { return JSON.stringify(a) === JSON.stringify(b); }
    function restore(s) { for (var i = 0; i < N; i++) assign[i] = s[i].slice(); }

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

    var toolbar = null, chipBtns = [], eraserBtn, undoBtn, redoBtn, clearBtn, exportEls = [];
    var countEls = [];
    if (x.toolbar !== false) {
      toolbar = h('div', { 'class': 'mtpr-toolbar', role: 'toolbar', 'aria-label': 'Plate picker tools' }, el);
      labels.forEach(function (lab, k) {
        var b = h('button', { type: 'button', 'class': 'mtpr-btn', 'aria-pressed': 'false', 'data-k': k }, toolbar);
        var sw = h('span', { 'class': 'mtpr-swatch' }, b); sw.style.background = colours[k];
        h('span', {}, b, lab);
        countEls.push(h('span', { 'class': 'mtpr-count' }, b, '0'));
        if (k < 9) h('span', { 'class': 'mtpr-key' }, b, String(k + 1));
        b.addEventListener('click', function () { setActive(k); });
        chipBtns.push(b);
      });
      eraserBtn = h('button', { type: 'button', 'class': 'mtpr-btn', 'aria-pressed': 'false', title: 'Eraser (E)' }, toolbar, 'Eraser');
      eraserBtn.addEventListener('click', function () { eraser = !eraser; updateToolbar(); });
      h('span', { 'class': 'mtpr-sep' }, toolbar);
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
      if (showExport) {
        exportEls.push(h('span', { 'class': 'mtpr-sep' }, toolbar));
        var cp = h('button', { type: 'button', 'class': 'mtpr-btn' }, toolbar, 'Copy rows');
        var dl = h('button', { type: 'button', 'class': 'mtpr-btn' }, toolbar, 'Download CSV');
        var sp = h('button', { type: 'button', 'class': 'mtpr-btn' }, toolbar, 'Save page');
        cp.addEventListener('click', copyRows);
        dl.addEventListener('click', downloadCsv);
        sp.addEventListener('click', savePage);
        exportEls.push(cp, dl, sp);
      }
    }

    var wrap = h('div', { 'class': 'mtpr-wrap', style: 'position:relative' }, el);
    var viewW = HDR_W + ncol * CELL + 2 * PAD, viewH = HDR_H + nrow * CELL + 2 * PAD;
    var svg = sv('svg', {
      'class': 'mtpr-grid', viewBox: '0 0 ' + viewW + ' ' + viewH, role: 'grid', tabindex: '0',
      'aria-label': 'Plate map, ' + x.plate + ' wells. Arrow keys move, Space paints.',
      'aria-rowcount': nrow + 1, 'aria-colcount': ncol, 'aria-multiselectable': 'true'
    }, wrap);
    svg.style.maxWidth = Math.round(viewW * MAX_CELL_PX / CELL) + 'px';
    svg.style.aspectRatio = viewW + ' / ' + viewH;
    var defs = sv('defs', {}, svg);
    var pat = sv('pattern', { id: 'mtpr-hatch', width: 8, height: 8, patternUnits: 'userSpaceOnUse', patternTransform: 'rotate(45)' }, defs);
    sv('line', { x1: 0, y1: 0, x2: 0, y2: 8, 'class': 'mtpr-hatch-line' }, pat);
    var clip = sv('clipPath', { id: id + '-clip', clipPathUnits: 'objectBoundingBox' }, defs);
    sv('rect', { width: 1, height: 1, rx: 0.22, ry: 0.22 }, clip);

    var colHdrs = [], rowHdrs = [];
    var hrow = sv('g', { role: 'row', 'aria-rowindex': 1 }, svg);
    for (var cc = 0; cc < ncol; cc++) {
      var cg = sv('g', { role: 'columnheader', 'aria-colindex': cc + 1, 'aria-label': 'Column ' + (cc + 1) }, hrow);
      sv('rect', { 'class': 'mtpr-hdhit', 'data-col': cc, x: PAD + HDR_W + cc * CELL, y: PAD, width: CELL, height: HDR_H }, cg);
      var ct = sv('text', { 'class': 'mtpr-hd', x: PAD + HDR_W + cc * CELL + CELL / 2, y: PAD + HDR_H / 2 + 1 }, cg);
      ct.textContent = String(cc + 1); colHdrs.push(ct);
    }
    var cellEls = [], chipLayers = [], textEls = [], lockEls = [], bases = [];
    var rowGroups = [];
    for (var rr = 0; rr < nrow; rr++) {
      var rg = sv('g', { role: 'row', 'aria-rowindex': rr + 2 }, svg);
      rowGroups.push(rg);
      var rh = sv('g', { role: 'rowheader', 'aria-label': 'Row ' + rowLabels[rr] }, rg);
      sv('rect', { 'class': 'mtpr-hdhit', 'data-row': rr, x: PAD, y: PAD + HDR_H + rr * CELL, width: HDR_W, height: CELL }, rh);
      var rt = sv('text', { 'class': 'mtpr-hd mtpr-rhd', x: PAD + HDR_W - 6, y: PAD + HDR_H + rr * CELL + CELL / 2 + 1 }, rh);
      rt.textContent = rowLabels[rr]; rowHdrs.push(rt);
      for (var c2 = 0; c2 < ncol; c2++) {
        var ii = rr * ncol + c2;
        var g = sv('g', { 'class': 'mtpr-cell', role: 'gridcell', id: id + '-c' + ii, 'data-i': ii, 'aria-colindex': c2 + 1, 'aria-selected': 'false' }, rg);
        var x0 = PAD + HDR_W + c2 * CELL, y0 = PAD + HDR_H + rr * CELL;
        bases.push(sv('rect', { 'class': 'mtpr-base', x: x0, y: y0, width: CELL, height: CELL }, g));
        chipLayers.push(sv('g', { 'class': 'mtpr-chips' }, g));
        lockEls.push(null); textEls.push(null);
        cellEls.push(g);
      }
    }
    var previewEl = sv('rect', { 'class': 'mtpr-preview', display: 'none' }, svg);
    var focusEl = sv('rect', { 'class': 'mtpr-focus', width: CELL, height: CELL }, svg);
    var tip = h('div', { 'class': 'mtpr-tip', role: 'tooltip' }, wrap);
    var status = h('div', { 'class': 'mtpr-status', 'aria-live': 'polite' }, el);

    // ---- rendering -------------------------------------------------------
    var scale = 1, bgTextOn = true;
    function cellPos(i) { var p = rcOf(i); return [PAD + HDR_W + p[1] * CELL, PAD + HDR_H + p[0] * CELL]; }
    function describe(i) {
      var s = wells[i];
      if (assign[i].length) s += ', ' + assign[i].map(function (k) { return labels[k]; }).join(' + ');
      if (bgTip[i]) s += ', ' + bgTip[i];
      if (locked[i]) s += ', locked';
      return s;
    }
    function renderCell(i) {
      var g = cellEls[i], layer = chipLayers[i], p = cellPos(i);
      while (layer.firstChild) layer.removeChild(layer.firstChild);
      if (bgFill[i]) bases[i].style.fill = bgFill[i]; else bases[i].style.fill = '';
      g.classList.toggle('mtpr-locked', locked[i]);
      g.setAttribute('aria-selected', assign[i].length ? 'true' : 'false');
      if (locked[i]) g.setAttribute('aria-disabled', 'true'); else g.removeAttribute('aria-disabled');
      g.setAttribute('aria-label', describe(i));
      var ins = CELL * 0.2, sz = CELL - 2 * ins, cx = p[0] + ins, cy = p[1] + ins, a = assign[i];
      if (locked[i]) sv('rect', { 'class': 'mtpr-lockfill', x: p[0], y: p[1], width: CELL, height: CELL }, layer);
      if (a.length) {
        sv('rect', { 'class': 'mtpr-halo', x: cx - 1.5, y: cy - 1.5, width: sz + 3, height: sz + 3, rx: 7, ry: 7 }, layer);
        if (a.length === 1) {
          sv('rect', { 'class': 'mtpr-chip', x: cx, y: cy, width: sz, height: sz, rx: 6, ry: 6, fill: colours[a[0]] }, layer);
        } else {
          var cg2 = sv('g', { 'clip-path': 'url(#' + id + '-clip)' }, layer);
          var bh = sz / a.length;
          a.forEach(function (k, n) { sv('rect', { x: cx, y: cy + n * bh, width: sz, height: bh + 0.3, fill: colours[k] }, cg2); });
          sv('rect', { 'class': 'mtpr-chip', x: cx, y: cy, width: sz, height: sz, rx: 6, ry: 6, fill: 'none' }, layer);
        }
      }
      if (bgText[i]) {
        var t = sv('text', { 'class': 'mtpr-bgtext', x: p[0] + CELL / 2, y: p[1] + CELL / 2 + 0.5 }, layer);
        t.textContent = bgText[i];
        t.setAttribute('font-size', Math.min(CELL * 0.4, 11 / scale));
        if (!bgTextOn) t.setAttribute('display', 'none');
      }
    }
    function renderAll() { for (var i = 0; i < N; i++) renderCell(i); updateToolbar(); }

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
      for (var i = 0; i < N; i++) {
        if (!bgText[i]) continue;
        var t = chipLayers[i].querySelector('.mtpr-bgtext');
        if (t) { t.setAttribute('font-size', Math.min(CELL * 0.4, 11 / scale)); t.setAttribute('display', on ? '' : 'none'); }
      }
      bgTextOn = on;
    }

    function updateToolbar() {
      var cnt = labels.map(function () { return 0; });
      assign.forEach(function (a) { a.forEach(function (k) { cnt[k]++; }); });
      if (!toolbar) return;
      chipBtns.forEach(function (b, k) {
        b.setAttribute('aria-pressed', !eraser && k === active ? 'true' : 'false');
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
    function valueObj() {
      var w = [], l = [];
      for (var i = 0; i < N; i++) assign[i].forEach(function (k) { w.push(wells[i]); l.push(labels[k]); });
      return { well: w, label: l };
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
        try { window.Shiny.setInputValue(el.id + '_active', labels[active]); } catch (e) { /* ignore */ }
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
      redoStack.push(snapshot()); restore(undoStack.pop()); renderAll(); emit();
    }
    function redo() {
      if (!redoStack.length) return;
      undoStack.push(snapshot()); restore(redoStack.pop()); renderAll(); emit();
    }
    function setActive(k) {
      if (k < 0 || k >= labels.length) return;
      eraser = false;
      var changed = k !== active; active = k;
      updateToolbar(); if (changed) emitActive();
    }

    // ---- exports (outside Shiny) ----------------------------------------
    function rows() { var v = valueObj(); return v.well.map(function (w, n) { return [w, v.label[n]]; }); }
    function csvCell(s) { return /[",\n\r]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s; }
    function csvText() { return 'well,label\n' + rows().map(function (r) { return csvCell(r[0]) + ',' + csvCell(r[1]); }).join('\n') + (rows().length ? '\n' : ''); }
    function download(name, text, type) {
      var blob = new Blob([text], { type: type });
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
    function downloadCsv() { download(baseName() + '.csv', csvText(), 'text/csv;charset=utf-8'); say('Downloaded the CSV.'); }
    function stateJSON() {
      return JSON.stringify({ version: 1, plate: x.plate, labels: labels, title: title, assignments: valueObj() }).replace(/</g, '\\u003c');
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
      var lines = [wells[i] + (assign[i].length ? ': ' + assign[i].map(function (k) { return labels[k]; }).join(' + ') : '')];
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
    el.addEventListener('keydown', function (ev) {
      var tag = ev.target && ev.target.tagName;
      if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
      var mod = ev.ctrlKey || ev.metaKey;
      if (ev.key === 'Escape') { if (drag) { ev.preventDefault(); cancelDrag(); } return; }
      if (mod && (ev.key === 'z' || ev.key === 'Z')) { ev.preventDefault(); if (ev.shiftKey) redo(); else undo(); return; }
      if (mod && (ev.key === 'y' || ev.key === 'Y')) { ev.preventDefault(); redo(); return; }
      if (mod || ev.altKey) return;
      if (/^[1-9]$/.test(ev.key)) { setActive(+ev.key - 1); return; }
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
    });

    // ---- updates from Shiny ----------------------------------------------
    function update(msg) {
      var before = snapshot(), moved = false;
      if (msg.locked != null) { setLocked(msg.locked); moved = true; }
      if (msg.background != null) { setBackground(msg.background); moved = true; }
      if (msg.value != null) { setValue(msg.value); moved = true; }
      if (msg.active != null) { var k = labels.indexOf(msg.active); if (k >= 0) { eraser = false; var ch = k !== active; active = k; if (ch) emitActive(); } }
      if (moved) { renderAll(); layout(); }
      if (msg.value != null) commit(before); else updateToolbar();
    }

    // ---- start -----------------------------------------------------------
    setFocus(0);
    renderAll();
    layout();
    var ro = null;
    if (window.ResizeObserver) { ro = new ResizeObserver(function () { layout(); }); ro.observe(el); }
    if (inShiny() && el.id) { emit(); emitActive(); }

    var picker = {
      update: update,
      value: valueObj,
      csv: csvText,
      pageHTML: pageHTML,
      undo: undo, redo: redo,
      layout: layout,
      destroy: function () { if (ro) ro.disconnect(); }
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
