(() => {
  "use strict";

  const $ = (selector, root = document) => root.querySelector(selector);
  const $$ = (selector, root = document) => [...root.querySelectorAll(selector)];

  function showView(name) {
    $$(".view").forEach((view) => view.classList.toggle("active", view.dataset.view === name));
    window.scrollTo({ top: 0, behavior: "smooth" });
    history.replaceState(null, "", name === "home" ? location.pathname : `#${name}`);
    if (gameTimers.has(name)) {
      gameTimers.start(name);
      window.Shiny?.setInputValue?.("game_opened", { game: name, nonce: Date.now() }, { priority: "event" });
    }
    if (name === "shadows") shadowRenderer.resize();
  }

  function openLeague(open = true) {
    $("#league_drawer")?.classList.toggle("open", open);
    $("#drawer_scrim")?.classList.toggle("open", open);
    $("#league_drawer")?.setAttribute("aria-hidden", open ? "false" : "true");
  }

  document.addEventListener("click", (event) => {
    const gameButton = event.target.closest("[data-game]");
    if (gameButton) showView(gameButton.dataset.game);
    if (event.target.closest("[data-open-league]")) openLeague(true);
    if (event.target.closest("[data-close-league]")) openLeague(false);
    if (event.target.closest("[data-edit-identity]")) $("#identity_modal")?.classList.remove("hidden");

    const candidate = event.target.closest(".candidate-row[data-candidate]");
    if (candidate && !candidate.closest(".candidate-list")?.classList.contains("answered")) {
      const choice = Number(candidate.dataset.candidate);
      const seconds = gameTimers.elapsed("curse");
      window.Shiny?.setInputValue?.("curse_pick", { choice, seconds, nonce: Date.now() }, { priority: "event" });
    }
  });

  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape") openLeague(false);
  });

  function connectShiny() {
    if (!window.Shiny?.addCustomMessageHandler || !window.Shiny?.setInputValue) {
      return setTimeout(connectShiny, 50);
    }

    Shiny.addCustomMessageHandler("saveIdentity", (_value) => {
      $("#identity_modal")?.classList.add("hidden");
      const activeGame = $(".view.active")?.dataset.view;
      if (gameTimers.has(activeGame)) {
        gameTimers.start(activeGame);
        Shiny.setInputValue("game_opened", { game: activeGame, nonce: Date.now() }, { priority: "event" });
      }
    });
    Shiny.addCustomMessageHandler("toggleButton", ({ id, enabled }) => {
      const button = document.getElementById(id);
      if (!button) return;
      button.disabled = !enabled;
      button.classList.toggle("disabled", !enabled);
    });
    Shiny.addCustomMessageHandler("resetGameTimer", ({ game }) => {
      gameTimers.reset(game);
      if (game === "curse") clearCurseAnswer();
    });
    Shiny.addCustomMessageHandler("stopGameTimer", ({ game }) => gameTimers.stop(game));
    Shiny.addCustomMessageHandler("clearGameTimers", (_message) => {
      gameTimers.clearAll();
      clearCurseAnswer();
    });
    Shiny.addCustomMessageHandler("setShadowTarget", ({ target }) => shadowRenderer.setTarget(target));
    Shiny.addCustomMessageHandler("curseAnswer", ({ choice, nearest }) => {
      const list = $(".candidate-list");
      list?.classList.add("answered");
      $$(`.candidate-row`).forEach((row) => {
        row.classList.toggle("selected", Number(row.dataset.candidate) === Number(choice));
        row.classList.toggle("nearest", Number(row.dataset.candidate) === Number(nearest));
      });
    });

  }

  function clearCurseAnswer() {
    $(".candidate-list")?.classList.remove("answered");
    $$(".candidate-row").forEach((row) => row.classList.remove("selected", "nearest"));
  }

  const gameTimers = (() => {
    const games = ["mean", "kmeans", "pca", "regression", "shadows", "curse"];
    const clocks = Object.fromEntries(games.map((game) => [game, { start: null, stoppedAt: null }]));
    const tick = () => {
      games.forEach((game) => {
        const node = $(`#${game}_timer`);
        const seconds = elapsed(game);
        if (node) node.textContent = seconds.toFixed(1);
        const lossNode = $(`#${game}_time_loss`);
        if (lossNode) {
          const base = Number(lossNode.dataset.basePoints || 0);
          const exactFinal = lossNode.dataset.finalPoints;
          const final = exactFinal === undefined
            ? Math.round(base * (0.9 + 0.1 * Math.exp(-seconds / 45)))
            : Number(exactFinal);
          lossNode.textContent = `−${Math.max(0, base - final)} puntos por tiempo${exactFinal === undefined ? " ahora" : ""}`;
        }
      });
      requestAnimationFrame(tick);
    };
    const elapsed = (game) => {
      const clock = clocks[game];
      if (!clock || clock.start === null) return 0;
      return ((clock.stoppedAt ?? performance.now()) - clock.start) / 1000;
    };
    const start = (game) => {
      const clock = clocks[game];
      if (clock && clock.start === null) { clock.start = performance.now(); clock.stoppedAt = null; }
    };
    const reset = (game) => {
      if (!clocks[game]) return;
      clocks[game].start = performance.now(); clocks[game].stoppedAt = null;
    };
    const stop = (game) => {
      const clock = clocks[game];
      if (clock && clock.start !== null && clock.stoppedAt === null) clock.stoppedAt = performance.now();
    };
    const clearAll = () => games.forEach((game) => {
      clocks[game].start = null; clocks[game].stoppedAt = null;
    });
    const has = (game) => games.includes(game);
    tick();
    return { elapsed, start, reset, stop, clearAll, has };
  })();

  const shadowRenderer = (() => {
    let canvas, ctx, width = 1, height = 1, dpr = 1;
    let yaw = -0.55, pitch = 0.38;
    let dragging = false, lastX = 0, lastY = 0;
    let targetIndex = 0, startedAt = performance.now();
    const targets = ["circle", "square", "triangle", "hexagon"];
    const targetLabels = { circle: "CÍRCULO", square: "CUADRADO", triangle: "TRIÁNGULO", hexagon: "HEXÁGONO" };
    const targetAxes = {
      circle: [0, 1, 0], square: [1, 0, 0], triangle: [0, 0, 1],
      hexagon: [1 / Math.sqrt(3), 1 / Math.sqrt(3), 1 / Math.sqrt(3)]
    };
    const targetMeshes = { circle: "triple", square: "triple", triangle: "triple", hexagon: "cube" };
    const faces = [];

    const addFace = (a, b, c, color) => faces.push({ points: [a, b, c], color });
    const buildMesh = (kind = "triple") => {
      faces.length = 0;
      if (kind === "cube") {
        const h = .75;
        const addQuad = (a, b, c, d, color) => {
          addFace(a, b, c, color); addFace(a, c, d, color);
        };
        addQuad([-h,-h,-h], [-h,-h,h], [-h,h,h], [-h,h,-h], "#705cf6");
        addQuad([h,-h,-h], [h,h,-h], [h,h,h], [h,-h,h], "#08a88a");
        addQuad([-h,-h,-h], [h,-h,-h], [h,-h,h], [-h,-h,h], "#ff6b5e");
        addQuad([-h,h,-h], [-h,h,h], [h,h,h], [h,h,-h], "#f7c948");
        addQuad([-h,-h,-h], [-h,h,-h], [h,h,-h], [h,-h,-h], "#2f80ed");
        addQuad([-h,-h,h], [h,-h,h], [h,h,h], [-h,h,h], "#a96df0");
        return;
      }
      const n = 56;
      for (let i = 0; i < n; i++) {
        const a = 2 * Math.PI * i / n, b = 2 * Math.PI * (i + 1) / n;
        const p0 = [Math.cos(a), -1, Math.sin(a)];
        const p1 = [Math.cos(b), -1, Math.sin(b)];
        const p2 = [Math.cos(b), 1 - 2 * Math.abs(Math.cos(b)), Math.sin(b)];
        const p3 = [Math.cos(a), 1 - 2 * Math.abs(Math.cos(a)), Math.sin(a)];
        addFace(p0, p1, p2, "#705cf6"); addFace(p0, p2, p3, "#705cf6");
        addFace([0, -1, 0], p1, p0, "#08a88a");
      }
      for (const sign of [-1, 1]) {
        for (let i = 0; i < n / 2; i++) {
          const y0 = -1 + 2 * i / (n / 2), y1 = -1 + 2 * (i + 1) / (n / 2);
          const x0 = sign * (1 - y0) / 2, x1 = sign * (1 - y1) / 2;
          const z0 = Math.sqrt(Math.max(0, 1 - x0 * x0));
          const z1 = Math.sqrt(Math.max(0, 1 - x1 * x1));
          const p0 = [x0, y0, -z0], p1 = [x0, y0, z0], p2 = [x1, y1, z1], p3 = [x1, y1, -z1];
          addFace(p0, p1, p2, sign < 0 ? "#ff6b5e" : "#f7c948");
          addFace(p0, p2, p3, sign < 0 ? "#ff6b5e" : "#f7c948");
        }
      }
    };

    function rotate([x, y, z]) {
      const cp = Math.cos(pitch), sp = Math.sin(pitch);
      const cy = Math.cos(yaw), sy = Math.sin(yaw);
      const y1 = y * cp - z * sp, z1 = y * sp + z * cp;
      return [x * cy + z1 * sy, y1, -x * sy + z1 * cy];
    }

    function normal(points) {
      const a = points[0], b = points[1], c = points[2];
      const u = b.map((v, i) => v - a[i]), v = c.map((q, i) => q - a[i]);
      return [u[1]*v[2]-u[2]*v[1], u[2]*v[0]-u[0]*v[2], u[0]*v[1]-u[1]*v[0]];
    }

    function hexToRgb(hex) {
      const n = parseInt(hex.slice(1), 16);
      return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
    }

    function targetOutline(cx, cy, radius, target) {
      ctx.save();
      ctx.strokeStyle = "rgba(255,255,255,.72)"; ctx.lineWidth = 2; ctx.setLineDash([6, 6]);
      ctx.beginPath();
      if (target === "circle") ctx.arc(cx, cy, radius, 0, Math.PI * 2);
      if (target === "square") ctx.rect(cx - radius, cy - radius, radius * 2, radius * 2);
      if (target === "triangle") { ctx.moveTo(cx, cy - radius); ctx.lineTo(cx + radius, cy + radius); ctx.lineTo(cx - radius, cy + radius); ctx.closePath(); }
      if (target === "hexagon") {
        for (let i = 0; i < 6; i++) {
          const angle = i * Math.PI / 3;
          const x = cx + 1.22 * radius * Math.cos(angle), y = cy + 1.22 * radius * Math.sin(angle);
          i ? ctx.lineTo(x, y) : ctx.moveTo(x, y);
        }
        ctx.closePath();
      }
      ctx.stroke(); ctx.restore();
    }

    function render() {
      if (!ctx) return;
      ctx.clearRect(0, 0, width, height);
      const objectCenter = [width * (width < 700 ? .5 : .34), height * (width < 700 ? .34 : .5)];
      const shadowCenter = [width * (width < 700 ? .5 : .79), height * (width < 700 ? .79 : .5)];
      const objectScale = Math.min(width * (width < 700 ? .25 : .22), height * (width < 700 ? .24 : .33));
      const shadowScale = Math.min(width * .105, height * .18);
      const transformed = faces.map((face) => {
        const pts = face.points.map(rotate);
        return { ...face, pts, depth: pts.reduce((s,p) => s + p[2], 0) / 3 };
      }).sort((a,b) => a.depth - b.depth);

      ctx.save();
      ctx.strokeStyle = "rgba(255,255,255,.08)"; ctx.lineWidth = 1;
      for (let r = 1; r <= 3; r++) { ctx.beginPath(); ctx.arc(objectCenter[0], objectCenter[1], objectScale * r / 3, 0, 7); ctx.stroke(); }
      ctx.restore();

      transformed.forEach((face) => {
        const n = normal(face.pts), length = Math.hypot(...n) || 1;
        const light = .38 + .62 * Math.abs(n[2] / length);
        const [r,g,b] = hexToRgb(face.color);
        ctx.beginPath();
        face.pts.forEach((p, i) => {
          const perspective = 3.8 / (4.3 - p[2]);
          const x = objectCenter[0] + p[0] * objectScale * perspective;
          const y = objectCenter[1] - p[1] * objectScale * perspective;
          i ? ctx.lineTo(x,y) : ctx.moveTo(x,y);
        });
        ctx.closePath(); ctx.fillStyle = `rgba(${r*light},${g*light},${b*light},.88)`; ctx.fill();
        ctx.strokeStyle = "rgba(255,255,255,.10)"; ctx.lineWidth = .65; ctx.stroke();
      });

      ctx.save();
      ctx.beginPath(); ctx.arc(shadowCenter[0], shadowCenter[1], shadowScale * 1.5, 0, 7);
      ctx.fillStyle = "rgba(255,255,255,.055)"; ctx.fill();
      transformed.forEach((face) => {
        ctx.beginPath();
        face.pts.forEach((p, i) => {
          const x = shadowCenter[0] + p[0] * shadowScale;
          const y = shadowCenter[1] - p[1] * shadowScale;
          i ? ctx.lineTo(x,y) : ctx.moveTo(x,y);
        });
        ctx.closePath(); ctx.fillStyle = "rgba(8,168,138,.22)"; ctx.fill();
      });
      targetOutline(shadowCenter[0], shadowCenter[1], shadowScale, targets[targetIndex]);
      ctx.fillStyle = "rgba(255,255,255,.58)"; ctx.font = "700 10px system-ui"; ctx.textAlign = "center";
      ctx.fillText("SOMBRA / OBJETIVO", shadowCenter[0], shadowCenter[1] + shadowScale * 1.85);
      ctx.restore();
    }

    function resize() {
      canvas = canvas || $("#shadow_canvas");
      if (!canvas) return;
      const rect = canvas.getBoundingClientRect();
      if (!rect.width || !rect.height) return;
      dpr = Math.min(window.devicePixelRatio || 1, 2);
      width = rect.width; height = rect.height;
      canvas.width = Math.round(width * dpr); canvas.height = Math.round(height * dpr);
      ctx = canvas.getContext("2d"); ctx.setTransform(dpr, 0, 0, dpr, 0, 0); render();
    }

    function reset(newTarget = false, notify = true) {
      yaw = -0.55; pitch = .38; startedAt = performance.now();
      if (newTarget) targetIndex = (targetIndex + 1) % targets.length;
      const label = $("#shadow_target"); if (label) label.textContent = targetLabels[targets[targetIndex]];
      const save = $("#shadow_save"); if (save) { save.disabled = true; save.classList.add("disabled"); }
      if (notify) window.Shiny?.setInputValue?.("shadow_reset", Date.now(), { priority: "event" });
      render();
    }

    function setTarget(target) {
      const index = targets.indexOf(target);
      if (index >= 0) targetIndex = index;
      buildMesh(targetMeshes[targets[targetIndex]]);
      reset(false, false);
    }

    function evaluate() {
      const axis = rotate(targetAxes[targets[targetIndex]]);
      const angle = Math.acos(Math.min(1, Math.abs(axis[2]))) * 180 / Math.PI;
      const seconds = (performance.now() - startedAt) / 1000;
      window.Shiny?.setInputValue?.("shadow_attempt", { target: targets[targetIndex], angle, seconds, nonce: Date.now() }, { priority: "event" });
    }

    function init() {
      buildMesh(targetMeshes[targets[targetIndex]]); canvas = $("#shadow_canvas"); if (!canvas) return;
      resize();
      canvas.addEventListener("pointerdown", (e) => { dragging = true; lastX = e.clientX; lastY = e.clientY; canvas.setPointerCapture(e.pointerId); });
      canvas.addEventListener("pointermove", (e) => {
        if (!dragging) return;
        yaw += (e.clientX - lastX) * .011; pitch += (e.clientY - lastY) * .011;
        pitch = Math.max(-Math.PI, Math.min(Math.PI, pitch)); lastX = e.clientX; lastY = e.clientY; render();
      });
      canvas.addEventListener("pointerup", () => { dragging = false; });
      canvas.addEventListener("dblclick", () => reset(false));
      $("#shadow_submit")?.addEventListener("click", evaluate);
      $("#shadow_restart")?.addEventListener("click", () => reset(false));
      window.addEventListener("resize", resize);
    }

    return { init, resize, reset, setTarget };
  })();

  function boot() {
    ["mean_submit", "kmeans_submit", "pca_submit", "regression_submit", "shadow_save", "curse_submit"].forEach((id) => {
      const button = document.getElementById(id);
      if (button) { button.disabled = true; button.classList.add("disabled"); }
    });
    connectShiny();
    shadowRenderer.init();
    const initial = location.hash.slice(1);
    if (["mean","kmeans","pca","regression","shadows","curse"].includes(initial)) showView(initial);
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot, { once: true });
  else boot();
})();
