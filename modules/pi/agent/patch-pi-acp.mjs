#!/usr/bin/env node
// Patch pi-acp@0.0.33 (dist/index.js):
//
// Advertise pi steering / follow-up queue modes ("all", "one-at-a-time")
// as settable `steering_mode` / `follow_up_mode` config options, wired to
// pi RPC set_steering_mode / set_follow_up_mode. agent-shell's generic
// `agent-shell-set-session-config-option` picks these up with no client
// changes; pi-acp's /steering and /follow-up slash commands keep working.
//
// Implement the `_session/steering` ACP extension so agent-shell-prompt-steer
// works: advertise `steering.supported` in initialize._meta (the gate behind
// agent-shell-steering-supported-p) and forward steers into pi's own
// steering queue via pi RPC `steer`. With no turn running, answer
// `promptRequired` so the client submits normally instead.
//
// Forward pi session stats as ACP usage: `usage_update` notifications drive
// agent-shell's context indicator, and the `usage` prompt-response field
// fills its token counters. Reported when a turn settles (pi-acp's own turn
// queue means session/prompt resolves exactly then).
//
// Expose pi session forking as ACP `session/fork`: advertise the unstable
// `fork` session capability and implement `unstable_forkSession` by spawning
// a new pi RPC proc with `pi --fork <sessionFile>` (full-session clone at
// the current leaf). The source proc is left untouched so both sessions stay
// live, unlike pi's in-place `clone` which rebinds the calling proc.
//
// Accept agent-shell's `full-access` default session mode as a no-op: pi
// has no session modes (pi-acp's modes are thinking levels), so without
// this every session init fails with `Unknown modeId: full-access`.
//
// Advertise a synthetic `full-access` ("Full Access") session mode and
// report it as current, so clients show Full Access instead of a thinking
// level in the mode slot without sending session/set_mode (whose in-flight
// window drops pi-acp's startup banner below the prompt). The real thinking
// level rides along in a separate field for the thought_level option.
//
// Idempotent per feature: exits 0 without changes when a feature's marker is
// already present.
// Verifies pi-acp version unless --force. Usage:
//   node patch-pi-acp.mjs [path/to/pi-acp/dist/index.js]

import { execSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join, resolve } from "node:path";

const EXPECTED_VERSION = "0.0.33";

function defaultDistPath() {
  let root;
  try {
    root = execSync("npm root -g", { encoding: "utf8" }).trim();
  } catch (err) {
    console.error(`failed to locate global npm root: ${err.message}`);
    process.exit(1);
  }
  return join(root, "pi-acp", "dist", "index.js");
}

const args = process.argv.slice(2);
const force = args.includes("--force");
const positional = args.filter((a) => a !== "--force");

const distPath = resolve(positional[0] ?? defaultDistPath());
if (!existsSync(distPath)) {
  console.error(`pi-acp dist not found: ${distPath}`);
  process.exit(1);
}
const pkgPath = join(dirname(distPath), "..", "package.json");
const version = JSON.parse(readFileSync(pkgPath, "utf8")).version;
if (version !== EXPECTED_VERSION && !force) {
  console.error(`expected pi-acp@${EXPECTED_VERSION}, found ${version}; aborting (use --force)`);
  process.exit(1);
}

let src = readFileSync(distPath, "utf8");

const steeringEdits = [
  // 1. Queue-mode values (match pi-acp's own /steering + /follow-up validation).
  [`var THOUGHT_LEVEL_CONFIG_ID = "thought_level";`,
   `var THOUGHT_LEVEL_CONFIG_ID = "thought_level";
var STEERING_MODE_CONFIG_ID = "steering_mode";
var FOLLOW_UP_MODE_CONFIG_ID = "follow_up_mode";
var QUEUE_MODES = ["all", "one-at-a-time"];
function isQueueMode(x) {
  return x === "all" || x === "one-at-a-time";
}`],

  // 2. Read live modes from pi state (same getState pattern as getThinkingState).
  [`async function getSessionConfiguration(proc, pre) {`,
   `async function getSteeringState(proc, pre) {
  const state = pre?.state ?? await (async () => {
    try {
      return await proc.getState();
    } catch {
      return null;
    }
  })();
  const mode = state?.steeringMode;
  const followUp = state?.followUpMode;
  return {
    mode: isQueueMode(mode) ? mode : "one-at-a-time",
    followUp: isQueueMode(followUp) ? followUp : "one-at-a-time"
  };
}
async function getSessionConfiguration(proc, pre) {`],

  // 3. Fetch alongside models/thinking (reuses pre.state on session/new,
  // so no extra RPC there).
  [`  const [models, modes] = await Promise.all([getModelState(proc, pre), getThinkingState(proc, { state: pre?.state })]);`,
   `  const [models, modes, steering] = await Promise.all([getModelState(proc, pre), getThinkingState(proc, { state: pre?.state }), getSteeringState(proc, pre)]);`],

  // 4. Pass into config options.
  [`    configOptions: buildConfigOptions({ models, modes }),`,
   `    configOptions: buildConfigOptions({ models, modes, steering }),`],

  // 5. Advertise both as settable selects (new categories; existing
  // model / thought-level / mode menus unaffected).
  [`    });
  }
  return configOptions;
}`,
   `    });
  }
  if (state.steering) {
    configOptions.push({
      type: "select",
      id: STEERING_MODE_CONFIG_ID,
      category: "steering_mode",
      name: "Steering",
      description: "How queued steering messages are delivered",
      currentValue: state.steering.mode,
      options: QUEUE_MODES.map((m) => ({
        value: m,
        name: m === "all" ? "All" : "One at a time",
        description: null
      }))
    });
    configOptions.push({
      type: "select",
      id: FOLLOW_UP_MODE_CONFIG_ID,
      category: "follow_up_mode",
      name: "Follow-up",
      description: "How queued follow-up messages are delivered",
      currentValue: state.steering.followUp,
      options: QUEUE_MODES.map((m) => ({
        value: m,
        name: m === "all" ? "All" : "One at a time",
        description: null
      }))
    });
  }
  return configOptions;
}`],

  // 6. Wire writes to pi RPC (same validation as the /steering + /follow-up
  // slash commands).
  [`    } else {
      throw RequestError3.invalidParams(\`Unknown config option: \${configId}\`);
    }`,
   `    } else if (configId === STEERING_MODE_CONFIG_ID || configId === FOLLOW_UP_MODE_CONFIG_ID) {
      const value = String(params.value ?? "").toLowerCase();
      if (!isQueueMode(value)) {
        throw RequestError3.invalidParams(\`Unknown queue mode: \${params.value} (expected all or one-at-a-time)\`);
      }
      if (configId === STEERING_MODE_CONFIG_ID) {
        await session.proc.setSteeringMode(value);
      } else {
        await session.proc.setFollowUpMode(value);
      }
    } else {
      throw RequestError3.invalidParams(\`Unknown config option: \${configId}\`);
    }`],
];

function applyEdits(edits) {
  edits.forEach(([oldText, newText], i) => {
    const n = src.split(oldText).length - 1;
    if (n !== 1) {
      console.error(`edit ${i + 1}: found ${n} occurrence(s), expected 1; aborting`);
      process.exit(1);
    }
    src = src.split(oldText).join(newText);
  });
}

const fullAccessEdits = [
  // pi has no session modes; agent-shell sends its default-session-mode-id
  // on every session init. Accept `full-access` and ignore it so init
  // succeeds; anything else still falls through to thinking-level validation.
  [`    const mode = String(params.modeId);
    if (!isThinkingLevel(mode)) {`,
   `    const mode = String(params.modeId);
    if (mode === "full-access") {
      // ponytail: no-op, pi has no session modes to switch to.
      return {};
    }
    if (!isThinkingLevel(mode)) {`],
];

let changed = false;

if (src.includes("STEERING_MODE_CONFIG_ID")) {
  console.log("steering: already patched; skipping");
} else {
  if (src.includes("PERMISSION_MODE_CONFIG_ID")) {
    console.error("dist contains stale permission_mode patches; reinstall pi-acp@0.0.33 first (npm install -g pi-acp@0.0.33) then re-run");
    process.exit(1);
  }
  applyEdits(steeringEdits);
  changed = true;
  console.log("steering: patched");
}

const fullAccessModeEdits = [
  // Report Full Access as the current session mode. thinkingLevel preserves
  // the real thinking level for the thought_level config option below.
  [`  const available = ["off", "minimal", "low", "medium", "high", "xhigh"];
  return {
    currentModeId: current,
    availableModes: available.map((id) => ({
      id,
      name: \`Thinking: \${id}\`,
      description: null
    }))
  };`,
   `  const available = ["off", "minimal", "low", "medium", "high", "xhigh"];
  return {
    currentModeId: "full-access",
    thinkingLevel: current,
    availableModes: [{ id: "full-access", name: "Full Access", description: null }].concat(available.map((id) => ({
      id,
      name: \`Thinking: \${id}\`,
      description: null
    })))
  };`],

  // The thought_level option keeps tracking the real thinking level.
  [`      currentValue: state.modes.currentModeId,`,
   `      currentValue: state.modes.thinkingLevel ?? state.modes.currentModeId,`],
];

if (src.includes('mode === "full-access"')) {
  console.log("full-access: already patched; skipping");
} else {
  applyEdits(fullAccessEdits);
  changed = true;
  console.log("full-access: patched");
}

const steeringIntoTurnEdits = [
  // agent-shell-steering-supported-p reads initialize._meta.steering.supported.
  [`      protocolVersion: requested === supportedVersion ? requested : supportedVersion,`,
   `      protocolVersion: requested === supportedVersion ? requested : supportedVersion,
      _meta: { steering: { supported: true } },`],

  // pi RPC already speaks `steer` (mid-run steering message); pi-acp's
  // client wrapper just never exposed it.
  [`  async setSteeringMode(mode) {
    const res = await this.request({ type: "set_steering_mode", mode });
    if (!res.success) throw new Error(\`pi set_steering_mode failed: \${res.error ?? JSON.stringify(res.data)}\`);
  }`,
   `  async setSteeringMode(mode) {
    const res = await this.request({ type: "set_steering_mode", mode });
    if (!res.success) throw new Error(\`pi set_steering_mode failed: \${res.error ?? JSON.stringify(res.data)}\`);
  }
  async steer(message, images = []) {
    const res = await this.request({ type: "steer", message, images });
    if (!res.success) throw new Error(\`pi steer failed: \${res.error ?? JSON.stringify(res.data)}\`);
  }`],

  // The SDK routes unknown methods to extMethod; answer the steering
  // extension here. promptToPiMessage reuses the session/prompt conversion.
  [`  async setSessionMode(params) {`,
   `  async extMethod(method, params) {
    if (method !== "_session/steering") {
      throw RequestError3.methodNotFound(method);
    }
    const session = await this.restoreSession(params.sessionId);
    if (!session.pendingTurn) {
      return { outcome: "promptRequired" };
    }
    const { message, images } = promptToPiMessage(params.prompt ?? []);
    try {
      await session.proc.steer(message, images);
    } catch {
      return { outcome: "failed" };
    }
    return { outcome: "injected" };
  }
  async setSessionMode(params) {`],
];

if (src.includes('name: "Full Access"')) {
  console.log("full-access-mode: already patched; skipping");
} else {
  applyEdits(fullAccessModeEdits);
  changed = true;
  console.log("full-access-mode: patched");
}

const usageEdits = [
  // Best-effort: stats must never fail the turn. Shapes match what
  // agent-shell reads (usage_update used/size/cost, response usage
  // totalTokens/inputTokens/outputTokens/cachedReadTokens/cachedWriteTokens).
  [`  async cancel() {`,
   `  async reportUsage() {
    let stats = null;
    try {
      stats = await this.proc.getSessionStats();
    } catch {
      return {};
    }
    const num = (x) => typeof x === "number" ? x : null;
    const t = stats?.tokens;
    const cu = stats?.contextUsage;
    const used = num(cu?.tokens);
    const size = num(cu?.contextWindow);
    const update = { sessionUpdate: "usage_update" };
    if (used !== null) update.used = used;
    if (size !== null && size > 0) update.size = size;
    if (num(stats?.cost) !== null) update.cost = { amount: stats.cost };
    if (used !== null || size !== null || update.cost !== undefined) {
      this.emit(update);
    }
    const usage = {};
    if (num(t?.total) !== null) usage.totalTokens = t.total;
    if (num(t?.input) !== null) usage.inputTokens = t.input;
    if (num(t?.output) !== null) usage.outputTokens = t.output;
    if (num(t?.cacheRead) !== null) usage.cachedReadTokens = t.cacheRead;
    if (num(t?.cacheWrite) !== null) usage.cachedWriteTokens = t.cacheWrite;
    return Object.keys(usage).length ? { usage } : {};
  }
  async cancel() {`],

  // session/prompt resolves when the turn settles, so one hook covers both
  // the notification and the response field.
  [`    const result = await session.prompt(message, images);
    const stopReason = result === "error" ? session.wasCancelRequested() ? "cancelled" : "end_turn" : result;
    return { stopReason };`,
   `    const result = await session.prompt(message, images);
    const stopReason = result === "error" ? session.wasCancelRequested() ? "cancelled" : "end_turn" : result;
    return { stopReason, ...await session.reportUsage() };`],
];

const forkEdits = [
  // PiRpcProcess.spawn only knows --session; teach it --fork so a forked
  // session gets its own proc without disturbing the source session.
  // (pi RPC `clone` would rebind the source proc to the fork instead.)
  [`    const args = ["--mode", "rpc", "--no-themes"];
    if (params.sessionPath) args.push("--session", params.sessionPath);`,
   `    const args = ["--mode", "rpc", "--no-themes"];
    if (params.sessionPath) args.push("--session", params.sessionPath);
    if (params.forkPath) args.push("--fork", params.forkPath);`],

  // Advertise the unstable session/fork capability so clients offer fork.
  [`          list: {},
          delete: {}`,
   `          list: {},
          delete: {},
          // UNSTABLE: session/fork -> unstable_forkSession below.
          fork: {}`],

  // Fork = full-session clone at the current leaf (ACP has no message
  // picker, so pi's entryId fork does not map). Keeps the source proc alive;
  // both sessions stay usable (new/load close other procs, fork does not).
  [`  async unstable_setSessionModel(params) {`,
   `  async unstable_forkSession(params) {
    if (!isAbsolute3(params.cwd)) {
      throw RequestError3.invalidParams(\`cwd must be an absolute path: \${params.cwd}\`);
    }
    const stored = this.findStoredSession(params.sessionId);
    // The file only exists after the first persisted entry; pi refuses to
    // fork an empty session, so fail fast with a retriable error instead.
    let forkSource = stored?.sessionFile ?? null;
    try {
      if (!forkSource || statSync2(forkSource).size === 0) forkSource = null;
    } catch {
      forkSource = null;
    }
    if (!forkSource) {
      throw RequestError3.invalidParams(\`Cannot fork session \${params.sessionId}: no saved history yet (wait for the first assistant response, then retry)\`);
    }
    this.lastSessionCwd = params.cwd;
    const fileCommands = loadSlashCommands(params.cwd);
    const enableSkillCommands = getEnableSkillCommands(params.cwd);
    let proc;
    try {
      proc = await PiRpcProcess.spawn({
        cwd: params.cwd,
        forkPath: forkSource,
        piCommand: process.env.PI_ACP_PI_COMMAND
      });
    } catch (e) {
      if (e?.name === "PiRpcSpawnError") {
        throw RequestError3.internalError({ code: e?.code }, String(e?.message ?? e));
      }
      throw e;
    }
    let state = null;
    try {
      state = await proc.getState();
    } catch {
      state = null;
    }
    const sessionId = typeof state?.sessionId === "string" ? state.sessionId : crypto.randomUUID();
    const sessionFile = typeof state?.sessionFile === "string" ? state.sessionFile : null;
    if (sessionFile) {
      this.store.upsert({ sessionId, cwd: params.cwd, sessionFile });
    }
    const session = this.sessions.getOrCreate(sessionId, {
      cwd: params.cwd,
      mcpServers: params.mcpServers,
      conn: this.conn,
      proc,
      fileCommands
    });
    const { configOptions, modes } = await getSessionConfiguration(proc, { state });
    setTimeout(() => {
      void (async () => {
        try {
          const pi = await proc.getCommands();
          const { commands } = toAvailableCommandsFromPiGetCommands(pi, {
            enableSkillCommands,
            includeExtensionCommands: false
          });
          await this.conn.sessionUpdate({
            sessionId: session.sessionId,
            update: {
              sessionUpdate: "available_commands_update",
              availableCommands: mergeCommands(commands, builtinAvailableCommands())
            }
          });
          return;
        } catch {
        }
        await this.conn.sessionUpdate({
          sessionId: session.sessionId,
          update: {
            sessionUpdate: "available_commands_update",
            availableCommands: mergeCommands(toAvailableCommands(fileCommands), builtinAvailableCommands())
          }
        });
      })();
    }, 0);
    return { sessionId: session.sessionId, modes, configOptions };
  }
  async unstable_setSessionModel(params) {`],
];

if (src.includes("_session/steering")) {
  console.log("steer-into-turn: already patched; skipping");
} else {
  applyEdits(steeringIntoTurnEdits);
  changed = true;
  console.log("steer-into-turn: patched");
}

if (src.includes("reportUsage")) {
  console.log("usage: already patched; skipping");
} else {
  applyEdits(usageEdits);
  changed = true;
  console.log("usage: patched");
}

if (src.includes("unstable_forkSession")) {
  console.log("fork: already patched; skipping");
} else {
  applyEdits(forkEdits);
  changed = true;
  console.log("fork: patched");
}

if (!changed) {
  console.log("self-check ok (no changes)");
  process.exit(0);
}

// The bundled source map no longer matches the patched file; drop the
// reference so debuggers do not load stale mappings.
src = src.replace(/\n\/\/# sourceMappingURL=.*$/m, "");

writeFileSync(distPath, src);

// Self-checks.
const check = readFileSync(distPath, "utf8");
const fail = (msg) => {
  console.error(`self-check failed: ${msg}`);
  process.exit(1);
};
if (!check.includes("STEERING_MODE_CONFIG_ID")) fail("steering marker missing");
if (!check.includes("getSteeringState")) fail("steering helper missing");
if (!check.includes('mode === "full-access"')) fail("full-access marker missing");
if (!check.includes('name: "Full Access"')) fail("full-access-mode marker missing");
if (!check.includes("thinkingLevel: current")) fail("thinking-level field missing");
if (!check.includes("_session/steering")) fail("steering handler missing");
if (!check.includes("steering: { supported: true }")) fail("steering advertisement missing");
if (!check.includes("reportUsage")) fail("usage helper missing");
if (!check.includes('sessionUpdate: "usage_update"')) fail("usage notification missing");
if (!check.includes("unstable_forkSession")) fail("fork handler missing");
if (!check.includes("forkPath")) fail("fork spawn arg missing");
if (!check.includes("fork: {}")) fail("fork capability missing");
if (check.includes("PERMISSION_MODE_CONFIG_ID")) fail("stale permission marker present");
console.log("self-check ok");
