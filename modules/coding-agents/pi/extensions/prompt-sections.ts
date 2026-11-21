/**
 * Prompt Sections — inject markdown files as system-prompt sections.
 *
 * Every `<dir>/<name>.md` becomes one prompt section: Pi wraps the body in
 * `<name>` tags and diffs it against what the model already has, so toggling
 * one section patches a system message instead of invalidating the whole
 * prompt. This is the mechanism ponytail uses for its ruleset, generalised —
 * adding a prompt means adding a file, not writing code.
 *
 * A plain file is injected from session start. Frontmatter `on: false` ships
 * it switched off, so opt-in personas (comment-sicko style) cost nothing
 * until asked for. `/prompts` lists, `/prompts <name>` toggles, and the
 * choice is recorded in the session so /fork and /resume keep it.
 *
 * Dir: $PI_PROMPT_SECTIONS_DIR, else $PI_CODING_AGENT_DIR/prompts.
 */

import { existsSync, readdirSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { basename, join } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export type Section = { name: string; on: boolean; body: string };

// Pi renders a section key as an XML tag, so anything tag-hostile is dropped
// from the filename; the body keeps whatever markdown it has.
export function parseSection(text: string, file: string): Section {
  const frontmatter = /^---\r?\n([\s\S]*?)\r?\n---\r?\n?/.exec(text);
  let on = true;
  if (frontmatter) {
    const declared = /^\s*on\s*:\s*(\S+)/m.exec(frontmatter[1]);
    if (declared) on = /^(true|yes|1)$/.test(declared[1].toLowerCase());
  }
  const name = basename(file, ".md")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
  return { name, on, body: (frontmatter ? text.slice(frontmatter[0].length) : text).trim() };
}

export function readSections(dir: string): Section[] {
  if (!existsSync(dir)) return [];
  return readdirSync(dir)
    .filter((file) => file.endsWith(".md"))
    .sort()
    .map((file) => parseSection(readFileSync(join(dir, file), "utf8"), file))
    .filter((section) => section.name && section.body);
}

// Toggles live in the session log, replayed back to the latest value per
// section, the way ponytail restores its level. Sections no longer on disk
// keep no state.
export function resolveStates(
  entries: unknown,
  defaults: Record<string, boolean>,
): Record<string, boolean> {
  const states = { ...defaults };
  if (!Array.isArray(entries)) return states;

  const seen = new Set<string>();
  for (let i = entries.length - 1; i >= 0; i -= 1) {
    const entry = entries[i] as { type?: string; customType?: string; data?: { name?: unknown; on?: unknown } };
    if (entry?.type !== "custom" || entry?.customType !== "prompt-section") continue;

    const name = entry.data?.name;
    if (typeof name !== "string" || seen.has(name) || !(name in states)) continue;
    seen.add(name);
    states[name] = Boolean(entry.data?.on);
  }
  return states;
}

export default function promptSections(pi: ExtensionAPI) {
  const dir =
    process.env.PI_PROMPT_SECTIONS_DIR ||
    join(process.env.PI_CODING_AGENT_DIR || join(homedir(), ".pi", "agent"), "prompts");

  let sections: Section[] = [];
  let states: Record<string, boolean> = {};
  let lastCtx: any = null;

  function syncStatus(ctx?: any) {
    if (ctx) lastCtx = ctx;
    const c = ctx || lastCtx;
    if (!c?.ui?.setStatus) return;
    // pi-web hands back a theme proxy that throws before initTheme (#ponytail #953).
    let theme;
    try {
      theme = c.ui.theme;
      if (!theme?.fg) return;
    } catch {
      return;
    }
    const active = sections.filter((s) => states[s.name]).map((s) => s.name);
    if (!active.length) {
      c.ui.setStatus("prompt-sections", undefined);
      return;
    }
    c.ui.setStatus("prompt-sections", theme.fg("dim", "🧩 ") + theme.fg("text", active.join(" ")));
  }

  function load(ctx?: any) {
    sections = readSections(dir);
    const entries = ctx?.sessionManager?.getBranch?.() || ctx?.sessionManager?.getEntries?.() || [];
    states = resolveStates(entries, Object.fromEntries(sections.map((s) => [s.name, s.on])));
    syncStatus(ctx);
  }

  pi.on("session_start", async (_event, ctx) => load(ctx));
  for (const eventName of ["session_tree", "session_switch", "session_branch"]) {
    pi.on(eventName as never, async (_event, ctx) => load(ctx));
  }

  pi.on("before_agent_start", (event) => {
    const target = event?.systemPromptOptions?.sections;
    if (!target || typeof target !== "object") return;
    for (const section of sections) {
      if (states[section.name]) target[section.name] = section.body;
      else delete target[section.name];
    }
  });

  pi.registerCommand("prompts", {
    description: "List or toggle injected prompt sections",
    getArgumentCompletions: (prefix: string) => {
      const typed = String(prefix || "").trim().toLowerCase();
      const names = sections.map((s) => s.name).filter((n) => n.startsWith(typed));
      return names.length ? names.map((name) => ({ value: name, label: name })) : null;
    },
    handler: async (args: string, ctx: any) => {
      const name = String(args || "").trim().toLowerCase();

      if (!name) {
        const summary = sections.map((s) => `${s.on ? "○" : "·"} ${s.name}`).join("\n");
        ctx?.ui?.notify?.(summary ? `Prompt sections (${dir}):\n${summary}` : `No prompt sections in ${dir}.`, "info");
        return;
      }

      if (!(name in states)) {
        ctx?.ui?.notify?.(`No prompt section named ${name}.`, "warning");
        return;
      }

      states[name] = !states[name];
      pi.appendEntry("prompt-section", { name, on: states[name] });
      syncStatus(ctx);
      ctx?.ui?.notify?.(`Prompt section ${name}: ${states[name] ? "on" : "off"}.`, "info");
    },
  });
}
