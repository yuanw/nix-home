import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import promptSections, { parseSection, readSections, resolveStates } from "../extensions/prompt-sections.ts";

function harness(dir: string, entries: unknown[] = []) {
  const handlers: Record<string, any[]> = {};
  const commands: Record<string, any> = {};
  const pi = {
    on: (name: string, fn: unknown) => ((handlers[name] ??= []).push(fn)),
    registerCommand: (name: string, def: unknown) => (commands[name] = def),
    appendEntry: (customType: string, data: unknown) => entries.push({ type: "custom", customType, data }),
  };
  const ctx = { sessionManager: { getBranch: () => entries }, ui: { notify() {}, setStatus() {} } };
  process.env.PI_PROMPT_SECTIONS_DIR = dir;
  promptSections(pi as never);

  return {
    commands,
    start: () => handlers.session_start[0](undefined, ctx),
    prompt: async () => {
      const event = { systemPromptOptions: { sections: {} as Record<string, string> } };
      await handlers.before_agent_start[0](event);
      return event.systemPromptOptions.sections;
    },
  };
}

test("parseSection keeps the body and defaults to on", () => {
  const s = parseSection("# Rules\n\nKill comments.\n", "comment-sicko.md");
  assert.deepEqual(s, { name: "comment-sicko", on: true, body: "# Rules\n\nKill comments." });
});

test("parseSection reads the on flag and drops frontmatter", () => {
  const s = parseSection("---\nname: sicko\ndescription: hater\non: false\n---\n\nBody\n", "sicko.md");
  assert.equal(s.on, false);
  assert.equal(s.body, "Body");
});

test("parseSection normalises the tag to filename-safe text", () => {
  assert.equal(parseSection("b", "No_Peak  Rules.md").name, "no-peak-rules");
});

test("readSections skips empty and non-markdown files", () => {
  const dir = mkdtempSync(join(tmpdir(), "prompt-sections-"));
  writeFileSync(join(dir, "b.md"), "second");
  writeFileSync(join(dir, "a.md"), "---\non: no\n---\nfirst");
  writeFileSync(join(dir, "empty.md"), "   ");
  writeFileSync(join(dir, "notes.txt"), "ignored");
  assert.deepEqual(readSections(dir), [
    { name: "a", on: false, body: "first" },
    { name: "b", on: true, body: "second" },
  ]);
  assert.deepEqual(readSections(join(dir, "missing")), []);
});

test("resolveStates keeps the latest toggle per section and ignores the rest", () => {
  const entry = (name, on) => ({ type: "custom", customType: "prompt-section", data: { name, on } });
  const entries = [
    entry("a", true),
    { type: "message" },
    entry("a", false),
    entry("gone", true),
    entry("b", true),
  ];
  assert.deepEqual(resolveStates(entries, { a: true, b: false }), { a: false, b: true });
  assert.deepEqual(resolveStates(undefined, { a: true }), { a: true });
});

test("sections land in systemPromptOptions and toggles survive a new session", async () => {
  const dir = mkdtempSync(join(tmpdir(), "prompt-sections-"));
  writeFileSync(join(dir, "always.md"), "# Always\n\nDo the thing.");
  writeFileSync(join(dir, "sicko.md"), "---\non: false\n---\nKill comments.");

  const entries: unknown[] = [];
  const first = harness(dir, entries);
  await first.start();
  assert.deepEqual(await first.prompt(), { always: "# Always\n\nDo the thing." });

  await first.commands.prompts.handler("sicko", { ui: { notify() {}, setStatus() {} } });
  assert.deepEqual(await first.prompt(), {
    always: "# Always\n\nDo the thing.",
    sicko: "Kill comments.",
  });

  // Same session log, fresh extension: the toggle comes back from the entries.
  const restored = harness(dir, entries);
  await restored.start();
  assert.equal((await restored.prompt()).sicko, "Kill comments.");
});
