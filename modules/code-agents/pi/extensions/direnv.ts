/**
 * Direnv Extension
 *
 * Loads direnv environment variables on session start and after each bash
 * command. This mimics how the shell hook works — it runs after every command
 * to pick up any .envrc changes from cd, git checkout, etc.
 *
 * Requirements:
 *   - direnv installed and in PATH
 *   - .envrc must be allowed (run `direnv allow` in your shell first)
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { createBashTool } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	const cwd = process.cwd();

	const bashTool = createBashTool(cwd, {
		spawnHook: ({ command, cwd, env }) => (
			{
				// discard stderr to save tokens
				command: `eval "$(direnv export bash 2>/dev/null)"\n${command}`,
				cwd,
				env,
			}
		),
	});

	pi.registerTool({
		...bashTool,
		execute: async (id, params, signal, onUpdate, _ctx) => {
			return bashTool.execute(id, params, signal, onUpdate);
		},
	});
}
