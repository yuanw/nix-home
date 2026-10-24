/**
 * Exit — `/exit` as an alias for the built-in `/quit`.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	pi.registerCommand("exit", {
		description: "Quit Pi",
		handler: async (_args, ctx) => {
			await ctx.shutdown();
		},
	});
}
