export default ({ anyCmd, unwrap, SHELLS }) => ({
  // Built-in "pipe to shell" only prompts; replace it with a hard block
  // that steers the agent at Nix instead of remote install scripts.
  disabledRules: ["pipe to shell"],
  extraRules: [
    {
      label: "kubectl delete",
      action: "prompt",
      reason: "Confirm before deleting Kubernetes resources.",
      test: (pipeline) =>
        anyCmd(pipeline, "kubectl", (args) => args[0] === "delete"),
    },
    {
      label: "curl | bash",
      group: "exec",
      action: "block",
      reason:
        "Fetching a remote script and piping it to a shell is blocked. " +
        "Install packages with Nix: `nix-shell -p <pkg>`, " +
        "`nix run nixpkgs#<pkg> -- …`, or add the package to this flake / " +
        "home-manager config (for bun: `nix-shell -p bun` or `nix run nixpkgs#bun`).",
      // Same shape as Mic92's "pipe to shell" rule: a curl/wget producer
      // followed by a shell or eval/source consumer, including
      // `eval "$(curl …)"` and `source <(curl …)` after pipeline synthesis.
      test: (pipeline) => {
        const fetchers = new Set(["curl", "wget"]);
        const execBuiltins = new Set(["eval", "source", "."]);
        const prod = pipeline.findIndex((argv) =>
          fetchers.has(unwrap(argv)[0]),
        );
        return (
          prod !== -1 &&
          pipeline.slice(prod + 1).some((argv) => {
            const head = unwrap(argv)[0];
            return SHELLS.has(head) || execBuiltins.has(head);
          })
        );
      },
    },
  ],
});
