export default ({ anyCmd }) => ({
  extraRules: [
    {
      label: "kubectl delete",
      action: "prompt",
      reason: "Confirm before deleting Kubernetes resources.",
      test: (pipeline) =>
        anyCmd(pipeline, "kubectl", (args) => args[0] === "delete"),
    },
  ],
});
