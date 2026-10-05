/*
  DGX Spark / TensorFold OpenAI-compatible endpoint as a nixpi provider.

  mkPiProvider does not pass freeform fields through; merge `compat` afterward
  so models.json can keep thinkingFormat and related knobs (see modules.pi
  freeform models.json override — stock nixpi strips these).
*/
{
  inputs,
  pkgs,
}:
(inputs.nixpi.lib.nixpi.mkPiProvider {
  inherit pkgs;
  name = "dgx-spark";
  api = "openai-completions";
  apiKey = "not-needed";
  baseUrl = "http://dgx-spark.local:8888/v1";
  models = [
    {
      _launch = true;
      contextWindow = 262144;
      # Must match the served name exactly (TensorFold SERVED_NAME).
      id = "Qwen3.8-Flash-Next";
      input = [
        "text"
        "image"
      ];
      maxTokens = 32768;
      name = "Qwen3.8 Flash Next (DGX Spark, TensorFold)";
      reasoning = true;
      thinkingLevelMap = {
        off = "off";
        minimal = "minimal";
        low = "low";
        medium = "medium";
        high = "high";
        xhigh = "xhigh";
        max = "max";
      };
    }
  ];
})
// {
  enable = true;
  compat = {
    supportsDeveloperRole = false;
    supportsReasoningEffort = false;
    supportsStore = false;
    thinkingFormat = "qwen-chat-template";
    thinkingTokenBudgetField = "thinking_token_budget";
  };
}
