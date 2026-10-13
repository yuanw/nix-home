# DGX Spark / TensorFold OpenAI-compatible endpoint, declared as a
# programs.pi.providers entry (freeform fields reach models.json untouched).
{
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
      name = "Qwen3.8 Flash Next (DGX Spark, TensorFold Zig)";
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
  compat = {
    supportsDeveloperRole = false;
    supportsReasoningEffort = false;
    supportsStore = false;
    thinkingFormat = "qwen-chat-template";
    thinkingTokenBudgetField = "thinking_token_budget";
  };
}
