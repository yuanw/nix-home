# DGX Spark / TensorFold OpenAI-compatible endpoint, declared as a
# programs.pi.providers entry (freeform fields reach models.json untouched).
# Serves Inkling 2 Mini (modules/inkling2.nix; the retired Qwen3.8-Flash-Next
# entry was removed with its service). The engine reads reasoning effort from
# chat_template_kwargs.reasoning_effort, so thinkingFormat is "chat-template"
# with that key bound to pi's mapped effort, not "qwen-chat-template".
{
  api = "openai-completions";
  apiKey = "not-needed";
  baseUrl = "http://dgx-spark.local:8888/v1";
  models = [
    {
      _launch = true;
      contextWindow = 262144;
      # Must match the served name exactly (TensorFold SERVED_NAME).
      id = "Inkling-2-Mini";
      input = [
        "text"
        "image"
      ];
      maxTokens = 32768;
      name = "Inkling 2 Mini (DGX Spark, TensorFold)";
      reasoning = true;
      # The server's effort names; "none" still reasons a little (model card).
      thinkingLevelMap = {
        off = "none";
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
    thinkingFormat = "chat-template";
    chatTemplateKwargs = {
      # pi substitutes $var objects only; the "$thinking.effort" string form
      # is passed through verbatim (pi 1.6.0), which TensorFold rejects.
      reasoning_effort = {
        "$var" = "thinking.effort";
      };
    };
    # The engine parses replies into reasoning_content/text/tool calls; replay
    # assistant messages with the reasoning field present.
    requiresReasoningContentOnAssistantMessages = true;
  };
}
