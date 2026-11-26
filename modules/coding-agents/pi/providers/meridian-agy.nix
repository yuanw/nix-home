# Meridian-backed Antigravity (agy) endpoint for Pi: freeform fields reach
# models.json untouched. Base URL/models from the meridian docs
# (docs/antigravity.md): conservatively 128k/4k client budgets, thinking off.
# Add another slug as a separate entry -- the account's /v1/models is the
# source of truth for which slugs exist.
{
  port ? 3457,
  ids ? [
    "gemini-3.8-flash-low"
    "gemini-3.8-flash-medium"
    "gemini-3.8-flash-high"
    "gemini-4-neon-low"
    "gemini-4-neon-medium"
    "gemini-4-neon-high"
    "gemini-4-neon-xhigh"
  ],
}:
let
  # One entry per account model slug; conservative Pi-side budgets (the native
  # per-request max_tokens is advisory and the CLI has its own context window).
  mkModel = id: {
    _launch = true;
    contextWindow = 128000;
    inherit id;
    input = [
      "text"
      "image"
    ];
    maxTokens = 4096;
    name = "Antigravity ${id}";
    reasoning = false;
  };
in
{
  api = "anthropic-messages";
  baseUrl = "http://127.0.0.1:${toString port}";
  apiKey = "local-placeholder";
  models = map mkModel ids;
}
