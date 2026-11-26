# Meridian-backed Antigravity (agy) endpoint for Pi: freeform fields reach
# models.json untouched. Base URL/models from the meridian docs
# (docs/antigravity.md): conservatively 128k/4k client budgets, thinking off.
{
  port ? 3457,
}:
{
  api = "anthropic-messages";
  baseUrl = "http://127.0.0.1:${toString port}";
  apiKey = "local-placeholder";
  models = [
    {
      _launch = true;
      contextWindow = 128000;
      id = "gemini-3.8-flash-low";
      input = [
        "text"
        "image"
      ];
      maxTokens = 4096;
      name = "Antigravity Gemini Flash Low";
      reasoning = false;
    }
  ];
}
