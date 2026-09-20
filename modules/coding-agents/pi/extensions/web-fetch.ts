import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

function truncate(text: string, maxLength: number) {
  const truncated = text.length > maxLength;
  return {
    text: truncated ? text.slice(0, maxLength) : text,
    truncated,
  };
}

async function fetchWithJina(url: string, raw: boolean, maxLength: number, signal: AbortSignal) {
  const targetUrl = raw ? url : `https://r.jina.ai/${url}`;
  const response = await fetch(targetUrl, {
    signal,
    headers: {
      "User-Agent": "pi-web-fetch/1.0",
      Accept: "text/markdown,text/plain,text/html;q=0.8,*/*;q=0.5",
    },
  });

  const fullText = await response.text();
  const { text, truncated } = truncate(fullText, maxLength);

  return {
    content: [
      {
        type: "text" as const,
        text:
          `Backend: ${raw ? "raw-fetch" : "jina-reader"}\n` +
          `URL: ${url}\n` +
          `Fetched: ${targetUrl}\n` +
          `Status: ${response.status} ${response.statusText}\n` +
          `Truncated: ${truncated}\n\n` +
          text,
      },
    ],
    details: {
      backend: raw ? "raw-fetch" : "jina-reader",
      url,
      fetchedUrl: targetUrl,
      status: response.status,
      statusText: response.statusText,
      truncated,
      originalLength: fullText.length,
      returnedLength: text.length,
    },
    isError: !response.ok,
  };
}

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "web_fetch",
    label: "Web Fetch",
    description:
      "Fetch a web page by URL and return readable text/Markdown. Tries browser-cli first, so JavaScript-rendered pages work and browser-cli can run headlessly via browsh when no GUI browser is connected. Falls back to Jina Reader for static fetching.",
    parameters: Type.Object({
      url: Type.String({ description: "The http(s) URL to fetch." }),
      backend: Type.Optional(
        Type.Union([
          Type.Literal("auto"),
          Type.Literal("browser"),
          Type.Literal("jina"),
          Type.Literal("raw"),
        ], {
          description:
            "Fetch backend. auto: browser-cli then Jina fallback. browser: browser-cli only. jina: Jina Reader. raw: direct fetch. Default: auto.",
        }),
      ),
      maxLength: Type.Optional(
        Type.Integer({
          minimum: 1000,
          maximum: 100000,
          description: "Maximum characters to return. Default: 30000.",
        }),
      ),
    }),
    async execute(_toolCallId, params, signal) {
      const maxLength = params.maxLength ?? 30000;
      const backend = params.backend ?? "auto";

      if (backend === "jina" || backend === "raw") {
        return fetchWithJina(params.url, backend === "raw", maxLength, signal);
      }

      const browserScript = String.raw`
set -euo pipefail
WEB_FETCH_URL="$1"
TAB="$(browser-cli --go "$WEB_FETCH_URL")"
browser-cli "$TAB" <<'EOF'
await wait("idle")
const article = read({ maxLength: __MAX_LENGTH__, includeMetadata: true })
if (article && article.content && article.content.trim()) {
  article
} else {
  snap({ full: true })
}
EOF
`.replace("__MAX_LENGTH__", String(maxLength));

      const result = await pi.exec("bash", ["-lc", browserScript, "web-fetch", params.url], {
        signal,
        timeout: 120000,
      });

      if (result.code === 0 && result.stdout.trim()) {
        const { text, truncated } = truncate(result.stdout.trim(), maxLength);
        return {
          content: [
            {
              type: "text" as const,
              text:
                `Backend: browser-cli\n` +
                `URL: ${params.url}\n` +
                `Truncated: ${truncated}\n\n` +
                text,
            },
          ],
          details: {
            backend: "browser-cli",
            url: params.url,
            truncated,
            originalLength: result.stdout.length,
            returnedLength: text.length,
            stderr: result.stderr,
          },
        };
      }

      if (backend === "browser") {
        return {
          content: [
            {
              type: "text" as const,
              text:
                `browser-cli failed for ${params.url}\n` +
                `exit code: ${result.code}\n\n` +
                `stderr:\n${result.stderr}\n\nstdout:\n${result.stdout}`,
            },
          ],
          details: {
            backend: "browser-cli",
            url: params.url,
            code: result.code,
            stderr: result.stderr,
            stdout: result.stdout,
          },
          isError: true,
        };
      }

      const fallback = await fetchWithJina(params.url, false, maxLength, signal);
      return {
        ...fallback,
        content: [
          {
            type: "text" as const,
            text:
              `browser-cli failed; fell back to Jina Reader.\n` +
              `browser-cli exit code: ${result.code}\n` +
              (result.stderr ? `browser-cli stderr:\n${result.stderr}\n\n` : "\n") +
              fallback.content[0].text,
          },
        ],
        details: {
          ...fallback.details,
          browserCliFailed: true,
          browserCliCode: result.code,
          browserCliStderr: result.stderr,
        },
      };
    },
  });
}
