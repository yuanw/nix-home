import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "web_fetch",
    label: "Web Fetch",
    description:
      "Fetch a web page by URL and return readable Markdown/text. Use this for public web pages and documentation URLs.",
    parameters: Type.Object({
      url: Type.String({ description: "The http(s) URL to fetch." }),
      raw: Type.Optional(
        Type.Boolean({
          description:
            "Fetch the raw URL directly instead of using Jina Reader for clean Markdown. Default: false.",
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
      const targetUrl = params.raw ? params.url : `https://r.jina.ai/${params.url}`;
      const response = await fetch(targetUrl, {
        signal,
        headers: {
          "User-Agent": "pi-web-fetch/1.0",
          Accept: "text/markdown,text/plain,text/html;q=0.8,*/*;q=0.5",
        },
      });

      const text = await response.text();
      const truncated = text.length > maxLength;
      const body = truncated ? text.slice(0, maxLength) : text;

      return {
        content: [
          {
            type: "text",
            text:
              `URL: ${params.url}\n` +
              `Fetched: ${targetUrl}\n` +
              `Status: ${response.status} ${response.statusText}\n` +
              `Truncated: ${truncated}\n\n` +
              body,
          },
        ],
        details: {
          url: params.url,
          fetchedUrl: targetUrl,
          status: response.status,
          statusText: response.statusText,
          truncated,
          originalLength: text.length,
          returnedLength: body.length,
        },
        isError: !response.ok,
      };
    },
  });
}
