import { execFile } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { useCallback, useEffect, useState } from "react";
import {
  Action,
  ActionPanel,
  Icon,
  List,
  Toast,
  getPreferenceValues,
  showHUD,
  showToast,
} from "@vicinae/api";

declare const __PASS_BIN__: string;

interface Preferences {
  passwordStoreDir?: string;
  passBinary?: string;
}

function expandHome(p: string): string {
  if (p === "~") return os.homedir();
  if (p.startsWith("~/")) return path.join(os.homedir(), p.slice(2));
  return p;
}

function resolveStoreDir(pref?: string): string {
  const configured = pref?.trim();
  if (configured) return expandHome(configured);
  const envDir = process.env.PASSWORD_STORE_DIR?.trim();
  if (envDir) return expandHome(envDir);
  return path.join(os.homedir(), ".password-store");
}

function listPassEntries(storeDir: string): string[] {
  const entries: string[] = [];
  const visited = new Set<string>();

  function walk(dir: string, prefix: string): void {
    let real: string;
    try {
      real = fs.realpathSync(dir);
    } catch {
      return;
    }
    if (visited.has(real)) return;
    visited.add(real);

    let items: fs.Dirent[];
    try {
      items = fs.readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }

    for (const item of items) {
      if (item.name.startsWith(".")) continue;
      const fullPath = path.join(dir, item.name);
      let isDir = item.isDirectory();
      let isFile = item.isFile();
      if (item.isSymbolicLink()) {
        try {
          const stat = fs.statSync(fullPath);
          isDir = stat.isDirectory();
          isFile = stat.isFile();
        } catch {
          continue;
        }
      }

      if (isDir) {
        walk(fullPath, prefix ? `${prefix}/${item.name}` : item.name);
      } else if (isFile && item.name.endsWith(".gpg")) {
        const stem = item.name.slice(0, -4);
        entries.push(prefix ? `${prefix}/${stem}` : stem);
      }
    }
  }

  walk(storeDir, "");
  return entries.sort((a, b) => a.localeCompare(b));
}

function runPassCopy(
  passBin: string,
  storeDir: string,
  entry: string,
): Promise<string> {
  const systemPaths = ["/usr/bin", "/bin", "/usr/sbin", "/sbin"];
  const currentPath = process.env.PATH ?? "";
  const mergedPath = [
    ...currentPath.split(":").filter(Boolean),
    ...systemPaths.filter((p) => !currentPath.split(":").includes(p)),
  ].join(":");

  return new Promise((resolve, reject) => {
    execFile(
      passBin,
      ["-c", entry],
      {
        env: {
          ...process.env,
          PASSWORD_STORE_DIR: storeDir,
          PATH: mergedPath,
        },
        timeout: 60_000,
      },
      (err, stdout, stderr) => {
        if (err) {
          const detail = stderr.trim() || err.message;
          reject(new Error(detail));
          return;
        }
        resolve(stdout.trim());
      },
    );
  });
}

export default function PassCopy() {
  const prefs = getPreferenceValues<Preferences>();
  const storeDir = resolveStoreDir(prefs.passwordStoreDir);
  const passBin = prefs.passBinary?.trim() || __PASS_BIN__ || "pass";

  const [entries, setEntries] = useState<string[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const loadEntries = useCallback(() => {
    setIsLoading(true);
    try {
      if (!fs.existsSync(storeDir)) {
        setEntries([]);
        setError(`Password store not found at ${storeDir}`);
      } else {
        setEntries(listPassEntries(storeDir));
        setError(null);
      }
    } catch (err) {
      setEntries([]);
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setIsLoading(false);
    }
  }, [storeDir]);

  useEffect(() => {
    loadEntries();
  }, [loadEntries]);

  const copyEntry = useCallback(
    async (entry: string) => {
      const toast = await showToast({
        style: Toast.Style.Animated,
        title: `Copying ${entry}...`,
      });
      try {
        const output = await runPassCopy(passBin, storeDir, entry);
        await toast.hide();
        await showHUD(output || `Copied ${entry} to clipboard`);
      } catch (err) {
        toast.style = Toast.Style.Failure;
        toast.title = `Failed to copy ${entry}`;
        toast.message = err instanceof Error ? err.message : String(err);
        await toast.update();
      }
    },
    [passBin, storeDir],
  );

  return (
    <List isLoading={isLoading} searchBarPlaceholder="Search pass entries...">
      {error ? (
        <List.EmptyView
          icon={Icon.LockDisabled}
          title="Unable to read password store"
          description={error}
        />
      ) : entries.length === 0 && !isLoading ? (
        <List.EmptyView
          icon={Icon.Key}
          title="No pass entries found"
          description={storeDir}
        />
      ) : (
        entries.map((entry) => (
          <List.Item
            key={entry}
            id={entry}
            title={entry}
            icon={Icon.Key}
            keywords={entry.split(/[/_.-]+/).filter(Boolean)}
            actions={
              <ActionPanel>
                <Action
                  title="Copy Password (pass -c)"
                  icon={Icon.CopyClipboard}
                  onAction={() => copyEntry(entry)}
                />
                <Action.CopyToClipboard
                  title="Copy Entry Name"
                  content={entry}
                  shortcut={{ modifiers: ["cmd", "shift"], key: "c" }}
                />
                <Action
                  title="Reload Entries"
                  icon={Icon.ArrowClockwise}
                  shortcut={{ modifiers: ["cmd"], key: "r" }}
                  onAction={loadEntries}
                />
              </ActionPanel>
            }
          />
        ))
      )}
    </List>
  );
}
