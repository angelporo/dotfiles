#!/usr/bin/env osascript -l JavaScript

ObjC.import("stdlib");
const app = Application.currentApplication();
app.includeStandardAdditions = true;

// Constants and Default Configuration
const DEFAULTS = {
  HOME_FOLDER: $.getenv("HOME"),
  IMAGE_EXTENSIONS: new Set([
    "png",
    "jpg",
    "jpeg",
    "gif",
    "svg",
    "ico",
    "tiff",
    "bmp",
    "icns",
    "webp",
    // "pdf",
  ]),
  CONFIG: {
    includeFolders: getEnvBoolean("INCLUDE_FOLDERS", true),
    foldersOnly: getEnvBoolean("ONLY_FOLDERS", false),
    showRelativePathNested: getEnvBoolean("SHOW_RELATIVE_NESTED_PATH", false),
    maxDepth: getEnvNumber("MAX_DEPTH", 4),
    defaultLocation: getEnvString(
      "DEFAULT_LOCATION",
      `${$.getenv("HOME")}/Desktop`,
    ),
  },
};

const config = { ...DEFAULTS.CONFIG };

function getEnvVar(key) {
  try {
    const value = $.getenv(key);
    if (value === undefined || value === null) {
      throw new Error(`Environment variable ${key} is not set`);
    }
    return value;
  } catch (error) {
    log(`Warning: ${error.message}, using default value`);
    return null;
  }
}

function getEnvBoolean(key, defaultValue) {
  const value = getEnvVar(key);
  if (value === null) return defaultValue;

  if (value !== "0" && value !== "1") {
    log(
      `Warning: ${key} should be "0" or "1", got "${value}". Using default value.`,
    );
    return defaultValue;
  }
  return value === "1";
}

function getEnvNumber(key, defaultValue) {
  const value = getEnvVar(key);
  if (value === null) return defaultValue;

  const num = Number(value);
  if (isNaN(num)) {
    log(
      `Warning: ${key} should be a number, got "${value}". Using default value.`,
    );
    return defaultValue;
  }
  return num;
}

function getEnvString(key, defaultValue) {
  const value = getEnvVar(key);
  return value === null || value.length == 0 ? defaultValue : value;
}

function log(message) {
  console.log(JSON.stringify(message));
}

function getFrontWin() {
  try {
    const finder = Application("Finder");
    // log(`getFrontWin: ${finder.windows.length}`);
    if (finder.windows.length == 0) return config.defaultLocation;
    const path = finder.insertionLocation().url().slice(7, -1);
    if (path.length == 0) return config.defaultLocation;
    return decodeURIComponent(path);
  } catch (_error) {
    return "";
  }
}

function runCommand(cmd) {
  try {
    return app.doShellScript(cmd);
  } catch (error) {
    console.log("Error running command:", error);
    return "";
  }
}

function escapeShellArg(arg) {
  return `'${arg.replace(/'/g, "'\\''")}'`;
}

function prettifyPath(path) {
  return path.replace(new RegExp(`^${DEFAULTS.HOME_FOLDER}`), "~");
}

function asAlfredItems(currentPath, results) {
  if (!results) return JSON.stringify({ items: [] });

  const files = results.split("\r").filter(Boolean);
  const currentFolderName = currentPath.split("/").pop();

  const items = files
    .map((filepath) => {
      try {
        const filename = filepath.split("/").pop();

        if (filename.startsWith(".")) return null;

        const prettyPath = prettifyPath(filepath);

        let subtitle = "";
        if (config.showRelativePathNested) {
          // Construct relative path for nested items
          const relativePath = filepath.replace(currentPath + "/", "");
          const isNested = relativePath.includes("/");
          if (isNested) {
            subtitle = `${currentFolderName}/${relativePath}`;
          }
        }

        const iconType = DEFAULTS.IMAGE_EXTENSIONS.has(
          filepath.split(".").pop().toLowerCase(),
        )
          ? "file"
          : "fileicon";

        return {
          title: filename,
          subtitle: subtitle,
          arg: filepath,
          type: "file:skipcheck",
          uid: filepath,
          icon: { type: iconType, path: filepath },
          text: {
            copy: filepath,
            largetype: prettyPath,
          },
          mods: {
            ctrl: {
              subtitle: prettyPath,
              arg: filepath,
            },
          },
        };
      } catch (error) {
        console.log("Error processing file:", filepath, error);
        return null;
      }
    })
    .filter(Boolean);

  return JSON.stringify({ items });
}

function buildFindCommand(currentPath, config) {
  const escapedPath = escapeShellArg(currentPath);
  const subst = `sed 's|^./|${currentPath}/|'`;
  const commands = [];

  commands.push(`cd ${escapedPath}`);

  if (config.foldersOnly) {
    // Simple case: only folders up to maxDepth
    commands.push(
      `find -E . -mindepth 1 -maxdepth ${config.maxDepth} -type d | ${subst}`,
    );
  } else {
    // First: immediate files in current directory
    commands.push(`find -E . -mindepth 1 -maxdepth 1 -type f | ${subst}`);

    if (config.includeFolders) {
      // Add immediate subfolders
      commands.push(`find -E . -mindepth 1 -maxdepth 1 -type d | ${subst}`);

      // Add files and folders at each depth level
      for (let depth = 2; depth <= config.maxDepth; depth++) {
        commands.push(
          `find -E . -mindepth ${depth} -maxdepth ${depth} -type f | ${subst}`,
        );
        commands.push(
          `find -E . -mindepth ${depth} -maxdepth ${depth} -type d | ${subst}`,
        );
      }
    } else {
      // Just files in subfolders without the folders themselves
      commands.push(
        `find -E . -mindepth 2 -maxdepth ${config.maxDepth} -type f | ${subst}`,
      );
    }
  }

  return commands.join(" && ");
}

function main() {
  let currentPath = getFrontWin();
  log(currentPath);
  const command = buildFindCommand(currentPath, config);
  const results = runCommand(command);
  return asAlfredItems(currentPath, results);
}

main();
