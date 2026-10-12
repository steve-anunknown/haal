// Driver for examples/email.hs: asks a headless browser whether strings are
// valid values of an <input type="email">.
//
// Usage: node driver.mjs chromium|firefox
//
// Protocol, one query per line:
//   stdin:  a JSON string, e.g. "a@a.a"
//   stdout: one character per nonempty prefix of it, 1 = valid, 0 = invalid
//
// The browser is the one installed on the system (BROWSER_PATH, default
// /snap/bin/<browser>). The temporary profile lives in the snap's own data
// directory, because a snap can write neither to /tmp nor to hidden
// directories in $HOME (PROFILE_PARENT overrides it for a non-snap browser).

import { mkdtempSync, rmSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { createInterface } from "node:readline";
import puppeteer from "puppeteer-core";

const name = process.argv[2];
if (name !== "chromium" && name !== "firefox") {
  console.error("usage: node driver.mjs chromium|firefox");
  process.exit(2);
}

const profileParent =
  process.env.PROFILE_PARENT ?? join(homedir(), "snap", name, "common");
const profile = mkdtempSync(join(profileParent, "haal-email-driver-"));
const browser = await puppeteer.launch({
  browser: name === "firefox" ? "firefox" : "chrome",
  executablePath: process.env.BROWSER_PATH ?? `/snap/bin/${name}`,
  userDataDir: profile,
  args: name === "chromium" ? ["--no-first-run", "--no-default-browser-check"] : [],
});

async function shutdown() {
  await browser.close();
  rmSync(profile, { recursive: true, force: true });
}

const page = await browser.newPage();
await page.setContent('<input type="email" id="e">');

// Verdicts on all nonempty prefixes in one round trip. The browser strips
// leading and trailing whitespace from the value, and an empty value is never
// a type mismatch, so a value that is empty after stripping counts as invalid:
// it is no address at all.
const verdicts = (word) =>
  page.evaluate((w) => {
    const input = document.getElementById("e");
    let out = "";
    for (let n = 1; n <= w.length; n++) {
      input.value = w.slice(0, n);
      out += input.value !== "" && !input.validity.typeMismatch ? "1" : "0";
    }
    return out;
  }, word);

const lines = createInterface({ input: process.stdin, crlfDelay: Infinity });
try {
  for await (const line of lines) {
    process.stdout.write((await verdicts(JSON.parse(line))) + "\n");
  }
} finally {
  await shutdown();
}
