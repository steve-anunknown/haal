// Driver for examples/email.hs: asks headless Chromium whether strings are
// valid values of an <input type="email">.
//
// Protocol, one query per line:
//   stdin:  a JSON string, e.g. "a@a.a"
//   stdout: one character per nonempty prefix of it, 1 = valid, 0 = invalid
//
// The browser is the one installed on the system (CHROMIUM, default
// /snap/bin/chromium). The temporary profile lives in the snap's own data
// directory, because the snap can write neither to /tmp nor to hidden
// directories in $HOME (PROFILE_PARENT overrides it for a non-snap browser).

import { mkdtempSync, rmSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { createInterface } from "node:readline";
import puppeteer from "puppeteer-core";

const profileParent =
  process.env.PROFILE_PARENT ?? join(homedir(), "snap", "chromium", "common");
const profile = mkdtempSync(join(profileParent, "haal-email-driver-"));
const browser = await puppeteer.launch({
  executablePath: process.env.CHROMIUM ?? "/snap/bin/chromium",
  userDataDir: profile,
  args: ["--no-first-run", "--no-default-browser-check"],
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
