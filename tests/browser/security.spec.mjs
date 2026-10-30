// Security boundaries (EXT-DATA-004/005, PF-015, SECURITY-THREAT-MODEL): the credential stays in memory only,
// untrusted repository data fails closed, and the page runs under a restrictive Content-Security-Policy.
import { test, expect } from "@playwright/test";
import { baseURL, connect, workspace, createInvestigation, save, expectState, target, otherWriter, prefix, open, browserStores } from "./support.mjs";

test("the credential is never in the page, a URL, a request body or browser storage", async ({ page }) => {
  const urls = [];
  const bodies = [];
  page.on("request", (r) => { urls.push(r.url()); bodies.push(r.postData() ?? ""); });
  const ws = workspace();
  await connect(page, ws);
  await createInvestigation(page, "Credential case");
  await save(page);
  await expectState(page, "Saved");
  expect(await page.locator("#causa-app").innerHTML()).not.toContain(target.token);
  expect(urls.some((u) => u.includes(target.token))).toBe(false);
  expect(bodies.some((b) => b.includes(target.token))).toBe(false);
  expect(JSON.stringify(await browserStores(page))).not.toContain(target.token);
});

test("a forged investigation history is refused and reported, never rendered", async ({ browser }) => {
  const ws = workspace();
  const context = await browser.newContext();
  const page = await context.newPage();
  await connect(page, ws);
  const id = await createInvestigation(page, "Forgery target");
  await save(page);
  await expectState(page, "Saved");
  // Someone rewrites the recorded history in place on GitHub.
  await otherWriter(prefix(ws, id) + "events.ndjson", "{\"forged\":true}\n");
  const reader = await context.newPage();
  await connect(reader, ws);
  await reader.goto(`${baseURL}/#/i/${id}`);
  await expect(reader.getByRole("alert").filter({ hasText: "failed verification" })).toBeVisible();
  await expect(reader.locator("#inv-title")).toHaveText("");
  await expect(reader.getByRole("button", { name: "Save to GitHub" })).toHaveCount(0);
  await context.close();
});

test("the page declares a restrictive Content-Security-Policy and no inline script runs", async ({ page }) => {
  const violations = [];
  page.on("console", (m) => { if (/Content Security Policy/i.test(m.text())) violations.push(m.text()); });
  await open(page);
  const csp = await page.locator('meta[http-equiv="Content-Security-Policy"]').getAttribute("content");
  expect(csp).toContain("script-src 'self' 'wasm-unsafe-eval'");
  expect(csp).toContain("object-src 'none'");
  expect(csp).not.toContain("unsafe-inline");
  expect(await page.evaluate(() => document.querySelectorAll("script:not([src])").length)).toBe(0);
  expect(violations).toEqual([]);
});
