// PAT-15 (PERSIST-054): the save-state indicator in each of its seven states.
import { test, expect } from "@playwright/test";
import { baseURL, live, connect, workspace, createInvestigation, save, observe, expectState, openInvestigation, otherWriter, prefix, graphqlPattern, lozenge } from "./support.mjs";

test.describe.configure({ mode: "serial" });

const forbidden = /\b(saved|synced|stored|safe)\b/i;

// WCAG relative luminance contrast between the indicator text and its effective background.
async function contrast(locator) {
  return locator.evaluate((el) => {
    const parse = (c) => (c.match(/[\d.]+/g) ?? []).map(Number);
    const lum = ([r, g, b]) => {
      const f = (v) => { v /= 255; return v <= 0.03928 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4; };
      return 0.2126 * f(r) + 0.7152 * f(g) + 0.0722 * f(b);
    };
    let bg = [255, 255, 255];
    for (let n = el; n; n = n.parentElement) {
      const c = parse(getComputedStyle(n).backgroundColor);
      if (c.length >= 3 && (c.length < 4 || c[3] > 0)) { bg = c.slice(0, 3); break; }
    }
    const fg = parse(getComputedStyle(el).color).slice(0, 3);
    const [a, b] = [lum(fg), lum(bg)].sort((x, y) => y - x);
    return (a + 0.05) / (b + 0.05);
  });
}

async function verifyState(page, state, info, extra) {
  await expectState(page, state);
  const indicator = lozenge(page);
  await expect(indicator).toHaveClass(/ef-status-lozenge/);
  expect(await contrast(indicator), `${state} text contrast`).toBeGreaterThanOrEqual(4.5);
  const announcement = ["Conflict", "Unknown commit outcome", "Save failed"].includes(state) ? page.getByRole("alert") : page.getByRole("status").first();
  await expect(announcement).toContainText(state);
  const text = await page.locator(".causa-inv-header").innerText();
  if (state !== "Saved") expect(text.replace(/Unsaved|Save failed|Save to GitHub/g, "")).not.toMatch(forbidden);
  await extra();
  // Reflow at 320px and at 200% text size.
  for (const [width, scale] of [[320, "100%"], [1024, "200%"], [820, "100%"], [1180, "100%"]]) {
    await page.setViewportSize({ width, height: 900 });
    await page.evaluate((s) => { document.documentElement.style.fontSize = s; }, scale);
    await expect(indicator).toBeVisible();
    const overflow = await page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
    expect(overflow, `no horizontal scroll at ${width}px, ${scale}`).toBeLessThanOrEqual(0);
  }
  await page.evaluate(() => { document.documentElement.style.fontSize = ""; });
  await page.setViewportSize({ width: 1180, height: 900 });
  const shot = await page.locator(".causa-inv-header").screenshot();
  await info.attach(`PAT-15 ${state} (${info.project.name})`, { body: shot, contentType: "image/png" });
}

test("PAT-15 every save state renders truthfully", async ({ browser }, info) => {
  const ws = workspace();
  const context = await browser.newContext();
  const page = await context.newPage();
  await connect(page, ws);
  const id = await createInvestigation(page, "Indicator case");

  await verifyState(page, "Unsaved", info, async () => {
    await expect(page.getByRole("button", { name: "Save to GitHub" })).toBeVisible();
    await expect(page.locator(".causa-save__text")).toContainText("exist only in this tab");
  });

  // Saving: hold the mutation until the state is checked.
  let release;
  const held = new Promise((r) => { release = r; });
  await page.route(graphqlPattern, async (route) => { await held; await route.continue(); }, { times: 1 });
  await save(page);
  await verifyState(page, "Saving", info, async () => {
    await expect(page.getByRole("button", { name: "Save to GitHub" })).toHaveCount(0);
    await expect(page.locator(".causa-save__text")).toContainText("not confirmed");
  });
  release();

  await verifyState(page, "Saved", info, async () => {
    await expect(page.locator(".ef-freshness")).toContainText("Last saved");
    await expect(page.getByRole("button", { name: "Save to GitHub" })).toHaveCount(0);
  });

  // Offline with an unsaved change.
  await context.setOffline(true);
  await observe(page, "offline note");
  await verifyState(page, "Offline", info, async () => {
    await expect(page.locator(".causa-save__text")).toContainText("cannot be sent until the connection returns");
    await expect(page.getByRole("button", { name: "Save to GitHub" })).toHaveCount(0);
  });
  await context.setOffline(false);
  await expectState(page, "Unsaved");

  // Unknown commit outcome: the mutation reaches GitHub but its answer is lost; reconciliation is held back.
  await page.route(/compare\//, (route) => route.abort("internetdisconnected"));
  await page.route(graphqlPattern, async (route) => { await route.fetch(); await route.abort("connectionreset"); }, { times: 1 });
  await save(page);
  await verifyState(page, "Unknown commit outcome", info, async () => {
    await expect(page.locator(".ef-operation-status")).toBeVisible();
    await expect(page.getByRole("button", { name: "Save to GitHub" })).toHaveCount(0);
    await expect(page.locator(".ef-operation-status")).toContainText("may or may not have reached GitHub");
  });
  // The three automatic reads fail; then Check again is offered, and only then does the network return.
  await expect(page.getByRole("button", { name: "Check again" })).toBeVisible({ timeout: 30_000 });
  await page.unroute(/compare\//);
  await page.getByRole("button", { name: "Check again" }).click();
  await expectState(page, "Saved");

  // Save failed: GitHub refuses with a rate limit.
  await observe(page, "rate limited note");
  await page.route(graphqlPattern, (route) => route.fulfill({ status: 403, contentType: "application/json", body: JSON.stringify({ message: "API rate limit exceeded for user." }) }), { times: 1 });
  await save(page);
  await verifyState(page, "Save failed", info, async () => {
    await expect(page.locator(".ef-operation-status")).toContainText("rate limit");
    await expect(page.locator(".ef-operation-status")).toContainText("kept in this tab");
    await expect(page.getByRole("button", { name: "Retry" })).toBeVisible();
  });
  await page.getByRole("button", { name: "Retry" }).click();
  await expectState(page, "Saved");

  // Conflict: another writer changes this investigation's history.
  await observe(page, "conflicting note");
  await otherWriter(prefix(ws, id) + "events.ndjson", "", true);
  await save(page);
  await verifyState(page, "Conflict", info, async () => {
    await expect(page.locator(".ef-conflict-review")).toBeVisible();
    for (const name of ["Review the differences", "Reapply my changes to the GitHub version", "Discard my changes and load the GitHub version"]) {
      await expect(page.getByRole("button", { name })).toBeVisible();
    }
  });
  await context.close();
});

test("PERSIST-033 the header counts investigations that are not saved", async ({ browser }) => {
  const ws = workspace();
  const page = await (await browser.newContext()).newPage();
  await connect(page, ws);
  const a = await createInvestigation(page, "Header one");
  const b = await createInvestigation(page, "Header two");
  await expect(page.locator(".causa-unsaved")).toContainText("2 investigations are not saved");
  await expect(page.locator(".causa-unsaved a")).toHaveCount(2);
  await openInvestigation(page, a);
  await save(page);
  await expectState(page, "Saved");
  await expect(page.locator(".causa-unsaved")).toContainText("1 investigation is not saved");
  await openInvestigation(page, b);
  await save(page);
  await expectState(page, "Saved");
  await expect(page.locator(".causa-unsaved")).toHaveCount(0);
  await expect(page.locator(".causa-connection")).toHaveText("Connected");
});
