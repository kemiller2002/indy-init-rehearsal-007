// Browser persistence acceptance: PAT-01, PAT-07, PAT-09, PAT-11 and the browser halves of PAT-02/04/05.
import { test, expect } from "@playwright/test";
import {
  baseURL, live, connect, workspace, createInvestigation, save, observe, expectState, openInvestigation,
  browserStores, expectStoresFree, countRequests, otherWriter, watchLocks, target,
} from "./support.mjs";

test.describe.configure({ mode: "serial" });

async function seeded(browser, ws, titles, removeLocks = false) {
  const context = await browser.newContext();
  const page = await context.newPage();
  await watchLocks(page, removeLocks);
  await connect(page, ws);
  const ids = [];
  for (const title of titles) {
    ids.push(await createInvestigation(page, title));
    await save(page);
    await expectState(page, "Saved");
  }
  await context.close();
  return ids;
}

async function tab(context, ws, removeLocks) {
  const page = await context.newPage();
  await watchLocks(page, removeLocks);
  await connect(page, ws);
  return page;
}

for (const removeLocks of [false, true]) {
  const suffix = removeLocks ? " (PAT-11: navigator.locks removed)" : "";

  test(`PAT-01 independent browser-tab state${suffix}`, async ({ browser }) => {
    const ws = workspace();
    const [id] = await seeded(browser, ws, ["Dark lamp"], removeLocks);
    const context = await browser.newContext();
    const first = await tab(context, ws, removeLocks);
    const second = await tab(context, ws, removeLocks);
    await openInvestigation(first, id);
    await openInvestigation(second, id);
    await observe(first, "first tab saw the switch was on");
    await observe(second, "second tab saw a loose bulb");
    await expect(first.getByRole("region", { name: "Observations" }).getByText("second tab saw a loose bulb")).toHaveCount(0);
    await expect(second.getByRole("region", { name: "Observations" }).getByText("first tab saw the switch was on")).toHaveCount(0);
    await expectState(first, "Unsaved");
    await expectState(second, "Unsaved");
    for (const page of [first, second]) {
      expectStoresFree(await browserStores(page), ["Dark lamp", "first tab saw", "second tab saw", ws, id]);
    }
    await save(first);
    await expectState(first, "Saved");
    await expect(second.getByRole("region", { name: "Observations" }).getByText("first tab saw the switch was on")).toHaveCount(0);
    await expectState(second, "Unsaved");
    for (const page of [first, second]) {
      expectStoresFree(await browserStores(page), ["Dark lamp", "first tab saw", "second tab saw", ws, id]);
      expect(await page.evaluate(() => window.__locksUsed)).toBe(0);
    }
    await context.close();
  });

  test(`PAT-02 three tabs save different investigations at once${suffix}`, async ({ browser }) => {
    const ws = workspace();
    const ids = await seeded(browser, ws, ["One", "Two", "Three"], removeLocks);
    const context = await browser.newContext();
    const pages = [];
    for (const id of ids) {
      const page = await tab(context, ws, removeLocks);
      await openInvestigation(page, id);
      await observe(page, `change for ${id}`);
      pages.push(page);
    }
    await Promise.all(pages.map((page) => save(page)));
    for (const page of pages) await expectState(page, "Saved");
    const check = await tab(context, ws, removeLocks);
    for (const id of ids) {
      await openInvestigation(check, id);
      await expect(check.getByRole("region", { name: "Observations" }).getByText(`change for ${id}`)).toHaveCount(1);
    }
    for (const page of pages) expect(await page.evaluate(() => window.__locksUsed)).toBe(0);
    await context.close();
  });

  test(`PAT-04 automatic rebase over unrelated changes${suffix}`, async ({ browser }) => {
    const ws = workspace();
    const [a, b] = await seeded(browser, ws, ["Alpha", "Beta"], removeLocks);
    const context = await browser.newContext();
    const page = await tab(context, ws, removeLocks);
    await openInvestigation(page, b);
    await observe(page, "same tab, other investigation");
    await save(page);
    await expectState(page, "Saved");
    await otherWriter("notes/outside.md", "outside\n");
    await openInvestigation(page, a);
    await observe(page, "after unrelated commits");
    await save(page);
    await expectState(page, "Saved");
    expect(await page.evaluate(() => window.__locksUsed)).toBe(0);
    await context.close();
  });

  test(`PAT-05 same-investigation conflict preservation${suffix}`, async ({ browser }) => {
    const ws = workspace();
    const [id] = await seeded(browser, ws, ["Shared"], removeLocks);
    const context = await browser.newContext();
    const first = await tab(context, ws, removeLocks);
    const second = await tab(context, ws, removeLocks);
    await openInvestigation(first, id);
    await openInvestigation(second, id);
    await observe(first, "kept in the first tab");
    await observe(second, "saved by the second tab");
    await save(second);
    await expectState(second, "Saved");
    const counts = countRequests(first);
    await save(first);
    await expectState(first, "Conflict");
    await expect(first.getByRole("region", { name: "Observations" }).getByText("kept in the first tab")).toBeVisible();
    const sent = counts.mutations;
    await first.getByRole("button", { name: "Review the differences" }).click();
    await expect(first.locator(".ef-diff-viewer")).toBeVisible();
    await first.getByRole("button", { name: "Discard my changes and load the GitHub version" }).click();
    await expect(first.getByRole("alertdialog")).toBeVisible();
    await first.getByRole("button", { name: "Keep my changes" }).click();
    expect(counts.mutations).toBe(sent);
    await first.getByRole("button", { name: "Reapply my changes to the GitHub version" }).click();
    await expectState(first, "Unsaved");
    await save(first);
    await expectState(first, "Saved");
    await expect(first.getByRole("region", { name: "Observations" }).getByText("saved by the second tab")).toBeVisible();
    await expect(first.getByRole("region", { name: "Observations" }).getByText("kept in the first tab")).toBeVisible();
    expect(await first.evaluate(() => window.__locksUsed)).toBe(0);
    await context.close();
  });
}

test("PAT-07 offline save failure", async ({ browser }) => {
  const ws = workspace();
  const [id] = await seeded(browser, ws, ["Offline case"]);
  const context = await browser.newContext();
  const page = await tab(context, ws, false);
  await openInvestigation(page, id);
  const counts = countRequests(page);
  await context.setOffline(true);
  await expect(page.locator(".causa-connection")).toHaveText("Offline");
  await expect(page.locator("#inv-title")).toHaveText("Offline case");
  await observe(page, "noted while offline");
  await expectState(page, "Offline");
  await expect(page.getByRole("button", { name: "Save to GitHub" })).toHaveCount(0);
  await page.goto(`${baseURL}/#/i/00000000-0000-4000-8000-00000000abcd`);
  await expect(page.getByText(/unavailable until the connection returns/)).toBeVisible();
  await page.goto(`${baseURL}/#/i/${id}`);
  await expect(page.getByRole("region", { name: "Observations" }).getByText("noted while offline")).toBeVisible();
  expect(counts.all).toBe(0);
  await context.setOffline(false);
  await expectState(page, "Unsaved");
  await page.waitForTimeout(3000);
  expect(counts.mutations).toBe(0);
  await save(page);
  await expectState(page, "Saved");

  // The network fails only on the rebase path: never Conflict, and no Discard.
  await otherWriter("notes/rebase-path.md", "moved\n");
  await observe(page, "rebase path change");
  await page.route(/git\/ref\/heads|contents\//, (route) => route.abort("internetdisconnected"));
  await save(page);
  await expect(page.locator(".causa-inv-header .ef-status-lozenge").first()).toHaveText(/Offline|Unknown commit outcome/, { timeout: 30_000 });
  await expect(page.getByRole("button", { name: /Discard/ })).toHaveCount(0);
  await expect(page.getByText("Conflict", { exact: true })).toHaveCount(0);
  await context.close();
});

test("PAT-09 navigation with unsaved changes", async ({ browser, browserName }) => {
  const ws = workspace();
  const [id] = await seeded(browser, ws, ["Leaving"]);
  const context = await browser.newContext();
  const page = await tab(context, ws, false);
  await openInvestigation(page, id);
  await observe(page, "unsaved before leaving");

  // Moving between routes keeps the change.
  await page.goto(`${baseURL}/#/help`);
  await expect(page.locator("#help-statement")).toContainText("exist only in this browser tab");
  await page.goto(`${baseURL}/#/i/${id}`);
  await expect(page.getByRole("region", { name: "Observations" }).getByText("unsaved before leaving")).toBeVisible();

  // Close, reload and an external link all request a leave warning. PAT-09 requires the warning in Chromium;
  // WebKit (Safari) may not show one, so there the confirmations and the statement are what is verified.
  const dialogs = [];
  page.on("dialog", async (dialog) => { dialogs.push(dialog.type()); await dialog.dismiss(); });
  if (browserName === "chromium") {
    await page.locator("#inv-title").click();
    await page.close({ runBeforeUnload: true });
    await expect.poll(() => dialogs.length).toBe(1);
    expect(dialogs[0]).toBe("beforeunload");
    expect(page.isClosed()).toBe(false);
    // A dismissed leave warning cancels the reload, so its promise never settles; bound the wait.
    await page.reload({ timeout: 3000 }).catch(() => {});
    await expect.poll(() => dialogs.length).toBeGreaterThanOrEqual(2);
    await page.evaluate(() => { location.href = "https://example.com/"; }, { timeout: 3000 }).catch(() => {});
    await expect.poll(() => dialogs.length).toBeGreaterThanOrEqual(3);
    expect(dialogs.every((d) => d === "beforeunload")).toBe(true);
  }
  expect(await page.evaluate(() => document.getElementById("causa-guard").dataset.guard)).toBe("on");

  // Disconnect asks for confirmation.
  await page.getByRole("button", { name: "Disconnect" }).click();
  await expect(page.getByRole("alertdialog")).toContainText("lose unsaved changes");
  await page.getByRole("button", { name: "Keep working" }).click();

  // Once everything is Saved, no warning is requested.
  await save(page);
  await expectState(page, "Saved");
  expect(await page.evaluate(() => document.getElementById("causa-guard").dataset.guard)).toBe("off");
  if (browserName === "chromium") {
    const before = dialogs.length;
    await page.close({ runBeforeUnload: true });
    await expect.poll(() => page.isClosed()).toBe(true);
    expect(dialogs.length).toBe(before);
  }
  await context.close();
});

test("PERSIST-020 the tab statement is present and reachable", async ({ page }) => {
  await page.goto(baseURL + "/");
  await expect(page.locator("#causa-app")).toBeVisible({ timeout: 60_000 });
  const statement = page.locator("#tab-statement");
  await expect(statement).toContainText("exist only in this browser tab");
  await expect(statement).toContainText("loses unsaved changes");
  const snapshot = await page.accessibility?.snapshot?.();
  if (snapshot) expect(JSON.stringify(snapshot)).toContain("exist only in this browser tab");
  await page.goto(baseURL + "/#/help");
  await expect(page.locator("#help-statement")).toBeVisible();
});

test("EXT-DATA-002 a fresh launch is disconnected and empty", async ({ page }) => {
  await page.goto(baseURL + "/");
  await expect(page.locator("#causa-app")).toBeVisible({ timeout: 60_000 });
  await expect(page.locator(".causa-connection")).toHaveText("Disconnected");
  await expect(page.getByRole("heading", { name: "Connect a data repository" })).toBeVisible();
  await expect(page.locator(".causa-card")).toHaveCount(0);
});

test("EXT-DATA-009 a refused credential is denied, not connected", async ({ page }) => {
  await page.goto(baseURL + "/");
  await expect(page.locator("#causa-app")).toBeVisible({ timeout: 60_000 });
  await page.fill("#c-owner", target.owner);
  await page.fill("#c-repo", target.name);
  await page.fill("#c-ws", workspace());
  await page.fill("#c-token", "not-a-valid-token");
  if (target.api) {
    await page.locator("details summary").click();
    await page.fill("#c-api", target.api);
  }
  await page.getByRole("button", { name: "Connect" }).click();
  await expect(page.locator(".causa-connection")).toHaveText("Access denied");
});
