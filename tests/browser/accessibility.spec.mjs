// UI-011 and PERSIST-030 accessibility: axe-core WCAG 2.1 AA scans of each screen, keyboard-only operation,
// and the reduced-motion loading screen. Visual appearance alone is not accessibility verification.
import { test, expect } from "@playwright/test";
import AxeBuilder from "@axe-core/playwright";
import { baseURL, connect, workspace, createInvestigation, save, expectState, open, target } from "./support.mjs";

async function scan(page, label) {
  const results = await new AxeBuilder({ page }).withTags(["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"]).analyze();
  const summary = results.violations.map((v) => `${v.id} (${v.impact}): ${v.nodes.length} node(s) — ${v.nodes[0]?.target.join(" ")}`);
  expect(summary, `${label}: axe WCAG 2.1 AA violations`).toEqual([]);
}

for (const scheme of ["light", "dark"]) {
  test(`axe scans of every screen (${scheme})`, async ({ browser }) => {
    const context = await browser.newContext({ colorScheme: scheme });
    const page = await context.newPage();
    await open(page);
    await scan(page, "connect");
    await page.goto(baseURL + "/#/help");
    await scan(page, "help");
    await page.goto(baseURL + "/#/settings");
    await scan(page, "settings");
    await connect(page, workspace());
    await scan(page, "empty workspace");
    await createInvestigation(page, "Accessible case");
    await scan(page, "investigation (Unsaved)");
    await save(page);
    await expectState(page, "Saved");
    await page.getByRole("button", { name: /The bulb|Propose hypothesis/ }).first().isVisible();
    await scan(page, "investigation (Saved)");
    await page.goto(baseURL + "/#/");
    await scan(page, "dashboard");
    await page.goto(baseURL + "/#/admin");
    await expect(page.getByRole("heading", { name: "Administration" })).toBeVisible();
    await scan(page, "administration");
    await context.close();
  });
}

test("keyboard only: connect, start, record and save", async ({ page }) => {
  await open(page);
  const type = async (selector, text) => { await page.locator(selector).focus(); await page.keyboard.type(text); };
  await page.keyboard.press("Tab");
  await expect(page.locator(".causa-skip")).toBeFocused();
  await type("#c-owner", target.owner);
  await type("#c-repo", target.name);
  await type("#c-ws", workspace());
  await type("#c-token", target.token);
  if (target.api) {
    await page.locator("details summary").focus();
    await page.keyboard.press("Enter");
    await type("#c-api", target.api);
  }
  await page.locator("#c-token").focus();
  await page.keyboard.press("Enter");
  await expect(page.locator(".causa-connection")).toHaveText(/^Connected/);
  await type("#n-title", "Keyboard case");
  await type("#n-problem", "Typed without a pointer");
  await page.keyboard.press("Tab");
  await expect(page.getByRole("button", { name: "Start investigation" })).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(page.locator("#inv-title")).toHaveText("Keyboard case");
  await type("#obs-t", "observed by keyboard");
  await page.keyboard.press("Tab");
  await page.keyboard.press("Enter");
  await page.getByRole("button", { name: "Save to GitHub" }).focus();
  await page.keyboard.press("Enter");
  await expectState(page, "Saved");
  await page.keyboard.press("Tab");
  const outline = await page.evaluate(() => getComputedStyle(document.activeElement).outlineStyle);
  expect(outline).not.toBe("none");
});

test("reduced motion stops the loading animation", async ({ browser }) => {
  const context = await browser.newContext({ reducedMotion: "reduce" });
  const page = await context.newPage();
  await page.goto(baseURL + "/");
  const animation = await page.locator(".causa-loading__mark").evaluate((el) => getComputedStyle(el).animationName);
  expect(animation).toBe("none");
  await context.close();
});
