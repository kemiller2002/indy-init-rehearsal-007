// The investigation workflow end to end (UI-004, UI-005, UI-025, UI-032, UI-033): commands go through the
// F# engine, only legal transitions are offered, and the result reloads from GitHub unchanged.
import { test, expect } from "@playwright/test";
import { baseURL, connect, workspace, createInvestigation, save, expectState, openInvestigation } from "./support.mjs";

test("hypothesis, evidence, relationship, inspector, transition, lifecycle and reload", async ({ browser }) => {
  const ws = workspace();
  const context = await browser.newContext();
  const page = await context.newPage();
  await connect(page, ws);
  const id = await createInvestigation(page, "Burned-out bulb");

  await page.fill("#hyp-s", "The filament broke");
  await page.getByRole("button", { name: "Propose hypothesis" }).click();
  const hypotheses = page.getByRole("region", { name: "Hypotheses" });
  await expect(hypotheses.getByText("The filament broke")).toBeVisible();
  await expect(hypotheses.getByText("Proposed")).toBeVisible();

  const steps = page.getByRole("region", { name: "Next steps" });
  await expect(steps).toContainText("Relate evidence to the hypothesis: The filament broke");

  await page.fill("#ev-c", "The filament is visibly separated");
  await page.fill("#ev-s", "Inspection under a lamp");
  await page.getByRole("button", { name: "Add evidence" }).click();
  await page.selectOption("#rel-e", { label: "The filament is visibly separated" });
  await page.selectOption("#rel-h", { label: "The filament broke" });
  await page.selectOption("#rel-k", "supports");
  await page.getByRole("button", { name: "Relate", exact: true }).click();
  await expect(page.getByRole("region", { name: "Next steps" })).toHaveCount(0);

  // The inspector shows the record and its connected records, both ways.
  await page.getByRole("region", { name: "Evidence" }).getByRole("button", { name: /visibly separated/ }).click();
  const inspector = page.getByRole("complementary", { name: "Inspector" });
  await expect(inspector).toContainText("Inspection under a lamp");
  await inspector.getByRole("button", { name: /The filament broke/ }).click();
  await expect(inspector.locator(".causa-inspector__label")).toHaveText("The filament broke");

  // Only the transitions the engine accepts are offered.
  const offered = await inspector.locator("#causa-transitions button").allTextContents();
  expect(offered.sort()).toEqual(["Mark falsified", "Mark plausible", "Mark superseded"]);
  await page.fill("#tr-reason", "Consistent with the evidence");
  await inspector.getByRole("button", { name: "Mark plausible" }).click();
  await expect(hypotheses.getByText("Plausible")).toBeVisible();
  await expect(inspector.locator("#causa-transitions button", { hasText: "Mark supported" })).toBeVisible();

  await page.fill("#life-r", "Work has started");
  await page.getByRole("button", { name: "Mark active" }).click();
  await expect(page.locator(".causa-inv-header")).toContainText("Lifecycle: Active");

  await save(page);
  await expectState(page, "Saved");

  // A new tab reads the same state back from GitHub.
  const other = await context.newPage();
  await connect(other, ws);
  await openInvestigation(other, id);
  await expectState(other, "Saved");
  await expect(other.locator(".causa-inv-header")).toContainText("Lifecycle: Active");
  await expect(other.getByRole("region", { name: "Hypotheses" }).getByText("Plausible")).toBeVisible();
  await expect(other.getByRole("region", { name: "Timeline" })).toContainText("Hypothesis transitioned");
  await context.close();
});

test("every Limen template has exactly one root element", async ({ page }) => {
  await page.goto(baseURL + "/");
  const bad = await page.evaluate(() =>
    [...document.querySelectorAll("template")]
      .filter((t) => t.content.children.length !== 1)
      .map((t) => `${t.getAttribute("data-if") ?? t.getAttribute("data-each")}: ${t.content.children.length} roots`));
  expect(bad).toEqual([]);
});
