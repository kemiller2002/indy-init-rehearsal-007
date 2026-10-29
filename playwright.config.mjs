// Browser acceptance tests. Fake target: the F# test server hosts dist/site and the GitHub fake.
// Live target (CAUSA_TARGET=live): the deployed site at CAUSA_BASE_URL with the real test repository.
import { defineConfig, devices } from "@playwright/test";

const live = process.env.CAUSA_TARGET === "live";

export default defineConfig({
  testDir: "tests/browser",
  testMatch: /.*\.spec\.mjs/,
  timeout: 180_000,
  workers: 1,
  retries: 0,
  reporter: [["list"], ["json", { outputFile: "test-results/browser.json" }], ["html", { open: "never", outputFolder: "playwright-report" }]],
  use: { trace: "off", screenshot: "off", video: "off" },
  projects: [
    { name: "chromium", use: { ...devices["Desktop Chrome"] } },
    ...(process.env.CAUSA_BROWSERS === "chromium" ? [] : [{ name: "webkit", use: { ...devices["Desktop Safari"] } }]),
  ],
  webServer: live
    ? undefined
    : {
        command: "dotnet tests/Causa.TestServer/bin/Release/net10.0/Causa.TestServer.dll dist/site 4300",
        url: "http://127.0.0.1:4300/index.html",
        reuseExistingServer: !process.env.CI,
        timeout: 120_000,
      },
});
