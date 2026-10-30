// Shared browser-test support. Two targets:
//   fake (default): the test server at CAUSA_BASE_URL serving dist/site and the stateful fake at /github.
//   live: CAUSA_TARGET=live, the deployed site at CAUSA_BASE_URL against the real GitHub test repository
//         with CAUSA_WRITE_TOKEN. Faults are injected at the transport with page.route in both targets.
import { expect } from "@playwright/test";
import { randomUUID } from "node:crypto";

export const live = process.env.CAUSA_TARGET === "live";
export const baseURL = process.env.CAUSA_BASE_URL ?? "http://127.0.0.1:4300";
const repo = (process.env.CAUSA_WRITE_REPO ?? "kemiller2002/causa-write-test").split("/");

export const target = live
  ? { owner: repo[0], name: repo[1], branch: process.env.CAUSA_WRITE_BRANCH ?? "main", token: process.env.CAUSA_WRITE_TOKEN, api: "", apiBase: "https://api.github.com" }
  : { owner: "causa-test", name: "data", branch: "main", token: "test-token", api: "/github", apiBase: `${baseURL}/github` };

export const graphqlPattern = live ? "https://api.github.com/graphql" : "**/github/graphql";

export const workspace = () => `workspaces/r7-browser/${randomUUID().slice(0, 12)}`;

export const prefix = (ws, id) => `${ws}/investigations/${id}/`;

async function fake(name, query = "", body) {
  const response = await fetch(`${baseURL}/__fake/${name}${query}`, { method: "POST", body });
  return response.json();
}

export const fakeStatus = () => fake("status");
export const fakeControl = fake;

async function gh(path, init = {}) {
  const response = await fetch(`https://api.github.com${path}`, {
    ...init,
    headers: { authorization: `Bearer ${target.token}`, accept: "application/vnd.github+json", "user-agent": "causa-r7-browser", ...(init.headers ?? {}) },
  });
  return { status: response.status, body: await response.json() };
}

async function liveHead() {
  const r = await gh(`/repos/${target.owner}/${target.name}/git/ref/heads/${target.branch}?n=${randomUUID()}`);
  return r.body.object.sha;
}

/** Another writer commits `content` to `path` (replacing it, or appending when `append`). */
export async function otherWriter(path, content, append = false) {
  if (!live) return append ? fake("append-line", `?path=${encodeURIComponent(path)}`) : fake("other-writer", `?path=${encodeURIComponent(path)}`, content);
  for (let attempt = 0; attempt < 5; attempt++) {
    const head = await liveHead();
    let text = content;
    if (append) {
      const file = await gh(`/repos/${target.owner}/${target.name}/contents/${path}?ref=${head}`);
      text = Buffer.from(file.body.content, "base64").toString("utf8") + "x";
    }
    const query = "mutation($input: CreateCommitOnBranchInput!) { createCommitOnBranch(input: $input) { commit { oid } } }";
    const input = {
      branch: { repositoryNameWithOwner: `${target.owner}/${target.name}`, branchName: target.branch },
      expectedHeadOid: head,
      message: { headline: "Causa browser test: another writer" },
      fileChanges: { additions: [{ path, contents: Buffer.from(text).toString("base64") }] },
    };
    const r = await gh("/graphql", { method: "POST", body: JSON.stringify({ query, variables: { input } }), headers: { "content-type": "application/json" } });
    if (r.body?.data?.createCommitOnBranch?.commit?.oid) return r.body.data.createCommitOnBranch.commit.oid;
  }
  throw new Error("the other writer could not commit");
}

/** Counts every GitHub request the page sends (by kind), using the page's own network events. */
export function countRequests(page) {
  const counts = { mutations: 0, reads: 0, all: 0 };
  page.on("request", (request) => {
    const url = request.url();
    if (!url.includes("/github") && !url.startsWith("https://api.github.com")) return;
    counts.all++;
    if (request.method() === "POST" && url.endsWith("/graphql")) counts.mutations++;
    else counts.reads++;
  });
  return counts;
}

/** Records any use of navigator.locks; optionally removes Web Locks before the app starts (PAT-11). */
export async function watchLocks(page, remove) {
  await page.addInitScript((removeLocks) => {
    window.__locksUsed = 0;
    if (removeLocks) {
      Object.defineProperty(Navigator.prototype, "locks", { get() { window.__locksUsed++; return undefined; }, configurable: true });
    } else if (navigator.locks) {
      const real = navigator.locks;
      Object.defineProperty(Navigator.prototype, "locks", { get() { window.__locksUsed++; return real; }, configurable: true });
    }
  }, remove);
}

export async function open(page) {
  await page.goto(baseURL + "/");
  await expect(page.locator("#causa-app")).toBeVisible({ timeout: 60_000 });
}

export async function connect(page, ws) {
  await open(page);
  await page.fill("#c-owner", target.owner);
  await page.fill("#c-repo", target.name);
  await page.fill("#c-branch", target.branch);
  await page.fill("#c-ws", ws);
  await page.fill("#c-token", target.token);
  if (target.api) {
    await page.locator("details summary").click();
    await page.fill("#c-api", target.api);
  }
  await page.getByRole("button", { name: "Connect" }).click();
  await expect(page.locator(".causa-connection")).toHaveText(/^Connected/, { timeout: 30_000 });
  await expect(page.locator(".causa-connection")).not.toHaveText(/loading/, { timeout: 30_000 });
}

export const lozenge = (page) => page.locator(".causa-inv-header .ef-status-lozenge").first();

export async function expectState(page, state, timeout = 30_000) {
  try {
    await expect(lozenge(page)).toHaveText(state, { timeout });
  } catch (error) {
    // Name the state and the cause the app reported, so a failure explains itself (no credentials are on the page).
    const explanation = await page.locator(".causa-inv-header").innerText().catch(() => "(no header)");
    throw new Error(`expected ${state}; the indicator reports:\n${explanation}\n${error.message}`);
  }
}

export async function createInvestigation(page, title) {
  await page.goto(baseURL + "/#/");
  await page.fill("#n-title", title);
  await page.fill("#n-problem", `What happened to ${title}?`);
  await page.getByRole("button", { name: "Start investigation" }).click();
  await expect(page.locator("#inv-title")).toHaveText(title);
  const id = new URL(page.url()).hash.replace("#/i/", "");
  return id;
}

export async function save(page) {
  await page.getByRole("button", { name: "Save to GitHub" }).click();
}

export async function observe(page, text) {
  await page.fill("#obs-t", text);
  await page.getByRole("button", { name: "Record observation" }).click();
  await expect(page.locator("#obs-t")).toHaveValue("");
}

export async function openInvestigation(page, id) {
  await page.goto(`${baseURL}/#/i/${id}`);
  await expect(page.locator("#inv-title")).toBeVisible();
}

/** Every browser store a page can write; none may hold investigation content (PERSIST-003). */
export async function browserStores(page) {
  return page.evaluate(async () => {
    const local = JSON.stringify({ ...localStorage });
    const session = JSON.stringify({ ...sessionStorage });
    const databases = indexedDB.databases ? (await indexedDB.databases()).map((d) => d.name) : [];
    const caches = "caches" in self ? await self.caches.keys() : [];
    const cookies = document.cookie;
    let opfs = [];
    try {
      const root = await navigator.storage.getDirectory();
      for await (const name of root.keys()) opfs.push(name);
    } catch { opfs = []; }
    return { local, session, databases, caches, cookies, opfs };
  });
}

export function expectStoresFree(stores, needles) {
  const all = JSON.stringify(stores);
  for (const needle of needles) expect(all, `no browser store holds "${needle}"`).not.toContain(needle);
  expect(stores.databases).toEqual([]);
  expect(stores.caches).toEqual([]);
  expect(stores.opfs).toEqual([]);
}
