// Diagnostic for PAT-02 on live GitHub: three createCommitOnBranch mutations sent at the same moment with the
// same expected head, as three tabs do. Prints only each answer's HTTP status, GraphQL error types and messages,
// and whether a commit oid came back. Never prints the credential. Writes only under workspaces/r7-probe/.
import { randomUUID } from "node:crypto";

const token = process.env.CAUSA_WRITE_TOKEN;
const [owner, name] = (process.env.CAUSA_WRITE_REPO ?? "kemiller2002/causa-write-test").split("/");
const branch = process.env.CAUSA_WRITE_BRANCH ?? "main";
const headers = { authorization: `Bearer ${token}`, accept: "application/vnd.github+json", "user-agent": "causa-r7-probe" };

const head = (await (await fetch(`https://api.github.com/repos/${owner}/${name}/git/ref/heads/${branch}?n=${randomUUID()}`, { headers })).json()).object.sha;
const query = "mutation($input: CreateCommitOnBranchInput!) { createCommitOnBranch(input: $input) { commit { oid } } }";
const commit = (i) => {
  const input = {
    branch: { repositoryNameWithOwner: `${owner}/${name}`, branchName: branch },
    expectedHeadOid: head,
    message: { headline: `Causa probe: concurrent writer ${i}` },
    fileChanges: { additions: [{ path: `workspaces/r7-probe/${randomUUID()}.txt`, contents: Buffer.from(`probe ${i}\n`).toString("base64") }] },
  };
  return fetch("https://api.github.com/graphql", { method: "POST", headers: { ...headers, "content-type": "application/json" }, body: JSON.stringify({ query, variables: { input } }) })
    .then(async (r) => ({ status: r.status, body: await r.json().catch(() => null) }));
};

for (const round of [1, 2]) {
  const answers = await Promise.all([1, 2, 3].map(commit));
  answers.forEach(({ status, body }, i) => {
    const errors = (body?.errors ?? []).map((e) => `${e.type ?? "(no type)"}: ${e.message}`);
    const oid = body?.data?.createCommitOnBranch?.commit?.oid ? "commit returned" : "no commit";
    console.log(`PROBE round ${round} writer ${i + 1}: HTTP ${status}; ${oid}; ${errors.length ? errors.join(" | ") : "no errors"}; ${body?.message ? "message: " + body.message : ""}`);
  });
}
