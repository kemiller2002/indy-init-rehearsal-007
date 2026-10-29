// Causa composition root: the only browser-side script. It starts the Limen kernel over the
// F# WebAssembly engine and relays three platform signals the kernel does not carry:
// connectivity, timers the engine asks for, and the leave warning (PERSIST-019).
import { BrowserKernel } from "./limen/dist/index.js";
import { DotnetWasmTransport } from "./limen/dist/hosts/dotnet-wasm-transport.js";

const byId = (id) => document.getElementById(id);

const failStart = () => {
  byId("causa-loading-text").textContent = "Causa could not start.";
  byId("causa-loading-failed").hidden = false;
  byId("causa-reload").addEventListener("click", () => location.reload());
};

const transport = new DotnetWasmTransport({
  loadRuntime: () => import("./_framework/dotnet.js"),
  exportPath: ["Causa", "Host", "Dispatch"],
});

// Diagnostics carry mechanism only; effect contents (and so credentials) are never logged.
const kernel = new BrowserKernel(transport, document, {
  report(event) {
    if (event.kind === "BridgeError") console.error("causa bridge", event.phase);
  },
});

await kernel.start();

if (kernel.status !== "running") {
  failStart();
} else {
  byId("causa-loading").hidden = true;
  byId("causa-app").hidden = false;

  const signal = (id, type, value) => {
    const input = byId(id);
    input.value = value;
    input.dispatchEvent(new Event(type));
  };

  const connectivity = () => signal("causa-net", "causa-net", navigator.onLine ? "online" : "offline");
  addEventListener("online", connectivity);
  addEventListener("offline", connectivity);
  if (!navigator.onLine) connectivity();

  const warn = (event) => {
    event.preventDefault();
    event.returnValue = "";
  };
  const guard = byId("causa-guard");
  let warning = false;
  let timer = "";
  const observe = () => {
    const wanted = guard.dataset.guard === "on";
    if (wanted !== warning) {
      warning = wanted;
      if (wanted) addEventListener("beforeunload", warn);
      else removeEventListener("beforeunload", warn);
    }
    const requested = guard.dataset.timer || "";
    if (requested && requested !== timer) {
      timer = requested;
      const [id, ms] = requested.split(":");
      setTimeout(() => signal("causa-timer", "causa-timer", id), Number(ms));
    }
  };
  new MutationObserver(observe).observe(guard, { attributes: true });
  observe();
}
