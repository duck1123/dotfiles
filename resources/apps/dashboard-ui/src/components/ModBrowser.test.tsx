import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { createMockModSource } from "../mods/mockModSource";
import { sampleMods } from "../mods/sampleMods";
import type { ModSource } from "../mods/types";
import { ModBrowser, matchesQuery } from "./ModBrowser";
import { sortVersions } from "./ModDetail";

function setup(source: ModSource = createMockModSource(), selectedId?: string) {
  const list = vi.spyOn(source, "list");
  const onSelect = vi.fn();
  render(<ModBrowser source={source} selectedId={selectedId} onSelect={onSelect} />);
  return { list, onSelect, user: userEvent.setup() };
}

const modButtons = () => within(screen.getByRole("navigation", { name: "Mods" })).getAllByRole("button");

test("lists every mod and counts the enabled ones", async () => {
  setup();
  await waitFor(() => expect(modButtons()).toHaveLength(sampleMods.length));
  const enabled = sampleMods.filter((m) => m.enable).length;
  expect(screen.getByText(`${sampleMods.length} mods · ${enabled} enabled in current`)).toBeInTheDocument();
});

test("search matches every word against id, title and tags", async () => {
  const { user } = setup();
  await waitFor(() => expect(modButtons()).toHaveLength(sampleMods.length));
  await user.type(screen.getByRole("searchbox", { name: "Search mods" }), "acme build");
  expect(modButtons()).toHaveLength(2);
  expect(screen.getByText(`2 of ${sampleMods.length} mods`, { exact: false })).toBeInTheDocument();
});

test("filters by state and tag", async () => {
  const { user } = setup();
  await waitFor(() => expect(modButtons()).toHaveLength(sampleMods.length));
  await user.selectOptions(screen.getByRole("combobox", { name: "Show" }), "no-page");
  expect(modButtons()).toHaveLength(sampleMods.filter((m) => m.downloadPage === null).length);
  await user.selectOptions(screen.getByRole("combobox", { name: "Show" }), "all");
  await user.selectOptions(screen.getByRole("combobox", { name: "Tag" }), "script");
  expect(modButtons()).toHaveLength(2);
});

test("selecting a mod shows it, and requires links to the required mod", async () => {
  const { onSelect, user } = setup();
  await waitFor(() => expect(modButtons()).toHaveLength(sampleMods.length));
  await user.click(screen.getByText("acme/widgets").closest("button")!);
  expect(screen.getByRole("heading", { level: 2 })).toHaveTextContent("Acme Widgets");
  await user.click(within(screen.getByRole("main")).getByRole("button", { name: "acme/core" }));
  expect(screen.getByRole("heading", { level: 2 })).toHaveTextContent("Acme Core Library");
  expect(onSelect).toHaveBeenLastCalledWith("acme/core");
});

test("shows the mod's problems and the ones naming no mod", async () => {
  setup(undefined, "nimbus/weather");
  expect(await screen.findByRole("list", { name: "Problems" })).toHaveTextContent("conflicts with zed/loose");
  expect(screen.getByRole("list", { name: "Catalog problems" })).toHaveTextContent("unknown base profile vanilla");
});

test("changing the profile reloads the list for it", async () => {
  const { list, user } = setup();
  await waitFor(() => expect(modButtons()).toHaveLength(sampleMods.length));
  await user.selectOptions(screen.getByRole("combobox", { name: "Profile" }), "testing");
  await waitFor(() => expect(list).toHaveBeenLastCalledWith("testing", expect.any(AbortSignal)));
});

test("a load error can be retried", async () => {
  const source = createMockModSource({ fail: "nix eval failed" });
  const { list, user } = setup(source);
  expect(await screen.findByRole("alert")).toHaveTextContent("Couldn't load the mods: nix eval failed");
  await user.click(screen.getByRole("button", { name: "Retry" }));
  await waitFor(() => expect(list).toHaveBeenCalledTimes(2));
});

test("helpers", () => {
  expect(sortVersions(["1.9", "1.10", "1.2"])).toEqual(["1.10", "1.9", "1.2"]);
  expect(matchesQuery(sampleMods[0], "LIBRARY acme")).toBe(true);
  expect(matchesQuery(sampleMods[0], "widgets")).toBe(false);
});
