import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { createMockModSource, type MockModOptions } from "../mods/mockModSource";
import { sampleMods } from "../mods/sampleMods";
import { ModBrowser } from "./ModBrowser";
import { diffFields, diffPlacement, parseTags, placementForm } from "./ModDetail";
import { folderProblem, placeTarget, planPlacement, placementProblems } from "../mods/placement";

async function setup(id: string, options: MockModOptions = {}) {
  const source = createMockModSource(options);
  const update = source.update ? vi.spyOn(source, "update") : undefined;
  const setInProfile = source.setInProfile ? vi.spyOn(source, "setInProfile") : undefined;
  const rename = source.rename ? vi.spyOn(source, "rename") : undefined;
  render(<ModBrowser source={source} selectedId={id} />);
  await screen.findByRole("heading", { level: 2 });
  return { update, setInProfile, rename, user: userEvent.setup(), detail: () => within(screen.getByRole("main")) };
}

test("sets a download page and shows the commit", async () => {
  const { update, user, detail } = await setup("acme/widgets-extra");
  expect(detail().getByText("None yet")).toBeInTheDocument();
  await user.click(detail().getByRole("button", { name: "Edit" }));
  await user.type(detail().getByRole("textbox", { name: "Download page" }), "https://example.com/extra");
  await user.click(detail().getByRole("button", { name: "Save" }));

  expect(update).toHaveBeenCalledWith("acme/widgets-extra", { downloadPage: "https://example.com/extra" });
  expect(await detail().findByRole("status")).toHaveTextContent(/^Saved \(commit [0-9a-f]+\)$/);
  expect(detail().getByRole("link", { name: "https://example.com/extra" })).toBeInTheDocument();
  // the list no longer flags it
  const item = screen.getByText("acme/widgets-extra", { selector: ".dui-mods__meta > span" }).closest("button")!;
  expect(item).not.toHaveTextContent("no page");
});

test("sends only what changed; empty fields clear, tags are tidied", async () => {
  const { update, user, detail } = await setup("acme/core");
  await user.click(detail().getByRole("button", { name: "Edit" }));
  await user.clear(detail().getByRole("textbox", { name: "Title" }));
  const tags = detail().getByRole("textbox", { name: "Tags" });
  await user.clear(tags);
  await user.type(tags, "script, library, ,new,script");
  await user.click(detail().getByRole("button", { name: "Save" }));
  expect(update).toHaveBeenCalledWith("acme/core", { title: null, tags: ["library", "new", "script"] });
  expect(await screen.findByRole("heading", { level: 2 })).toHaveTextContent("acme/core");
});

test("an unchanged form saves nothing", async () => {
  const { update, user, detail } = await setup("acme/core");
  await user.click(detail().getByRole("button", { name: "Edit" }));
  await user.click(detail().getByRole("button", { name: "Save" }));
  expect(update).not.toHaveBeenCalled();
  expect(detail().getByRole("status")).toHaveTextContent("No change");
});

test("refuses a download page that isn't a URL", async () => {
  const { update, user, detail } = await setup("acme/widgets-extra");
  await user.click(detail().getByRole("button", { name: "Edit" }));
  // type="url" would block submit in a browser; the check here is the fallback
  const page = detail().getByRole("textbox", { name: "Download page" });
  page.setAttribute("type", "text");
  await user.type(page, "ftp://nope");
  await user.click(detail().getByRole("button", { name: "Save" }));
  expect(detail().getByRole("alert")).toHaveTextContent("must be an http(s) URL");
  expect(update).not.toHaveBeenCalled();
});

test("a failed save keeps the form open with the error", async () => {
  const { user, detail } = await setup("acme/core", { failWrites: "git commit: index.lock exists" });
  await user.click(detail().getByRole("button", { name: "Edit" }));
  await user.type(detail().getByRole("textbox", { name: "Tags" }), ", extra");
  await user.click(detail().getByRole("button", { name: "Save" }));
  expect(await detail().findByRole("alert")).toHaveTextContent("index.lock exists");
  expect(detail().getByRole("button", { name: "Save" })).toBeEnabled();
});

test("removes a mod from the profile and adds it back", async () => {
  const { setInProfile, user, detail } = await setup("acme/widgets");
  await user.click(detail().getByRole("button", { name: "Remove from current" }));
  expect(setInProfile).toHaveBeenLastCalledWith("current", "acme/widgets", false);
  expect(await detail().findByText("Not in current")).toBeInTheDocument();
  expect(detail().getByRole("status")).toHaveTextContent(/Saved \(commit/);

  await user.click(detail().getByRole("button", { name: "Add to current" }));
  expect(setInProfile).toHaveBeenLastCalledWith("current", "acme/widgets", true);
  expect(await detail().findByText("Enabled in current")).toBeInTheDocument();
  await waitFor(() => expect(screen.getByText(/enabled in current$/)).toBeInTheDocument());
});

test("a read-only source shows no edit controls, nor does a Nix-defined mod", async () => {
  const { detail } = await setup("acme/core", { readOnly: true });
  expect(detail().queryByRole("button", { name: "Edit" })).not.toBeInTheDocument();
  expect(detail().queryByRole("button", { name: /from current/ })).not.toBeInTheDocument();
});

test("a Nix-defined mod can be toggled but not edited", async () => {
  const { detail } = await setup("pixel/hair-set");
  expect(detail().queryByRole("button", { name: "Edit" })).not.toBeInTheDocument();
  expect(detail().getByRole("button", { name: "Remove from current" })).toBeInTheDocument();
});

test("helpers", () => {
  expect(parseTags(" b, a,, b ")).toEqual(["a", "b"]);
  const mod = sampleMods[0];
  expect(diffFields(mod, { downloadPage: mod.downloadPage!, title: mod.title!, tags: "script, library" })).toEqual({});
  expect(diffFields(mod, { downloadPage: " ", title: mod.title!, tags: "library,script" })).toEqual({ downloadPage: null });
});

test("moves a mod out of a shared folder into its own, flattened", async () => {
  const { update, user, detail } = await setup("acme/tweak-moodlets");
  expect(detail().getByText("Mods/", { selector: "code" })).toBeInTheDocument();
  await user.click(detail().getByRole("button", { name: "Move…" }));
  expect(detail().getByRole("radio", { name: /Directly in/ })).toBeChecked();
  await user.click(detail().getByRole("radio", { name: /Its own folder/ }));
  // still under Acme/ until flattened
  expect(detail().getByLabelText("Where the files go")).toHaveTextContent("acme_tweak-moodlets/Acme/Acme_TweakMoodlets.package");
  await user.click(detail().getByRole("checkbox", { name: /Flatten/ }));
  expect(detail().getByLabelText("Where the files go")).toHaveTextContent(
    "Acme/Acme_TweakMoodlets.package → acme_tweak-moodlets/Acme_TweakMoodlets.package",
  );
  await user.click(detail().getByRole("button", { name: "Save" }));

  expect(update).toHaveBeenCalledWith("acme/tweak-moodlets", { folder: "default", flatten: true });
  expect(await detail().findByRole("status")).toHaveTextContent(/Saved \(commit [0-9a-f]+\) The files move at the next `sims switch`/);
  await waitFor(() => expect(detail().getByText("Mods/acme_tweak-moodlets/", { selector: "code" })).toBeInTheDocument());
});

test("a custom folder: checked as typed, and the game's depth limits block saving", async () => {
  const { update, user, detail } = await setup("acme/core");
  await user.click(detail().getByRole("button", { name: "Move…" }));
  await user.click(detail().getByRole("radio", { name: "Another folder" }));
  const folder = detail().getByRole("textbox", { name: "Folder under Mods/" });
  await user.type(folder, "../up");
  expect(detail().getByRole("alert")).toHaveTextContent("without . or .. parts");
  expect(detail().getByRole("button", { name: "Save" })).toBeDisabled();
  await user.clear(folder);
  await user.type(folder, "Libraries/Acme");
  // the script would be two folders deep
  expect(detail().getByRole("alert")).toHaveTextContent(".ts4script files load at most one folder below Mods/");
  expect(detail().getByText(/It writes state files/)).toBeInTheDocument();
  await user.clear(folder);
  await user.type(folder, "Acme Libraries/");
  expect(detail().queryByRole("alert")).not.toBeInTheDocument();
  await user.click(detail().getByRole("button", { name: "Save" }));
  expect(update).toHaveBeenCalledWith("acme/core", { folder: "Acme Libraries" });
});

test("renames a mod after a preview and shows it under its new id", async () => {
  const { rename, user, detail } = await setup("acme/widgets");
  await user.click(detail().getByRole("button", { name: "Rename…" }));
  const id = detail().getByRole("textbox", { name: /New id/ });
  await user.clear(id);
  await user.type(id, "Acme/Bad Id");
  await user.click(detail().getByRole("button", { name: "Preview" }));
  expect(detail().getByRole("alert")).toHaveTextContent("lowercase letters");
  expect(rename).not.toHaveBeenCalled();

  await user.clear(id);
  await user.type(id, "acme/gadgets");
  await user.click(detail().getByRole("button", { name: "Preview" }));
  expect(rename).toHaveBeenLastCalledWith("acme/widgets", "acme/gadgets", false);
  const plan = await detail().findByLabelText("What the rename changes");
  expect(plan).toHaveTextContent("Installs to Mods/acme_gadgets/ instead of Mods/acme_widgets/");
  expect(plan).toHaveTextContent("mods/acme/widgets-extra.json");

  await user.click(detail().getByRole("button", { name: "Rename" }));
  expect(rename).toHaveBeenLastCalledWith("acme/widgets", "acme/gadgets", true);
  expect(await screen.findByText("acme/gadgets", { selector: ".dui-mod__id" })).toBeInTheDocument();
  expect(detail().getByRole("status")).toHaveTextContent(/Renamed from acme\/widgets \(commit/);
});

test("editing the id resets the rename preview", async () => {
  const { rename, user, detail } = await setup("acme/widgets");
  await user.click(detail().getByRole("button", { name: "Rename…" }));
  const id = detail().getByRole("textbox", { name: /New id/ });
  await user.clear(id);
  await user.type(id, "acme/gadgets");
  await user.click(detail().getByRole("button", { name: "Preview" }));
  await detail().findByRole("button", { name: "Rename" });
  await user.type(id, "-2");
  expect(detail().getByRole("button", { name: "Preview" })).toBeInTheDocument();
  expect(rename).toHaveBeenCalledTimes(1);
});

test("placement helpers", () => {
  expect(placeTarget("A/b/c.package", { folder: "x", flatten: false })).toBe("x/A/b/c.package");
  expect(placeTarget("A/b/c.package", { folder: "x", flatten: true })).toBe("x/c.package");
  expect(placeTarget("A/c.package", { folder: null, flatten: true })).toBe("c.package");
  // one file imported twice installs once when flattened; different files on one path clash
  const installs = [
    { source: "a/one.package", target: "m/a/one.package", sha256: "1" },
    { source: "b/one.package", target: "m/b/one.package", sha256: "1" },
    { source: "c/one.package", target: "m/c/one.package", sha256: "2" },
  ];
  const planned = planPlacement(installs, { folder: "m", flatten: true });
  expect(planned).toHaveLength(2);
  expect(placementProblems(planned)).toEqual([expect.stringContaining("2 different files would install here")]);
  expect(folderProblem(" ok/folder/ ")).toBeNull();
  expect(folderProblem("/abs")).not.toBeNull();
  expect(folderProblem("a//b")).not.toBeNull();

  const mod = sampleMods.find((m) => m.id === "zed/loose")!;
  expect(placementForm(mod)).toEqual({ place: "root", customFolder: "", flatten: false });
  expect(diffPlacement(mod, { place: "root", customFolder: "", flatten: false })).toEqual({});
  expect(diffPlacement(mod, { place: "custom", customFolder: " Zed/ ", flatten: false })).toEqual({ folder: "Zed" });
  const core = sampleMods[0];
  expect(diffPlacement(core, { place: "root", customFolder: "", flatten: true })).toEqual({ folder: "", flatten: true });
});

test("lists an archive's files and, for the installed version, where each goes", async () => {
  const { user, detail } = await setup("acme/core");
  const row = (label: string) => detail().getByRole("rowheader", { name: new RegExp(`^${label.replace(/\./g, "\\.")}`) }).closest("tr")!;

  const installed = within(row("2.4.1"));
  expect(installed.getByText("4 files inside")).toBeInTheDocument();
  expect(installed.getByText("AcmeCore.package").closest("li")).toHaveTextContent(
    "AcmeCore.package → Mods/acme_core/AcmeCore.package",
  );
  expect(installed.getByText("Readme.txt").closest("li")).toHaveTextContent("Readme.txt (not installed)");

  // an older version lists its files without destinations
  const older = within(row("2.3.0"));
  expect(older.getByText("Readme.txt").closest("li")).toHaveTextContent(/^Readme\.txt$/);

  // a move shows the new destinations
  await user.click(detail().getByRole("button", { name: "Move…" }));
  await user.click(detail().getByRole("radio", { name: /Directly in/ }));
  await user.click(detail().getByRole("button", { name: "Save" }));
  await detail().findByRole("status");
  expect(within(row("2.4.1")).getByText("AcmeCore.package").closest("li")).toHaveTextContent("→ Mods/AcmeCore.package");
});
