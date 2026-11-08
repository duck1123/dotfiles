import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { createMockModSource, type MockModOptions } from "../mods/mockModSource";
import { sampleMods } from "../mods/sampleMods";
import { ModBrowser } from "./ModBrowser";
import { diffFields, parseTags } from "./ModDetail";

async function setup(id: string, options: MockModOptions = {}) {
  const source = createMockModSource(options);
  const update = source.update ? vi.spyOn(source, "update") : undefined;
  const setInProfile = source.setInProfile ? vi.spyOn(source, "setInProfile") : undefined;
  render(<ModBrowser source={source} selectedId={id} />);
  await screen.findByRole("heading", { level: 2 });
  return { update, setInProfile, user: userEvent.setup(), detail: () => within(screen.getByRole("main")) };
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
