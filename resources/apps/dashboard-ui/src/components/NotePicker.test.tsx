import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { useState } from "react";
import { createMockSource } from "../notes/mockSource";
import type { NoteSource } from "../notes/types";
import { NotePicker } from "./NotePicker";

const scope = { query: "test", noun: "topic" };

function Harness({ source, initial = [], multi }: { source: NoteSource; initial?: string[]; multi?: boolean }) {
  const [value, setValue] = useState(initial);
  return (
    <>
      <NotePicker source={source} scope={scope} value={value} onChange={setValue} multi={multi} debounceMs={0} />
      <div data-testid="value">{value.join(",")}</div>
    </>
  );
}

const value = () => screen.getByTestId("value").textContent;

test("searches as you type and picks with the keyboard", async () => {
  const user = userEvent.setup();
  render(<Harness source={createMockSource()} />);

  await user.type(screen.getByRole("combobox"), "rust");
  const options = await screen.findAllByRole("option");
  expect(options).toHaveLength(2);
  expect(options[1]).toHaveTextContent("Science › Chemistry");

  await user.keyboard("{ArrowDown}{Enter}");
  expect(value()).toBe("mockRustCorr");
  expect(screen.getByRole("combobox")).toHaveValue("");
});

test("leaves picked notes out of the suggestions", async () => {
  const user = userEvent.setup();
  render(<Harness source={createMockSource()} initial={["mockBaking01"]} />);

  await user.type(screen.getByRole("combobox"), "ing");
  const options = await screen.findAllByRole("option");
  const titles = options.map((option) => option.textContent);
  expect(titles.some((t) => t?.startsWith("Gardening"))).toBe(true);
  expect(titles.some((t) => t?.startsWith("Baking"))).toBe(false);
});

test("shows chips with titles and removes them", async () => {
  const user = userEvent.setup();
  render(<Harness source={createMockSource()} initial={["mockSourdgh1", "mockGarden01"]} />);

  expect(await screen.findByRole("link", { name: "Sourdough" })).toHaveAttribute("href", "#mock/mockSourdgh1");
  await user.click(screen.getByRole("button", { name: "Remove Sourdough" }));
  expect(value()).toBe("mockGarden01");

  await user.click(screen.getByRole("combobox"));
  await user.keyboard("{Backspace}");
  expect(value()).toBe("");
});

test("says when nothing matches", async () => {
  const user = userEvent.setup();
  render(<Harness source={createMockSource()} />);

  await user.type(screen.getByRole("combobox"), "zzz");
  expect(await screen.findByText("No topics match “zzz”")).toHaveAttribute("role", "status");
});

test("shows a search error and retries", async () => {
  const user = userEvent.setup();
  let fail = true;
  const base = createMockSource();
  const source: NoteSource = {
    ...base,
    search: (...args) => (fail ? Promise.reject(new Error("boom")) : base.search(...args)),
  };
  render(<Harness source={source} />);

  await user.type(screen.getByRole("combobox"), "rust");
  expect(await screen.findByRole("alert")).toHaveTextContent("Search failed: boom");

  fail = false;
  await user.click(screen.getByRole("button", { name: "Retry" }));
  await waitFor(() => expect(screen.getAllByRole("option")).toHaveLength(2));
});

test("ignores a slow search that finishes after newer text", async () => {
  const user = userEvent.setup();
  const base = createMockSource();
  const pending: Array<() => void> = [];
  // Resolves only when told to, and never looks at the abort signal.
  const source: NoteSource = {
    ...base,
    search: (text, scope) =>
      new Promise((resolve) => pending.push(() => base.search(text, scope).then(resolve))),
  };
  render(<Harness source={source} />);

  const input = screen.getByRole("combobox");
  await user.type(input, "r");
  await waitFor(() => expect(pending).toHaveLength(1));
  await user.type(input, "ust");
  await waitFor(() => expect(pending.length).toBeGreaterThan(1));

  pending[pending.length - 1]();
  await waitFor(() => expect(screen.getAllByRole("option")).toHaveLength(2));
  pending[0]();
  await new Promise((r) => setTimeout(r, 20));
  expect(screen.getAllByRole("option")).toHaveLength(2);
});

test("single mode replaces the picked note", async () => {
  const user = userEvent.setup();
  render(<Harness source={createMockSource()} initial={["mockTomato01"]} multi={false} />);

  await user.type(screen.getByRole("combobox"), "origami");
  await user.click(await screen.findByRole("option"));
  expect(value()).toBe("mockOrigami1");
});
