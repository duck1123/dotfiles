import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { createMockInboxSource } from "../inbox/mockInboxSource";
import { sampleItems } from "../inbox/sampleItems";
import type { InboxSource } from "../inbox/types";
import { createMockSource } from "../notes/mockSource";
import { InboxReview } from "./InboxReview";

const topics = { query: "test", noun: "topic" };

function setup(inbox: InboxSource = createMockInboxSource()) {
  const save = vi.spyOn(inbox, "save");
  render(<InboxReview inbox={inbox} notes={createMockSource()} topicScope={topics} />);
  return { save, user: userEvent.setup() };
}

const queueItems = () => within(screen.getByRole("navigation", { name: "Inbox" })).getAllByRole("button");
const titleField = () => screen.getByRole("textbox", { name: "Title" });

test("loads the queue and selects the first item", async () => {
  setup();
  await waitFor(() => expect(queueItems()).toHaveLength(sampleItems.length));
  expect(queueItems()[0]).toHaveAttribute("aria-current", "true");
  expect(titleField()).toHaveValue(sampleItems[0].title);
  expect(await screen.findByText(sampleItems[0].preview!)).toBeInTheDocument();
});

test("picks a topic, marks reviewed and moves to the next item", async () => {
  const { save, user } = setup();
  await screen.findByDisplayValue(sampleItems[0].title);

  await user.type(screen.getByRole("combobox", { name: "Topics" }), "sourd");
  await user.click(await screen.findByRole("option", { name: /Sourdough/ }));
  await user.click(screen.getByRole("button", { name: "Mark reviewed" }));

  expect(save).toHaveBeenCalledWith(sampleItems[0].noteId, {
    title: sampleItems[0].title,
    kind: "link",
    topicIds: ["mockSourdgh1"],
    done: true,
  });
  await waitFor(() => expect(queueItems()).toHaveLength(sampleItems.length - 1));
  expect(titleField()).toHaveValue(sampleItems[1].title);
  expect(screen.getByText("1 reviewed this session")).toBeInTheDocument();
});

test("Enter in the title doesn't mark reviewed, Ctrl+Enter does", async () => {
  const { save, user } = setup();
  await screen.findByDisplayValue(sampleItems[0].title);

  await user.clear(titleField());
  await user.type(titleField(), "Keeping a starter alive{Enter}");
  expect(save).not.toHaveBeenCalled();

  await user.keyboard("{Control>}{Enter}{/Control}");
  await waitFor(() => expect(save).toHaveBeenCalledWith(sampleItems[0].noteId, expect.objectContaining({ title: "Keeping a starter alive", done: true })));
});

test("Save keeps the item in the queue", async () => {
  const { save, user } = setup();
  await screen.findByDisplayValue(sampleItems[0].title);

  await user.selectOptions(screen.getByRole("combobox", { name: "Kind" }), "journal");
  await user.click(screen.getByRole("button", { name: "Save" }));

  await waitFor(() => expect(save).toHaveBeenCalledWith(sampleItems[0].noteId, expect.objectContaining({ kind: "journal", done: false })));
  expect(queueItems()).toHaveLength(sampleItems.length);
  expect(within(queueItems()[0]).getByText("journal")).toBeInTheDocument();
});

test("a failed save shows the error and keeps the edits", async () => {
  const { user } = setup(createMockInboxSource({ failSave: "boom" }));
  await screen.findByDisplayValue(sampleItems[0].title);

  await user.clear(titleField());
  await user.type(titleField(), "Edited");
  await user.click(screen.getByRole("button", { name: "Mark reviewed" }));

  expect(await screen.findByRole("alert")).toHaveTextContent("Couldn't save: boom");
  expect(titleField()).toHaveValue("Edited");
  expect(queueItems()).toHaveLength(sampleItems.length);
});

test("older ~tag relations can be copied to topics", async () => {
  const { save, user } = setup();
  await screen.findByDisplayValue(sampleItems[0].title);
  await user.click(queueItems()[2]);

  await user.click(await screen.findByRole("button", { name: "Copy to topics" }));
  await user.click(screen.getByRole("button", { name: "Mark reviewed" }));
  await waitFor(() => expect(save).toHaveBeenCalledWith(sampleItems[2].noteId, expect.objectContaining({ topicIds: ["mockTomato01"] })));
});

test("says so when the queue is empty", async () => {
  setup(createMockInboxSource({ items: [] }));
  expect(await screen.findByText("Nothing left to review.")).toBeInTheDocument();
});

test("focuses the topic search for each new item, ready for typing", async () => {
  const { user } = setup();
  const topicsField = () => screen.getByRole("combobox", { name: "Topics" });
  await screen.findByDisplayValue(sampleItems[0].title);
  expect(topicsField()).toHaveFocus();

  await user.keyboard("{Control>}{Enter}{/Control}");
  await screen.findByDisplayValue(sampleItems[1].title);
  expect(topicsField()).toHaveFocus();

  // Typing goes straight into the search.
  await user.keyboard("sourd");
  expect(await screen.findByRole("option", { name: /Sourdough/ })).toBeInTheDocument();

  await user.click(queueItems()[2]);
  await screen.findByDisplayValue(sampleItems[3].title);
  expect(topicsField()).toHaveFocus();
});

test("a capture joins the end of the queue without taking the selection", async () => {
  const { user } = setup();
  await screen.findByDisplayValue(sampleItems[0].title);

  await user.click(screen.getByRole("textbox", { name: "Capture" }));
  await user.keyboard("task: water the plants{Enter}");

  expect(await screen.findByRole("status")).toHaveTextContent("Captured task water the plants");
  expect(screen.getByRole("textbox", { name: "Capture" })).toHaveValue("");
  expect(queueItems()).toHaveLength(sampleItems.length + 1);
  expect(within(queueItems()[sampleItems.length]).getByText("water the plants")).toBeInTheDocument();
  expect(titleField()).toHaveValue(sampleItems[0].title);
});

test("with focusCapture, the capture box keeps the focus and the first capture is selected", async () => {
  const user = userEvent.setup();
  render(<InboxReview inbox={createMockInboxSource({ items: [] })} notes={createMockSource()} topicScope={topics} focusCapture />);
  const box = screen.getByRole("textbox", { name: "Capture" });
  expect(box).toHaveFocus();
  await screen.findByText("Nothing left to review.");

  await user.keyboard("first line{Shift>}{Enter}{/Shift}second line{Enter}");
  expect(await screen.findByDisplayValue("first line")).toBeInTheDocument();
  expect(box).toHaveFocus();
});

test("a failed capture shows the error and keeps the text", async () => {
  const { user } = setup(createMockInboxSource({ failCapture: "boom" }));
  await screen.findByDisplayValue(sampleItems[0].title);

  await user.click(screen.getByRole("textbox", { name: "Capture" }));
  await user.keyboard("a thought{Enter}");
  expect(await screen.findByText("Couldn't capture: boom")).toBeInTheDocument();
  expect(screen.getByRole("textbox", { name: "Capture" })).toHaveValue("a thought");
});

test("no capture box when the source can't capture", async () => {
  const { capture: _capture, ...inbox } = createMockInboxSource();
  setup(inbox);
  await screen.findByDisplayValue(sampleItems[0].title);
  expect(screen.queryByRole("textbox", { name: "Capture" })).not.toBeInTheDocument();
});
