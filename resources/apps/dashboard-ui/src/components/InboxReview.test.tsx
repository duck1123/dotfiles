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
