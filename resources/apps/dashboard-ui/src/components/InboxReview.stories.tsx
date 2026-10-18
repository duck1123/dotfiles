import type { Meta, StoryObj } from "@storybook/react-vite";
import { createMockInboxSource } from "../inbox/mockInboxSource";
import { createMockSource } from "../notes/mockSource";
import { InboxReview } from "./InboxReview";

const topics = { query: "note.ancestors.noteId = mockTopics", noun: "topic" };

const meta = {
  title: "Inbox/InboxReview",
  component: InboxReview,
  parameters: { layout: "fullscreen" },
  args: {
    inbox: createMockInboxSource({ latencyMs: 300 }),
    notes: createMockSource({ latencyMs: 200 }),
    topicScope: topics,
  },
  argTypes: {
    inbox: { control: false },
    notes: { control: false },
    topicScope: { control: false },
  },
  decorators: [
    (Story) => (
      <div style={{ padding: 16 }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof InboxReview>;

export default meta;
type Story = StoryObj<typeof meta>;

/**
 * The whole page on mock data. Saving changes the mock queue: "Mark reviewed"
 * removes the item and moves to the next. Reload the story to start over.
 */
export const Default: Story = {};

export const LoadError: Story = {
  args: { inbox: createMockInboxSource({ latencyMs: 300, failList: "Windmill job failed (502)" }) },
};

export const AllDone: Story = {
  args: { inbox: createMockInboxSource({ items: [] }) },
};

export const SaveFails: Story = {
  args: { inbox: createMockInboxSource({ latencyMs: 300, failSave: "ETAPI DELETE /attributes/abc: 404" }) },
};
