import type { Meta, StoryObj } from "@storybook/react-vite";
import { fn } from "storybook/test";
import { sampleItems } from "../inbox/sampleItems";
import { createMockSource } from "../notes/mockSource";
import { ReviewItem } from "./ReviewItem";

const topics = { query: "note.ancestors.noteId = mockTopics", noun: "topic" };
const save = (ms: number) => fn(() => new Promise<void>((resolve) => setTimeout(resolve, ms)));

const meta = {
  title: "Inbox/ReviewItem",
  component: ReviewItem,
  args: {
    item: sampleItems[0],
    source: createMockSource({ latencyMs: 200 }),
    topicScope: topics,
    onSave: save(600),
  },
  argTypes: {
    source: { control: false },
    topicScope: { control: false },
  },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: 720, minHeight: 480 }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof ReviewItem>;

export default meta;
type Story = StoryObj<typeof meta>;

/** A link with no topics yet. */
export const Link: Story = {};

/** A video with the reviewer's own notes below the generated description. */
export const VideoWithTopic: Story = {
  args: { item: sampleItems[1] },
};

/** As shown from the queue, before the item's preview and notes have loaded. */
export const NotesLoading: Story = {
  args: { item: { ...sampleItems[0], preview: undefined, notes: undefined } },
};

/** An item with an older ~tag, which can be copied over as a topic. */
export const WithOlderTags: Story = {
  args: { item: sampleItems[2] },
};

export const LongTitle: Story = {
  args: { item: sampleItems[3] },
};

/** A link whose title is still its URL. */
export const UntitledLink: Story = {
  args: { item: sampleItems[4] },
};

/** An unknown kind is kept as an option rather than silently changed. */
export const UnknownKind: Story = {
  args: { item: { ...sampleItems[0], kind: "recipe" } },
};

export const SaveFails: Story = {
  args: {
    onSave: fn(() => new Promise<void>((_, reject) => setTimeout(() => reject(new Error("ETAPI POST /attributes: 500")), 400))),
  },
};
