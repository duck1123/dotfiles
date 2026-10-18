import type { Meta, StoryObj } from "@storybook/react-vite";
import { useState } from "react";
import { fn } from "storybook/test";
import { createMockSource, sampleNotes } from "../notes/mockSource";
import type { NoteScope } from "../notes/types";
import { NotePicker, type NotePickerProps } from "./NotePicker";

const topics: NoteScope = { query: "note.ancestors.noteId = mockTopics", noun: "topic" };
const source = createMockSource({ latencyMs: 250 });

// Keeps the picked IDs in state, so the stories behave like a real form, and
// still reports each change to the Actions panel.
function Stateful(props: NotePickerProps) {
  const [value, setValue] = useState(props.value);
  return (
    <div style={{ maxWidth: 480, minHeight: 360 }}>
      <NotePicker
        {...props}
        value={value}
        onChange={(ids) => {
          setValue(ids);
          props.onChange(ids);
        }}
      />
    </div>
  );
}

const meta = {
  title: "Notes/NotePicker",
  component: NotePicker,
  render: (args) => <Stateful {...args} />,
  args: {
    source,
    scope: topics,
    value: [],
    onChange: fn(),
    label: "Topics",
  },
  argTypes: {
    source: { control: false },
    scope: { control: false },
  },
} satisfies Meta<typeof NotePicker>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Nothing picked yet. Focus the field and type, e.g. "rust" or "ba". */
export const Empty: Story = {};

export const WithPicked: Story = {
  args: { value: ["mockSourdgh1", "mockGarden01"] },
};

/** Typing "rust": two topics share the title, told apart by their paths. */
export const SameTitle: Story = {
  args: { defaultText: "rust", defaultOpen: true },
};

export const Loading: Story = {
  args: {
    source: createMockSource({ latencyMs: 1e9 }),
    defaultText: "garden",
    defaultOpen: true,
  },
};

export const NoResults: Story = {
  args: { defaultText: "zzz", defaultOpen: true },
};

export const SearchError: Story = {
  args: {
    source: createMockSource({ latencyMs: 250, failWith: "Windmill job failed (502)" }),
    defaultText: "rust",
    defaultOpen: true,
  },
};

/** Already-picked notes are left out of the suggestions. */
export const PickedAreHidden: Story = {
  args: { value: ["mockBaking01"], defaultText: "ing", defaultOpen: true },
};

export const ManyPicked: Story = {
  args: { value: sampleNotes.slice(0, 11).map((note) => note.noteId) },
};

export const LongTitles: Story = {
  args: { value: ["mockLongT001"], defaultText: "deliberately", defaultOpen: true },
};

/** Exactly one note; picking another replaces it. */
export const Single: Story = {
  args: { multi: false, value: ["mockTomato01"], label: "Parent topic" },
};

/** A picked ID the source doesn't know shows as its ID. */
export const UnknownId: Story = {
  args: { value: ["mockGarden01", "doesNotExist"] },
};
