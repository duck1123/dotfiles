import type { Meta, StoryObj } from "@storybook/react-vite";
import { sampleNotes } from "../notes/mockSource";
import { NoteChip } from "./NoteChip";

const meta = {
  title: "Notes/NoteChip",
  component: NoteChip,
  args: {
    note: sampleNotes[6],
    href: "#mock/note",
  },
} satisfies Meta<typeof NoteChip>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Linked: Story = {};

export const Removable: Story = {
  args: { onRemove: () => {} },
};

export const WithIcon: Story = {
  args: { note: sampleNotes[4], onRemove: () => {} },
};

export const PlainLabel: Story = {
  args: { href: undefined },
};

export const LongTitle: Story = {
  args: { note: sampleNotes[12], onRemove: () => {} },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: 280 }}>
        <Story />
      </div>
    ),
  ],
};
