import type { Meta, StoryObj } from "@storybook/react-vite";
import { useState } from "react";
import { fn } from "storybook/test";
import { sampleItems } from "../inbox/sampleItems";
import { ReviewQueue, type ReviewQueueProps } from "./ReviewQueue";

function Stateful(props: ReviewQueueProps) {
  const [selectedId, setSelectedId] = useState(props.selectedId);
  return (
    <div style={{ maxWidth: 340 }}>
      <ReviewQueue
        {...props}
        selectedId={selectedId}
        onSelect={(id) => {
          setSelectedId(id);
          props.onSelect(id);
        }}
      />
    </div>
  );
}

const meta = {
  title: "Inbox/ReviewQueue",
  component: ReviewQueue,
  render: (args) => <Stateful {...args} />,
  args: {
    items: sampleItems,
    selectedId: sampleItems[0].noteId,
    onSelect: fn(),
    onRetry: fn(),
  },
} satisfies Meta<typeof ReviewQueue>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {};

export const Loading: Story = {
  args: { status: "loading" },
};

export const LoadError: Story = {
  args: { status: "error", error: "Windmill job failed (502)" },
};

export const Empty: Story = {
  args: { items: [] },
};
