import type { Meta, StoryObj } from "@storybook/react-vite";
import { createMockInboxSource } from "../inbox/mockInboxSource";
import { CaptureBox } from "./CaptureBox";

const meta = {
  title: "Inbox/CaptureBox",
  component: CaptureBox,
  args: {
    onCapture: (text) => createMockInboxSource({ items: [], latencyMs: 400 }).capture!(text),
  },
  argTypes: {
    onCapture: { control: false },
  },
  decorators: [
    (Story) => (
      <div style={{ maxWidth: 340 }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof CaptureBox>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Enter sends, Shift+Enter starts a new line. Try "task: water the plants" or a URL. */
export const Default: Story = {};

export const CaptureFails: Story = {
  args: {
    onCapture: (text) => createMockInboxSource({ latencyMs: 400, failCapture: "Windmill job failed (502)" }).capture!(text),
  },
};
