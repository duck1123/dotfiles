import type { Meta, StoryObj } from "@storybook/react-vite";
import { createMockMediaSource } from "../media/mockMediaSource";
import { MediaReview } from "./MediaReview";

const meta = {
  title: "Media/MediaReview",
  component: MediaReview,
  parameters: { layout: "fullscreen" },
  args: { source: createMockMediaSource({ latencyMs: 200 }) },
  argTypes: { source: { control: false } },
  decorators: [
    (Story) => (
      <div style={{ padding: 16 }}>
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof MediaReview>;

export default meta;
type Story = StoryObj<typeof meta>;

/** The whole page on mock data. A decision moves the file to another view. Reload the story to start over. */
export const Default: Story = {};

export const LoadError: Story = {
  args: { source: createMockMediaSource({ latencyMs: 200, failList: "Windmill job failed (502)" }) },
};

export const AllDone: Story = {
  args: { source: createMockMediaSource({ files: [] }) },
};

export const SaveFails: Story = {
  args: { source: createMockMediaSource({ latencyMs: 200, failDecide: "media_file 1 can't take that decision now" }) },
};
