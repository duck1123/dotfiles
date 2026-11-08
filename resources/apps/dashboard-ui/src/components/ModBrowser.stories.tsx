import type { Meta, StoryObj } from "@storybook/react-vite";
import { fn } from "storybook/test";
import { createMockModSource } from "../mods/mockModSource";
import { sampleMods } from "../mods/sampleMods";
import { ModBrowser } from "./ModBrowser";

const meta = {
  title: "Mods/ModBrowser",
  component: ModBrowser,
  parameters: { layout: "fullscreen" },
  decorators: [
    (Story) => (
      <div style={{ padding: 16 }}>
        <Story />
      </div>
    ),
  ],
  args: {
    source: createMockModSource(),
    selectedId: sampleMods[0].id,
    onSelect: fn(),
  },
} satisfies Meta<typeof ModBrowser>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {};

export const WithProblem: Story = {
  args: { selectedId: "nimbus/weather" },
};

export const NothingSelected: Story = {
  args: { selectedId: undefined },
};

export const Loading: Story = {
  args: { source: createMockModSource({ delay: 1e9 }) },
};

export const LoadError: Story = {
  args: { source: createMockModSource({ fail: "nix eval failed: simsnix: 2 problem(s)" }) },
};

export const EmptyCatalog: Story = {
  args: {
    source: createMockModSource({ list: { profile: "current", profiles: ["current"], problems: [], mods: [] } }),
    selectedId: undefined,
  },
};
