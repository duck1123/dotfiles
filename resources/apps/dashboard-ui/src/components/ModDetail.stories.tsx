import type { Meta, StoryObj } from "@storybook/react-vite";
import { fn } from "storybook/test";
import { sampleMods } from "../mods/sampleMods";
import { ModDetail } from "./ModDetail";

const byId = (id: string) => sampleMods.find((m) => m.id === id)!;
const knownIds = new Set(sampleMods.map((m) => m.id));

const meta = {
  title: "Mods/ModDetail",
  component: ModDetail,
  decorators: [
    (Story) => (
      <div style={{ maxWidth: 720 }}>
        <Story />
      </div>
    ),
  ],
  args: { mod: byId("acme/core"), profile: "current", knownIds, onSelect: fn() },
} satisfies Meta<typeof ModDetail>;

export default meta;
type Story = StoryObj<typeof meta>;

export const SeveralVersions: Story = {};

export const Requires: Story = {
  args: { mod: byId("acme/widgets") },
};

export const NotEnabled: Story = {
  args: { mod: byId("acme/widgets-extra") },
};

export const PinnedLooseFiles: Story = {
  args: { mod: byId("zed/loose") },
};

export const ProblemAndLongTitle: Story = {
  args: { mod: byId("nimbus/weather") },
};

export const DefinedInNix: Story = {
  args: { mod: byId("pixel/hair-set") },
};

export const MissingDependency: Story = {
  args: { mod: { ...byId("acme/widgets"), requires: ["acme/core", "gone/away"] } },
};

export const InSharedFolder: Story = {
  args: {
    mod: byId("acme/tweak-moodlets"),
    onSave: fn(async () => ({ commit: "abc1234", notes: ["the files move at the next `sims switch`"] })),
  },
};
