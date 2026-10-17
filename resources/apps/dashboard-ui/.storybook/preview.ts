import type { Preview } from "@storybook/react-vite";
import "../src/theme.css";
import "./preview.css";

const preview: Preview = {
  parameters: {
    layout: "padded",
  },
};

export default preview;
