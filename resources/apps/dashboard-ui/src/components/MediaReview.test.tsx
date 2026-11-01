import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { createMockMediaSource } from "../media/mockMediaSource";
import { sampleFiles } from "../media/sampleFiles";
import type { MediaSource } from "../media/types";
import { MediaReview, spotWindow } from "./MediaReview";

function setup(source: MediaSource = createMockMediaSource()) {
  const decide = vi.spyOn(source, "decide");
  render(<MediaReview source={source} />);
  return { decide, user: userEvent.setup() };
}

const fileButtons = () => within(screen.getByRole("navigation", { name: "Flagged files" })).getAllByRole("button");

test("lists the flagged files grouped by show and selects the first", async () => {
  setup();
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length));
  expect(fileButtons()[0]).toHaveAttribute("aria-current", "true");
  expect(screen.getByRole("heading", { name: /Better Off Ted\s*2/ })).toBeInTheDocument();
  expect(screen.getByRole("heading", { level: 2 })).toHaveTextContent("X-Men '97 - S01E10");
  expect(screen.getByText("About 19:19–19:49")).toBeInTheDocument();
  expect(screen.getByText(/hole in the file, video decode errors/)).toBeInTheDocument();
});

test("the range covers where a real hole was, past the logged position", () => {
  expect(spotWindow({ t: 1159.392, lines: 2, rules: [] }, 2404)).toBe("About 19:19–19:49");
  expect(spotWindow({ t: 1292.608, lines: 4, rules: [] }, 1300)).toBe("About 21:32–21:40");
  expect(spotWindow({ t: 4000, lines: 1, rules: [] }, 7000)).toBe("About 1:06:40–1:07:10");
  expect(spotWindow({ t: null, lines: 1, rules: [] }, 7000)).toBe("Position unknown");
});

test("shows how far a truncated file gets", async () => {
  const { user } = setup();
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length));
  await user.click(screen.getByRole("button", { name: /S02E06/ }));
  expect(await screen.findByRole("heading", { level: 2, name: /S02E06/ })).toBeInTheDocument();
  expect(within(screen.getByRole("main")).getByText("Stops at 13:11 of 21:57")).toBeInTheDocument();
});

test("a replacement request moves on to the next file and shows up in its own view", async () => {
  const { decide, user } = setup();
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length));

  await user.click(screen.getByRole("button", { name: "Request replacement" }));
  expect(decide).toHaveBeenCalledWith(sampleFiles[0].id, "replace");
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length - 1));
  expect(screen.getByRole("heading", { level: 2 })).toHaveTextContent("S02E06");

  await user.click(screen.getByRole("button", { name: "Replacement requested" }));
  await waitFor(() => expect(fileButtons()).toHaveLength(1));
  expect(fileButtons()[0]).toHaveTextContent("S01E10");
});

test("a decision can be undone", async () => {
  const { decide, user } = setup();
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length));
  await user.click(screen.getByRole("button", { name: "Mark good" }));
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length - 1));

  await user.click(screen.getByRole("button", { name: "Marked good" }));
  await user.click(await screen.findByRole("button", { name: "Back to review" }));
  expect(decide).toHaveBeenLastCalledWith(sampleFiles[0].id, "reopen");
  expect(await screen.findByText("Nothing here.")).toBeInTheDocument();

  await user.click(screen.getByRole("button", { name: "To review" }));
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length));
});

test("a failed save keeps the file and says why", async () => {
  const { user } = setup(createMockMediaSource({ failDecide: "database is down" }));
  await waitFor(() => expect(fileButtons()).toHaveLength(sampleFiles.length));
  await user.click(screen.getByRole("button", { name: "Mark good" }));
  expect(await screen.findByRole("alert")).toHaveTextContent("database is down");
  expect(fileButtons()).toHaveLength(sampleFiles.length);
});

test("a load failure can be retried", async () => {
  setup(createMockMediaSource({ failList: "Windmill job failed (502)" }));
  expect(await screen.findByRole("alert")).toHaveTextContent("Windmill job failed (502)");
  expect(screen.getByRole("button", { name: "Retry" })).toBeInTheDocument();
});
