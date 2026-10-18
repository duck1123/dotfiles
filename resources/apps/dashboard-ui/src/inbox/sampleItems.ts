import type { InboxItem } from "./types";

// Made-up queue items for stories and tests (public repo: nothing real).
export const sampleItems: InboxItem[] = [
  {
    noteId: "mockInbox001",
    title: "How to keep a sourdough starter alive on vacation",
    kind: "link",
    created: "2026-10-12T09:14:00",
    url: "https://example.com/blog/sourdough-starter-vacation",
    topicIds: [],
    tagIds: [],
    preview: "A short guide to drying, freezing or feeding a starter less often while you're away.",
  },
  {
    noteId: "mockInbox002",
    title: "Building a hand-wired 40% keyboard",
    kind: "video",
    created: "2026-10-13T21:40:00",
    url: "https://example.com/watch?v=abc123",
    topicIds: ["mockKeybrd01"],
    tagIds: [],
    preview: "Channel: Example Maker. Wiring the matrix, flashing firmware, and choosing switches.",
  },
  {
    noteId: "mockInbox003",
    title: "renew the tomato cage order",
    kind: "task",
    created: "2026-10-15",
    topicIds: [],
    tagIds: ["mockTomato01"],
  },
  {
    noteId: "mockInbox004",
    title: "Thoughts after the garden club meeting, which ran long and covered far more than the agenda said it would",
    kind: "journal",
    created: "2026-10-16T19:02:00",
    topicIds: ["mockGarden01", "mockHobby001"],
    tagIds: [],
    preview: "We talked about seed swaps, the spring plant sale, and whether to move the meeting night.",
  },
  {
    noteId: "mockInbox005",
    title: "https://example.org/a/very/long/url/that/was/never/given/a/title?ref=share",
    kind: "link",
    created: "2026-10-17T07:30:00",
    url: "https://1abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstu.example.org/a/very/long/url/that/was/never/given/a/title?ref=share",
    topicIds: [],
    tagIds: [],
  },
];
