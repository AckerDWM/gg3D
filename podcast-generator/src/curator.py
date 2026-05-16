"""Use Claude to select and rank the most podcast-worthy stories."""

import json
import anthropic

from .sources import Story


_CURATOR_SYSTEM = """\
You are the producer of "Agent Report", a weekly podcast about agentic AI.
Your job: pick the 5-7 best stories for a general-audience, 20-minute episode.

Prioritise stories that are:
1. Genuinely new or surprising (not hype rehash)
2. Relevant to AI agents, autonomous systems, multi-agent frameworks, or AI tool use
3. A mix of research breakthroughs, industry moves, and community drama
4. Interesting to non-experts — accessible, with real-world stakes

Return ONLY valid JSON — an array of objects with keys:
  "rank" (1 = most important),
  "title",
  "url",
  "source",
  "why_interesting" (2-3 sentences, plain English),
  "funny_angle" (a humorous observation or analogy, 1-2 sentences)

No markdown fences, no extra keys, no commentary outside the JSON.\
"""


def _stories_to_prompt(stories: list[Story]) -> str:
    lines = []
    for i, s in enumerate(stories, 1):
        pub = s.published.strftime("%Y-%m-%d") if s.published else "unknown date"
        lines.append(
            f"{i}. [{s.source}] {s.title}\n"
            f"   URL: {s.url}\n"
            f"   Date: {pub}\n"
            f"   Summary: {s.summary}\n"
        )
    return "\n".join(lines)


def curate_stories(
    stories: list[Story],
    model: str = "claude-opus-4-7",
    verbose: bool = True,
) -> list[dict]:
    if not stories:
        return []

    client = anthropic.Anthropic()
    prompt = (
        f"Here are {len(stories)} items from the past week. "
        "Pick the best 5-7 for the podcast.\n\n"
        + _stories_to_prompt(stories)
    )

    if verbose:
        print(f"  Curating {len(stories)} stories with {model}...")

    message = client.messages.create(
        model=model,
        max_tokens=2048,
        system=_CURATOR_SYSTEM,
        messages=[{"role": "user", "content": prompt}],
    )

    raw = message.content[0].text.strip()
    # Strip accidental markdown fences
    if raw.startswith("```"):
        raw = raw.split("```")[1]
        if raw.startswith("json"):
            raw = raw[4:]
    raw = raw.strip()

    selected = json.loads(raw)
    selected.sort(key=lambda x: x.get("rank", 99))
    if verbose:
        print(f"  → selected {len(selected)} stories")
    return selected
