"""Generate the full podcast script via Claude."""

import anthropic
from datetime import date

from .config import TARGET_WORD_COUNT


_SCRIPT_SYSTEM = f"""\
You are the writer and host of "Agent Report" — a weekly, lightly irreverent podcast
about the world of agentic AI. Your tone is like a knowledgeable friend who has clearly
read too many AI papers but is still fun at parties.

Target length: ~{TARGET_WORD_COUNT} words (~20 minutes at speaking pace).
Audience: curious general public — assume no ML background, do assume a functioning brain.

Structure (follow this exactly):

[COLD OPEN] (≈150 words)
A punchy, funny hook riffing on the week's most absurd or dramatic development.
No intro of the show yet — just drop the listener into the chaos.

[INTRO] (≈120 words)
"Welcome to Agent Report..." — introduce the show, the host (you), and tease the top stories.
Keep it warm and a bit cheeky.

[WEEK IN REVIEW] (≈200 words)
30-second summary of the overall vibe: what was the theme of the week?
Recurring characters? Ongoing sagas? Use a memorable metaphor.

[STORY SEGMENTS] — one per selected story (≈350 words each)
For each story:
  • Explain it accessibly in plain English (no jargon without a one-liner explanation)
  • Why it actually matters for regular humans
  • A humorous angle, analogy, or hypothetical — keep it smart, not silly
  • A one-sentence tease into the next story

[LIGHTNING ROUND] (≈150 words)
Rapid-fire 2-3 sentence blurbs on 3-4 smaller stories or quirky finds.
Fast, punchy, funny.

[OUTRO] (≈100 words)
Wrap up with a witty observation about the week, something to watch next week,
and a sign-off that's slightly self-aware about the absurdity of AI news.

Formatting rules:
- Use [SECTION TITLE] headers exactly as shown above
- Do NOT use markdown bullets or asterisks inside the script body
- Write in spoken-word style — contractions, rhetorical questions, asides are great
- Do not break the fourth wall (don't mention you're an AI writing this)
\
"""


def generate_script(
    curated_stories: list[dict],
    extra_context: str = "",
    model: str = "claude-opus-4-7",
    verbose: bool = True,
) -> str:
    client = anthropic.Anthropic()

    today = date.today().strftime("%B %d, %Y")
    stories_text = _format_stories_for_prompt(curated_stories)

    user_msg = (
        f"Today is {today}. Write this week's episode of Agent Report.\n\n"
        f"Selected stories (ranked by importance):\n\n{stories_text}"
    )
    if extra_context:
        user_msg += f"\n\nAdditional context:\n{extra_context}"

    if verbose:
        print(f"  Generating script with {model}...")

    message = client.messages.create(
        model=model,
        max_tokens=4096,
        system=_SCRIPT_SYSTEM,
        messages=[{"role": "user", "content": user_msg}],
    )

    script = message.content[0].text.strip()
    if verbose:
        word_count = len(script.split())
        print(f"  → {word_count} words generated")
    return script


def _format_stories_for_prompt(stories: list[dict]) -> str:
    lines = []
    for s in stories:
        lines.append(
            f"Rank {s.get('rank', '?')}: {s.get('title', 'Unknown')}\n"
            f"  Source: {s.get('source', '')}\n"
            f"  URL: {s.get('url', '')}\n"
            f"  Why interesting: {s.get('why_interesting', '')}\n"
            f"  Funny angle: {s.get('funny_angle', '')}\n"
        )
    return "\n".join(lines)
