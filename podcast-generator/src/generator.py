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

## Naming and attribution rules — these are non-negotiable

Always use the full proper name of every institution, researcher, technology, protocol,
company, and product. The listener should never need to look at a reference list to
know who did the work or what it was called.

Specifically:
- Never say "researchers" alone — say "researchers at MIT CSAIL" or "the team at Anthropic"
- Never say "a new paper" — say "a paper in Nature Medicine" or "arXiv:2603.07670"
- Never say "a major tech company" — say "Google DeepMind" or "Microsoft Research"
- Name benchmarks, datasets, and protocols explicitly (CyberGym, ClinicalBench, A2A, MCP)
- Name journals on first mention: Nature Medicine, npj Digital Medicine, Science Robotics,
  NEJM AI, The Lancet Digital Health, NeurIPS, ICML, ICLR, ACL — whichever applies
- Name respected technical blogs when you draw on them: Simon Willison's weblog,
  The Batch from DeepLearning.AI, Towards Data Science, Import AI by Jack Clark,
  The Gradient, Sebastian Raschka's newsletter
- Include arXiv IDs in the format arXiv:YYMM.NNNNN when covering preprints

## Structure (follow this exactly)

[COLD OPEN] (≈150 words)
A punchy, funny hook riffing on the week's most absurd or dramatic development.
Name the technology and organization immediately. No intro of the show yet —
just drop the listener into the chaos.

[INTRO] (≈120 words)
"Welcome to Agent Report..." — introduce the show, the host (you), and tease the top
stories by name. Keep it warm and a bit cheeky.

[WEEK IN REVIEW] (≈200 words)
The overall vibe and theme of the week. Name the conferences, organizations, and reports
you draw on. Use a memorable metaphor.

[STORY SEGMENTS] — one per selected story (≈350 words each)
For each story:
  • Open by naming the institution, publication venue, and technology
  • Explain it accessibly in plain English (no jargon without a one-liner explanation)
  • Why it actually matters for regular humans
  • A humorous angle, analogy, or hypothetical — keep it smart, not silly
  • Reference any supporting academic work by journal name and authors

[LIGHTNING ROUND] (≈150 words)
Rapid-fire 2-3 sentence blurbs on 3-4 smaller stories. Name the company or institution
in the first sentence of each blurb. Fast, punchy, funny.

[WEEK IN SUMMARY] (≈150 words)
A tight bulleted recap — one sentence per main story — with the name of the
institution/technology leading each bullet. This is the listener's cheat sheet.
Format as a spoken list, not a written one: "First... Second... Third..."

[OUTRO] (≈100 words)
A witty observation about the week's overall theme, one thing to watch next week
(named specifically), and a sign-off that's slightly self-aware about the absurdity
of AI news.

## Formatting rules
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
        parts = [
            f"Rank {s.get('rank', '?')}: {s.get('title', 'Unknown')}",
            f"  URL: {s.get('url', '')}",
            f"  Source feed: {s.get('source', '')}",
        ]
        if s.get("institution"):
            parts.append(f"  Institution: {s['institution']}")
        if s.get("authors"):
            parts.append(f"  Authors: {s['authors']}")
        if s.get("journal"):
            parts.append(f"  Journal/venue: {s['journal']}")
        if s.get("arxiv_id"):
            parts.append(f"  arXiv ID: {s['arxiv_id']}")
        if s.get("tech_names"):
            parts.append(f"  Technologies/products: {s['tech_names']}")
        parts.append(f"  Why interesting: {s.get('why_interesting', '')}")
        parts.append(f"  Funny angle: {s.get('funny_angle', '')}")
        lines.append("\n".join(parts))
    return "\n\n".join(lines)
