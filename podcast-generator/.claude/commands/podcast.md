---
description: Generate this week's Agent Report — a humorous 20-min podcast about agentic AI developments
---

# /podcast — Agent Report Generator

Generate a full podcast script covering the most important agentic AI developments from the past week.

## What this does

1. **Fetches** recent stories from arXiv (AI agent papers), Hacker News, and AI company blogs
2. **Curates** the top 5-7 stories using Claude — ranked by newsworthiness, relevance to agents, and entertainment value
3. **Writes** a ~2800-word (~20 min) script in the voice of "Agent Report": accessible, smart, lightly irreverent

## Workflow

Run the generator, then display the result:

```bash
python generate_podcast.py $ARGUMENTS
```

Where `$ARGUMENTS` can include:
- `--days N` — look back N days (default: 7)
- `--output path/to/script.txt` — save to file instead of stdout
- `--model claude-opus-4-7` — override the Claude model
- `--no-arxiv` / `--no-hn` / `--no-rss` — skip individual sources

## Requirements

Make sure dependencies are installed and `ANTHROPIC_API_KEY` is set:

```bash
pip install -r requirements.txt
export ANTHROPIC_API_KEY=your_key_here
```

## Episode structure

The generated script follows this format:

| Section | Length | Purpose |
|---|---|---|
| Cold Open | ~150 words | Punchy hook — names the technology and org immediately |
| Intro | ~120 words | "Welcome to Agent Report..." — teases stories by name |
| Week in Review | ~200 words | Overall vibe; names conferences, reports, organizations |
| Story Segments | ~350 words each | 5-7 deep dives: named institutions, journals, arXiv IDs, humor |
| Lightning Round | ~150 words | Rapid-fire; each blurb opens with the company/institution name |
| Week in Summary | ~150 words | Spoken bullet recap — one sentence per story, proper nouns first |
| Outro | ~100 words | Witty sign-off; names one thing to watch next week |

## Attribution standards

Every story segment names:
- The institution or company (e.g. "MIT CSAIL", "Anthropic", "Mount Sinai Icahn School of Medicine")
- The publication venue when academic (Nature Medicine, npj Digital Medicine, Science Robotics, NeurIPS, etc.)
- The arXiv ID for preprints (arXiv:YYMM.NNNNN)
- The specific product, system, or protocol name (MDASH, A2A, MCP, dreaming)
- Respected technical blogs when drawn on (Simon Willison's weblog, The Batch from DeepLearning.AI, Towards Data Science, Import AI by Jack Clark)

## Instructions for Claude

When the user runs `/podcast`:

1. Check that `ANTHROPIC_API_KEY` is available: `echo $ANTHROPIC_API_KEY | head -c 10`
2. Install dependencies if needed: `pip install -r requirements.txt -q`
3. Run the generator: `python generate_podcast.py $ARGUMENTS`
4. If it fails, diagnose and fix before re-running
5. Present the script to the user in a readable format
6. Offer to save it to a file if they haven't already

If the user provides extra context (e.g., "focus on multi-agent frameworks" or "include the GPT-5 announcement"), pass it via `--days` or explain that they can edit the config in `src/config.py` to adjust keywords and sources.
