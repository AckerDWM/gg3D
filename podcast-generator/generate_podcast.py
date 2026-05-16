#!/usr/bin/env python3
"""
Agent Report — weekly agentic AI podcast generator.

Usage:
    python generate_podcast.py [--days N] [--output FILE] [--model MODEL]
"""

import argparse
import os
import sys
from datetime import date
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description="Generate this week's Agent Report podcast script.")
    parser.add_argument("--days", type=int, default=7, help="Days to look back for stories (default: 7)")
    parser.add_argument("--output", type=str, default=None, help="Save script to this file (default: stdout)")
    parser.add_argument(
        "--model",
        type=str,
        default="claude-opus-4-7",
        help="Claude model to use (default: claude-opus-4-7)",
    )
    parser.add_argument("--no-arxiv", action="store_true", help="Skip arXiv fetching")
    parser.add_argument("--no-hn", action="store_true", help="Skip Hacker News fetching")
    parser.add_argument("--no-rss", action="store_true", help="Skip RSS feed fetching")
    args = parser.parse_args()

    if not os.environ.get("ANTHROPIC_API_KEY"):
        print("Error: ANTHROPIC_API_KEY environment variable not set.", file=sys.stderr)
        sys.exit(1)

    from src.sources import gather_all_stories, fetch_arxiv, fetch_hacker_news, fetch_rss_feeds
    from src.curator import curate_stories
    from src.generator import generate_script

    print(f"\nAgent Report Generator — looking back {args.days} days\n{'='*50}")

    # --- Gather ---
    print("\n[1/3] Gathering stories...")
    all_stories = []

    if not args.no_arxiv:
        print("  Fetching arXiv papers...")
        try:
            results = fetch_arxiv(days=args.days)
            all_stories.extend(results)
            print(f"    → {len(results)} papers")
        except Exception as e:
            print(f"    → arXiv failed: {e}")

    if not args.no_hn:
        print("  Fetching Hacker News...")
        try:
            results = fetch_hacker_news(days=args.days)
            all_stories.extend(results)
            print(f"    → {len(results)} stories")
        except Exception as e:
            print(f"    → HN failed: {e}")

    if not args.no_rss:
        print("  Fetching RSS feeds...")
        try:
            results = fetch_rss_feeds(days=args.days)
            all_stories.extend(results)
            print(f"    → {len(results)} posts")
        except Exception as e:
            print(f"    → RSS failed: {e}")

    if not all_stories:
        print("\nNo stories found. Check your network connection or try --days 14.")
        sys.exit(1)

    print(f"\n  Total raw stories: {len(all_stories)}")

    # --- Curate ---
    print("\n[2/3] Curating top stories...")
    selected = curate_stories(all_stories, model=args.model)
    if not selected:
        print("Curation returned no stories.")
        sys.exit(1)

    # --- Generate ---
    print("\n[3/3] Writing podcast script...")
    script = generate_script(selected, model=args.model)

    # --- Output ---
    print("\n" + "="*50)
    if args.output:
        out_path = Path(args.output)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(script, encoding="utf-8")
        print(f"Script saved to: {out_path}")
    else:
        print("\n")
        print(script)

    print(f"\n{'='*50}")
    print(f"Done. Episode date: {date.today().strftime('%B %d, %Y')}")


if __name__ == "__main__":
    main()
