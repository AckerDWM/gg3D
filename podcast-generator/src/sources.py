"""Fetch recent agentic-AI content from arXiv, Hacker News, and RSS feeds."""

import time
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from typing import Optional

try:
    import feedparser
    HAS_FEEDPARSER = True
except ImportError:
    HAS_FEEDPARSER = False

from .config import (
    ARXIV_API_URL, ARXIV_MAX_RESULTS, ARXIV_QUERY,
    DAYS_LOOKBACK, HN_AGENT_KEYWORDS, HN_API_BASE,
    HN_SEARCH_URL, RSS_FEEDS,
)


@dataclass
class Story:
    title: str
    url: str
    summary: str
    source: str
    published: Optional[datetime] = None
    score: int = 0
    tags: list[str] = field(default_factory=list)

    def age_days(self) -> float:
        if not self.published:
            return 0.0
        now = datetime.now(timezone.utc)
        pub = self.published.replace(tzinfo=timezone.utc) if self.published.tzinfo is None else self.published
        return (now - pub).total_seconds() / 86400


def _fetch_url(url: str, timeout: int = 10) -> str:
    req = urllib.request.Request(url, headers={"User-Agent": "ai-podcast-generator/1.0"})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read().decode("utf-8", errors="replace")


def fetch_arxiv(days: int = DAYS_LOOKBACK) -> list[Story]:
    cutoff = datetime.now(timezone.utc) - timedelta(days=days)
    params = urllib.parse.urlencode({
        "search_query": ARXIV_QUERY,
        "start": 0,
        "max_results": ARXIV_MAX_RESULTS,
        "sortBy": "submittedDate",
        "sortOrder": "descending",
    })
    xml_text = _fetch_url(f"{ARXIV_API_URL}?{params}")
    root = ET.fromstring(xml_text)
    ns = {"atom": "http://www.w3.org/2005/Atom"}

    stories = []
    for entry in root.findall("atom:entry", ns):
        pub_str = entry.findtext("atom:published", default="", namespaces=ns)
        try:
            pub = datetime.fromisoformat(pub_str.replace("Z", "+00:00"))
        except ValueError:
            continue
        if pub < cutoff:
            continue

        title = (entry.findtext("atom:title", default="", namespaces=ns) or "").strip().replace("\n", " ")
        summary = (entry.findtext("atom:summary", default="", namespaces=ns) or "").strip().replace("\n", " ")
        link_el = entry.find("atom:link[@rel='alternate']", ns) or entry.find("atom:link", ns)
        url = link_el.get("href", "") if link_el is not None else ""

        stories.append(Story(
            title=title,
            url=url,
            summary=summary[:500],
            source="arXiv",
            published=pub,
            tags=["research"],
        ))
    return stories


def fetch_hacker_news(days: int = DAYS_LOOKBACK) -> list[Story]:
    since = int((datetime.now(timezone.utc) - timedelta(days=days)).timestamp())
    keywords = " OR ".join(HN_AGENT_KEYWORDS[:6])
    params = urllib.parse.urlencode({
        "query": keywords,
        "tags": "story",
        "numericFilters": f"created_at_i>{since},points>10",
        "hitsPerPage": 40,
    })
    data_raw = _fetch_url(f"{HN_SEARCH_URL}?{params}")

    import json
    data = json.loads(data_raw)
    stories = []
    for hit in data.get("hits", []):
        title = hit.get("title", "")
        url = hit.get("url") or f"https://news.ycombinator.com/item?id={hit.get('objectID')}"
        summary = f"HN discussion with {hit.get('points', 0)} points and {hit.get('num_comments', 0)} comments."
        created = hit.get("created_at_i")
        pub = datetime.fromtimestamp(created, tz=timezone.utc) if created else None

        stories.append(Story(
            title=title,
            url=url,
            summary=summary,
            source="Hacker News",
            published=pub,
            score=hit.get("points", 0),
            tags=["community"],
        ))
    return stories


def fetch_rss_feeds(days: int = DAYS_LOOKBACK) -> list[Story]:
    if not HAS_FEEDPARSER:
        print("  [skip] feedparser not installed — skipping RSS feeds")
        return []

    cutoff = datetime.now(timezone.utc) - timedelta(days=days)
    stories = []
    for feed_name, feed_url in RSS_FEEDS:
        try:
            feed = feedparser.parse(feed_url)
            for entry in feed.entries:
                pub = None
                if hasattr(entry, "published_parsed") and entry.published_parsed:
                    pub = datetime(*entry.published_parsed[:6], tzinfo=timezone.utc)
                if pub and pub < cutoff:
                    continue

                title = getattr(entry, "title", "")
                url = getattr(entry, "link", "")
                summary = getattr(entry, "summary", "") or getattr(entry, "description", "")
                summary = summary[:500] if summary else ""

                stories.append(Story(
                    title=title,
                    url=url,
                    summary=summary,
                    source=feed_name,
                    published=pub,
                    tags=["blog"],
                ))
        except Exception as exc:
            print(f"  [warn] RSS fetch failed for {feed_name}: {exc}")
    return stories


def gather_all_stories(days: int = DAYS_LOOKBACK, verbose: bool = True) -> list[Story]:
    all_stories: list[Story] = []

    steps = [
        ("arXiv papers", fetch_arxiv),
        ("Hacker News", fetch_hacker_news),
        ("RSS feeds", fetch_rss_feeds),
    ]
    for label, fn in steps:
        if verbose:
            print(f"  Fetching {label}...")
        try:
            results = fn(days=days)
            all_stories.extend(results)
            if verbose:
                print(f"    → {len(results)} items")
        except Exception as exc:
            if verbose:
                print(f"    → failed: {exc}")

    return all_stories
