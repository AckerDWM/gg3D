"""Data source configuration."""

ARXIV_QUERY = (
    "(ti:agent OR ti:agentic OR ti:multi-agent OR ti:autonomous OR ti:tool-use)"
    " AND (cat:cs.AI OR cat:cs.LG OR cat:cs.CL)"
)
ARXIV_MAX_RESULTS = 30

ARXIV_API_URL = "https://export.arxiv.org/api/query"
HN_API_BASE = "https://hacker-news.firebaseio.com/v0"
HN_SEARCH_URL = "https://hn.algolia.com/api/v1/search"

RSS_FEEDS = [
    ("Anthropic Blog",      "https://www.anthropic.com/rss.xml"),
    ("OpenAI Blog",         "https://openai.com/blog/rss.xml"),
    ("Google DeepMind",     "https://deepmind.google/blog/rss.xml"),
    ("The Gradient",        "https://thegradient.pub/rss/"),
    ("Import AI",           "https://importai.substack.com/feed"),
    ("Sebastian Raschka",   "https://magazine.sebastianraschka.com/feed"),
]

HN_AGENT_KEYWORDS = [
    "agent", "agentic", "autonomous", "llm", "claude", "gpt", "gemini",
    "copilot", "mcp", "tool use", "multi-agent", "ai assistant",
]

# ~20 minutes at 140 wpm ≈ 2800 words
TARGET_WORD_COUNT = 2800

DAYS_LOOKBACK = 7
