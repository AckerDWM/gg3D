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
    # Company research blogs
    ("Anthropic Blog",              "https://www.anthropic.com/rss.xml"),
    ("OpenAI Blog",                 "https://openai.com/blog/rss.xml"),
    ("Google DeepMind Blog",        "https://deepmind.google/blog/rss.xml"),
    ("Microsoft Research Blog",     "https://www.microsoft.com/en-us/research/feed/"),
    # Academic / preprint aggregators
    ("Hugging Face Papers",         "https://huggingface.co/papers.rss"),
    # Respected technical newsletters and blogs
    ("Import AI (Jack Clark)",      "https://importai.substack.com/feed"),
    ("The Batch (DeepLearning.AI)", "https://www.deeplearning.ai/the-batch/feed/"),
    ("The Gradient",                "https://thegradient.pub/rss/"),
    ("Sebastian Raschka",           "https://magazine.sebastianraschka.com/feed"),
    ("Simon Willison",              "https://simonwillison.net/atom/everything/"),
    ("Towards Data Science",        "https://towardsdatascience.com/feed"),
]

# Used by the curator prompt to guide source attribution in the script
ACADEMIC_JOURNALS = [
    "Nature", "Nature Medicine", "Nature Machine Intelligence",
    "npj Digital Medicine", "npj Artificial Intelligence",
    "Science", "Science Robotics", "Cell", "Cell Systems",
    "NEJM AI", "The Lancet Digital Health",
    "NeurIPS", "ICML", "ICLR", "ACL", "EMNLP",
]

HN_AGENT_KEYWORDS = [
    "agent", "agentic", "autonomous", "llm", "claude", "gpt", "gemini",
    "copilot", "mcp", "tool use", "multi-agent", "ai assistant",
]

# ~20 minutes at 140 wpm ≈ 2800 words
TARGET_WORD_COUNT = 2800

DAYS_LOOKBACK = 7
