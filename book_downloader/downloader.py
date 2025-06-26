import os
import requests
import time
import re

# Directory to save books
SAVE_DIR = "books/adults"
os.makedirs(SAVE_DIR, exist_ok=True)

# Target number of books
TARGET = 500

# Base URL
BASE_URL = "https://gutendex.com/books/?languages=en"

# List of bookshelf strings to skip if found in a book's bookshelves
SKIP_BOOKSHELVES = [
    "Browsing: Children & Young Adult Reading",
    "Children's Literature"
]

def get_txt_url(formats):
    """Returns a valid plain-text download link from available formats."""
    for key, url in formats.items():
        if key.startswith("text/plain") and ".txt" in url:
            return url
    return None

def should_skip(book):
    """Return True if the book contains any bookshelf from SKIP_BOOKSHELVES."""
    bookshelves = book.get("bookshelves", [])
    return any(skip_shelf in bookshelves for skip_shelf in SKIP_BOOKSHELVES)

def download_books():
    downloaded = 0
    page = 1
    seen_titles = set()

    while downloaded < TARGET:
        url = f"{BASE_URL}&page={page}"
        print(f"Fetching page {page}...")

        try:
            res = requests.get(url)
            res.raise_for_status()
            data = res.json()
        except Exception as e:
            print(f"Error fetching page {page}: {e}")
            break

        for book in data["results"]:
            if should_skip(book):
                print(f"⏭️ Skipping due to bookshelves filter: {book['title']}")
                continue

            txt_url = get_txt_url(book["formats"])
            if txt_url:
                title_key = book['title'].lower().strip()
                if title_key in seen_titles:
                    print(f"⏭️ Skipping duplicate: {book['title']}")
                    continue  # Skip this book
                
                seen_titles.add(title_key)  # Mark as seen
                
                try:
                    book_res = requests.get(txt_url)
                    book_res.raise_for_status()

                    safe_title = re.sub(r'[^a-zA-Z0-9_-]', '', book['title'][:40].replace(' ', '_'))
                    filename = f"{book['id']}_{safe_title}.txt"
                    filepath = os.path.join(SAVE_DIR, filename)

                    with open(filepath, "w", encoding="utf-8") as f:
                        f.write(book_res.text)

                    downloaded += 1
                    print(f"✅ [{downloaded}/{TARGET}] Downloaded: {book['title']}")
                    print(f"   Bookshelves: {book.get('bookshelves', [])}")

                    if downloaded >= TARGET:
                        break
                    time.sleep(0.1)  # Be nice to the server
                except Exception as e:
                    print(f"Failed to download {book['title']}: {e}")
                    continue

        if not data.get("next"):
            print("No more results available.")
            break

        page += 1

if __name__ == "__main__":
    download_books()
