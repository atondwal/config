#!/usr/bin/env python3
"""Search Claude session history for matching content."""

import json
import os
import re
import sys
from pathlib import Path


def decode_project_path(dirname: str) -> str:
    """Convert directory name back to path: -home-user-src -> /home/user/src"""
    if dirname.startswith("-"):
        return dirname.replace("-", "/", 1).replace("-", "/")
    return dirname


def extract_content(msg: dict) -> str:
    """Extract searchable text from a message."""
    content = msg.get("message", {}).get("content", "")
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for item in content:
            if isinstance(item, str):
                parts.append(item)
            elif isinstance(item, dict):
                if item.get("type") == "text":
                    parts.append(item.get("text", ""))
                elif item.get("type") == "tool_use":
                    parts.append(f"[tool: {item.get('name', '')}]")
                    inp = item.get("input", {})
                    if isinstance(inp, dict):
                        for v in inp.values():
                            if isinstance(v, str):
                                parts.append(v)
                elif item.get("type") == "tool_result":
                    result = item.get("content", "")
                    if isinstance(result, str):
                        parts.append(result)
                elif item.get("type") == "thinking":
                    parts.append(item.get("thinking", ""))
        return "\n".join(parts)
    return ""


def search_sessions(pattern: str, max_results: int = 20) -> list:
    """Search all session files for pattern matches."""
    projects_dir = Path.home() / ".claude" / "projects"
    if not projects_dir.exists():
        return []

    try:
        regex = re.compile(pattern, re.IGNORECASE)
    except re.error as e:
        print(f"Invalid regex: {e}", file=sys.stderr)
        return []

    results = []
    session_files = []

    for project_dir in projects_dir.iterdir():
        if not project_dir.is_dir():
            continue
        for jsonl_file in project_dir.glob("*.jsonl"):
            session_files.append((jsonl_file, project_dir.name))

    # Sort by modification time (newest first)
    session_files.sort(key=lambda x: x[0].stat().st_mtime, reverse=True)

    for jsonl_file, project_name in session_files:
        if len(results) >= max_results:
            break

        project_path = decode_project_path(project_name)
        messages = []

        try:
            with open(jsonl_file, "r") as f:
                for line in f:
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        msg = json.loads(line)
                        if msg.get("type") in ("user", "assistant"):
                            messages.append(msg)
                    except json.JSONDecodeError:
                        continue
        except Exception:
            continue

        for i, msg in enumerate(messages):
            if len(results) >= max_results:
                break

            content = extract_content(msg)
            if not content:
                continue

            match = regex.search(content)
            if match:
                # Get context: 1 message before and after
                context_before = ""
                context_after = ""
                if i > 0:
                    ctx = extract_content(messages[i - 1])
                    if ctx:
                        context_before = ctx[:200] + "..." if len(ctx) > 200 else ctx

                if i < len(messages) - 1:
                    ctx = extract_content(messages[i + 1])
                    if ctx:
                        context_after = ctx[:200] + "..." if len(ctx) > 200 else ctx

                # Truncate matching content
                if len(content) > 500:
                    start = max(0, match.start() - 100)
                    end = min(len(content), match.end() + 400)
                    content = ("..." if start > 0 else "") + content[start:end] + "..."

                results.append({
                    "session": jsonl_file.stem,
                    "project": project_path,
                    "timestamp": msg.get("timestamp", ""),
                    "role": msg.get("message", {}).get("role", msg.get("type", "")),
                    "slug": msg.get("slug", ""),
                    "content": content,
                    "context_before": context_before,
                    "context_after": context_after,
                })

    return results


def main():
    if len(sys.argv) < 2:
        print("Usage: search.py <pattern> [max_results]", file=sys.stderr)
        sys.exit(1)

    pattern = sys.argv[1]
    max_results = int(sys.argv[2]) if len(sys.argv) > 2 else 20

    results = search_sessions(pattern, max_results)

    if not results:
        print(f"No matches found for: {pattern}")
        return

    print(f"Found {len(results)} matches for: {pattern}\n")
    print("=" * 60)

    for r in results:
        print(f"\nSession: {r['slug'] or r['session']}")
        print(f"Project: {r['project']}")
        print(f"Time: {r['timestamp']}")
        print(f"Role: {r['role']}")
        print("-" * 40)
        if r["context_before"]:
            print(f"[before] {r['context_before']}")
            print()
        print(r["content"])
        if r["context_after"]:
            print()
            print(f"[after] {r['context_after']}")
        print("=" * 60)


if __name__ == "__main__":
    main()
