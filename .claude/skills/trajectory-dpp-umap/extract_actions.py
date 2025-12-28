#!/usr/bin/env python3
"""Extract actions from transcripts for trajectory-based diversity analysis.

Usage:
    python extract_actions.py <transcripts_dir> <output_json>
"""

import argparse
import json
import re
from collections import defaultdict
from pathlib import Path


def decode_transcript(raw_text: str) -> str:
    """Decode JSON-encoded transcript text."""
    try:
        return json.loads(raw_text)
    except json.JSONDecodeError:
        return raw_text


def extract_actions_from_transcript(text: str) -> list[str]:
    """Extract tool invocations with full command text from transcript."""
    actions = []

    # Pattern to match tool invocations: <invoke name="tool_name">...</invoke>
    # Handle antml: namespace
    invoke_pattern = re.compile(
        r'<(?:antml:)?invoke name="([^"]+)">(.*?)</(?:antml:)?invoke>',
        re.DOTALL
    )

    for match in invoke_pattern.finditer(text):
        tool_name = match.group(1)
        params_block = match.group(2)

        # Skip placeholder examples from system prompt
        if tool_name.startswith("$"):
            continue

        # Extract parameters - handle antml: namespace
        param_pattern = re.compile(
            r'<(?:antml:)?parameter name="([^"]+)">([^<]*)</(?:antml:)?parameter>',
            re.DOTALL
        )
        params = {}
        for pmatch in param_pattern.finditer(params_block):
            params[pmatch.group(1)] = pmatch.group(2).strip()

        # Build action string based on tool type
        if tool_name == "bash":
            cmd = params.get("command", "")
            # Skip empty commands
            if not cmd.strip():
                continue
            # Truncate very long commands but keep meaningful prefix
            if len(cmd) > 500:
                cmd = cmd[:500] + "..."
            actions.append(f"bash: {cmd}")
        elif tool_name == "str_replace_based_edit_tool":
            cmd_type = params.get("command", "")
            path = params.get("path", "")
            if cmd_type == "view":
                actions.append(f"view: {path}")
            elif cmd_type == "create":
                actions.append(f"create: {path}")
            elif cmd_type == "str_replace":
                old_str = params.get("old_str", "")[:100]
                actions.append(f"str_replace: {path} [{old_str}...]")
            elif cmd_type == "insert":
                line = params.get("insert_line", "?")
                actions.append(f"insert: {path}:{line}")
            else:
                actions.append(f"edit_{cmd_type}: {path}")
        else:
            # Generic tool action
            param_summary = " ".join(
                f"{k}={v[:50]}" for k, v in list(params.items())[:3]
            )
            actions.append(f"{tool_name}: {param_summary}")

    return actions


def parse_problem_from_filename(stem: str) -> str:
    """Extract problem name from transcript filename."""
    # Filename format: problem_STATUS_runid.txt
    # Status is PASS or FAIL_score (e.g., FAIL_0.67)
    # Example: arith-tok-mod-2_FAIL_0.00_36697d5e.txt
    if "_PASS_" in stem:
        return stem.split("_PASS_")[0]
    elif "_FAIL_" in stem:
        return stem.split("_FAIL_")[0]
    else:
        # Fallback: use first part
        return stem.rsplit("_", 2)[0] if stem.count("_") >= 2 else stem


def main():
    parser = argparse.ArgumentParser(
        description="Extract actions from transcripts"
    )
    parser.add_argument("transcripts_dir", help="Directory containing transcripts")
    parser.add_argument("output_json", help="Output JSON file")
    args = parser.parse_args()

    transcript_dir = Path(args.transcripts_dir)
    output_file = Path(args.output_json)

    # Group transcripts by problem
    transcripts_by_problem = defaultdict(list)
    for f in transcript_dir.glob("*.txt"):
        problem = parse_problem_from_filename(f.stem)
        transcripts_by_problem[problem].append(f)

    print(f"Found {len(transcripts_by_problem)} unique problems")

    # Extract actions for each problem
    problem_actions = {}
    all_actions = []

    for problem, files in sorted(transcripts_by_problem.items()):
        combined_actions = []
        for f in files:
            raw_text = f.read_text()
            text = decode_transcript(raw_text)
            actions = extract_actions_from_transcript(text)
            combined_actions.extend(actions)
            all_actions.extend(actions)

        problem_actions[problem] = {
            "num_runs": len(files),
            "num_actions": len(combined_actions),
            "actions": combined_actions,
            "unique_actions": list(set(combined_actions)),
        }

    # Compute action vocabulary
    action_counts = defaultdict(int)
    for action in all_actions:
        action_counts[action] += 1

    # Save results
    results = {
        "num_problems": len(problem_actions),
        "total_actions": len(all_actions),
        "unique_actions": len(action_counts),
        "problems": problem_actions,
        "action_vocabulary": dict(sorted(
            action_counts.items(), key=lambda x: -x[1]
        )[:500]),  # Top 500 actions
    }

    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)

    print(f"Extracted {len(all_actions)} total actions")
    print(f"Unique actions: {len(action_counts)}")
    print(f"Saved to {output_file}")


if __name__ == "__main__":
    main()
