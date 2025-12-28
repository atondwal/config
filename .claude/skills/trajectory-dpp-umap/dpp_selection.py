#!/usr/bin/env python3
"""Select diverse subset using DPP on trajectory embeddings.

Usage:
    python dpp_selection.py <actions_json> <output_json> [--problems <json>] [--scale <float>]
"""

import argparse
import json

import numpy as np
from sentence_transformers import SentenceTransformer


def sample_dpp_exact(L: np.ndarray, rng: np.random.Generator = None) -> list[int]:
    """Sample from a DPP using eigenvalue decomposition.

    L is the L-ensemble kernel matrix (similarity matrix).
    Returns indices of selected items.

    Uses standard DPP sampling algorithm from Kulesza & Taskar.
    """
    if rng is None:
        rng = np.random.default_rng()

    n = L.shape[0]
    eigenvalues, eigenvectors = np.linalg.eigh(L)

    # Select eigenvalues with probability lambda_i / (1 + lambda_i)
    probs = eigenvalues / (1 + eigenvalues)
    probs = np.clip(probs, 0, 1)
    selected_eigs = rng.random(n) < probs

    if not selected_eigs.any():
        # Fallback: select at least one
        selected_eigs[np.argmax(eigenvalues)] = True

    V = eigenvectors[:, selected_eigs].copy()
    k = V.shape[1]

    # Sample k items using V
    selected = []
    for i in range(k):
        # Compute probabilities proportional to ||v_i||^2
        norms_sq = (V ** 2).sum(axis=1)
        norms_sq = np.maximum(norms_sq, 0)
        probs = norms_sq / (norms_sq.sum() + 1e-10)

        # Sample item
        item = rng.choice(n, p=probs)
        selected.append(item)

        # Find the column with largest |V[item, j]|
        j = np.argmax(np.abs(V[item, :]))

        # Update V by projecting out the selected item
        if abs(V[item, j]) > 1e-10:
            V = V - np.outer(V[:, j], V[item, :]) / V[item, j]

        # Remove column j
        V = np.delete(V, j, axis=1)

        if V.shape[1] == 0:
            break

    return selected


def greedy_max_distance(embeddings: np.ndarray, k: int) -> list[int]:
    """Greedy facility location: select k points maximizing min distance."""
    n = embeddings.shape[0]
    selected = []

    # Start with point closest to centroid
    centroid = embeddings.mean(axis=0)
    dists_to_centroid = np.linalg.norm(embeddings - centroid, axis=1)
    first = np.argmin(dists_to_centroid)
    selected.append(first)

    # Greedily add point furthest from selected set
    min_dists = np.full(n, np.inf)
    while len(selected) < k:
        last = selected[-1]
        dists_to_last = np.linalg.norm(embeddings - embeddings[last], axis=1)
        min_dists = np.minimum(min_dists, dists_to_last)
        min_dists[selected] = -np.inf  # Don't re-select
        next_point = np.argmax(min_dists)
        selected.append(next_point)

    return selected


def main():
    parser = argparse.ArgumentParser(
        description="Select diverse problems using DPP"
    )
    parser.add_argument("actions_json", help="Actions JSON from extract_actions.py")
    parser.add_argument("output_json", help="Output JSON file")
    parser.add_argument("--problems", help="JSON list of problems to filter to")
    parser.add_argument(
        "--scale", type=float, default=1.0,
        help="Scale kernel to adjust selection size (>1 = more, <1 = fewer)"
    )
    parser.add_argument("--seed", type=int, default=42, help="Random seed")
    args = parser.parse_args()

    # Load trajectory actions
    with open(args.actions_json) as f:
        actions_data = json.load(f)

    # Filter to specified problems if provided
    if args.problems:
        with open(args.problems) as f:
            filter_problems = set(json.load(f))
        problems = sorted([
            p for p in filter_problems if p in actions_data["problems"]
        ])
        print(f"Filtered to {len(problems)} problems")
    else:
        problems = sorted(actions_data["problems"].keys())
        print(f"Using all {len(problems)} problems")

    # Build trajectory text for each problem
    trajectories = []
    for problem in problems:
        pdata = actions_data["problems"][problem]
        unique_actions = pdata["unique_actions"]
        trajectory_text = "\n".join(unique_actions)
        trajectories.append(trajectory_text)

    # Embed trajectories
    print("\nLoading sentence transformer...")
    model = SentenceTransformer("all-MiniLM-L6-v2")

    print("Computing trajectory embeddings...")
    embeddings = model.encode(trajectories, show_progress_bar=True)
    embeddings = embeddings / np.linalg.norm(embeddings, axis=1, keepdims=True)

    print(f"Embeddings shape: {embeddings.shape}")

    # Build L-ensemble kernel (similarity matrix)
    L = args.scale * (embeddings @ embeddings.T)
    print(f"Kernel matrix shape: {L.shape}")
    eigs = np.linalg.eigvalsh(L)
    print(f"Kernel eigenvalue range: [{eigs.min():.4f}, {eigs.max():.4f}]")
    expected_size = (eigs / (1 + eigs)).sum()
    print(f"Expected DPP sample size: {expected_size:.1f}")

    # Run DPP sampling
    print("\nRunning DPP sampling...")
    rng = np.random.default_rng(args.seed)

    # Sample multiple times and pick median-sized
    samples = []
    for i in range(10):
        sample = sample_dpp_exact(L, rng)
        samples.append(sample)
        print(f"  Sample {i+1}: {len(sample)} items")

    samples.sort(key=len)
    selected_indices = samples[len(samples) // 2]
    selected_problems = [problems[i] for i in selected_indices]

    print(f"\nDPP selected {len(selected_problems)} problems")

    # Also compute greedy for comparison
    print("\nComputing greedy max-distance selection...")
    greedy_indices = greedy_max_distance(embeddings, len(selected_problems))
    greedy_problems = [problems[i] for i in greedy_indices]

    # Save results
    results = {
        "total_problems": len(problems),
        "dpp_selected_count": len(selected_problems),
        "dpp_selected": sorted(selected_problems),
        "greedy_selected": sorted(greedy_problems),
        "kernel_scale": args.scale,
        "embedding_model": "all-MiniLM-L6-v2",
        "embeddings": embeddings.tolist(),
        "problem_order": problems,
    }

    with open(args.output_json, "w") as f:
        json.dump(results, f, indent=2)

    print(f"\nSaved to {args.output_json}")


if __name__ == "__main__":
    main()
