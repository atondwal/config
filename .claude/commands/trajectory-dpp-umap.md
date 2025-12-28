# Trajectory DPP-UMAP Visualization

Generate an interactive 3D UMAP visualization with DPP (Determinantal Point Process) diversity selection based on trajectory embeddings from transcripts.

## What This Does

1. **Extract actions** from transcripts (bash commands, file edits, tool calls)
2. **Embed trajectories** using sentence-transformers
3. **DPP sampling** to select diverse subset (items "repel" like fermions)
4. **Generate interactive HTML** with 3D UMAP visualization

## Scripts Location

The scripts are in `~/.claude/skills/trajectory-dpp-umap/`:
- `extract_actions.py` - Parse transcripts, extract tool invocations
- `dpp_selection.py` - Embed trajectories, run DPP sampling
- `generate_viz.py` - Generate the HTML visualization

## Usage

```bash
# 1. Extract actions from transcripts
python ~/.claude/skills/trajectory-dpp-umap/extract_actions.py \
    <transcripts_dir> \
    actions.json

# 2. Run DPP selection (optionally filter to specific problems)
python ~/.claude/skills/trajectory-dpp-umap/dpp_selection.py \
    actions.json \
    dpp_results.json \
    [--problems hard_problems.json] \
    [--scale 1.0]

# 3. Generate visualization
python ~/.claude/skills/trajectory-dpp-umap/generate_viz.py \
    dpp_results.json \
    actions.json \
    output.html \
    [--title "My Visualization"] \
    [--taiga-env-id <uuid>] \
    [--taiga-prefix "my-env-"]
```

## Input Requirements

**Transcripts directory**: Contains `*.txt` files with JSON-encoded transcripts
- Filename format: `{problem}_PASS_{runid}.txt` or `{problem}_FAIL_{score}_{runid}.txt`
- Transcripts contain `<invoke name="...">` or `<invoke name="...">` tool calls

## Options

**DPP selection:**
- `--problems <json>`: Filter to only these problems
- `--scale <float>`: Scale kernel to adjust selection size (>1 = more, <1 = fewer)
- `--seed <int>`: Random seed for reproducibility

**Visualization:**
- `--title <str>`: Page title
- `--taiga-env-id <uuid>`: Enable Taiga links with this environment ID
- `--taiga-prefix <str>`: Prefix for problem IDs in Taiga URLs
- `--code-dir <path>`: Directory with code files to show in details

## Example (RASP decompilation)

```bash
cd /path/to/project

# Extract actions
python ~/.claude/skills/trajectory-dpp-umap/extract_actions.py \
    transcripts_v29/ \
    trajectory_actions.json

# Run DPP on hard problems only
python ~/.claude/skills/trajectory-dpp-umap/dpp_selection.py \
    trajectory_actions.json \
    dpp_results.json \
    --problems zeros_both_versions.json

# Generate viz with Taiga links
python ~/.claude/skills/trajectory-dpp-umap/generate_viz.py \
    dpp_results.json \
    trajectory_actions.json \
    rasp_trajectory_umap.html \
    --title "RASP Hard Problems - Trajectory UMAP" \
    --taiga-env-id 8e646c11-1461-44a4-9e8d-e3800a02ba07 \
    --taiga-prefix "rasp-decompile-"
```

## Dependencies

Requires a Python environment with:
- `sentence-transformers`
- `numpy`

Create with: `uv venv && source .venv/bin/activate && uv pip install sentence-transformers numpy`

## How DPP Works

DPP = "free fermions picking seats". Probability of selecting subset S is proportional to det(L_S), where L is a similarity matrix. Similar items have correlated rows in L, making the determinant small → unlikely to select both. This naturally produces diverse selections.

Expected sample size ≈ Σ(λ_i / (1 + λ_i)) where λ_i are eigenvalues of L. Scale L to adjust.
