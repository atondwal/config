#!/usr/bin/env python3
"""Generate interactive 3D UMAP visualization with DPP selection.

Usage:
    python generate_viz.py <dpp_json> <actions_json> <output_html> [options]
"""

import argparse
import colorsys
import json
from pathlib import Path


def get_wave(name: str) -> str:
    """Extract wave/tier from problem name."""
    prefixes = [
        "w0", "w1", "w2", "w3", "sel", "cmp", "arith", "cross", "xprod",
        "pat", "fin", "last", "deep", "ext", "param", "paren", "var",
        "bool", "pos", "rel", "streak", "shuffle",
    ]
    for w in prefixes:
        if name.startswith(w + "-") or name.startswith(w):
            return w
    return "other"


def generate_colors(n: int) -> list[str]:
    """Generate n distinct colors using HSV color space."""
    colors = []
    for i in range(n):
        hue = i / n
        rgb = colorsys.hsv_to_rgb(hue, 0.7, 0.9)
        hex_color = "#{:02x}{:02x}{:02x}".format(
            int(rgb[0] * 255), int(rgb[1] * 255), int(rgb[2] * 255)
        )
        colors.append(hex_color)
    return colors


def main():
    parser = argparse.ArgumentParser(
        description="Generate UMAP visualization"
    )
    parser.add_argument("dpp_json", help="DPP results JSON")
    parser.add_argument("actions_json", help="Actions JSON")
    parser.add_argument("output_html", help="Output HTML file")
    parser.add_argument("--title", default="Trajectory-Based 3D UMAP",
                        help="Page title")
    parser.add_argument("--taiga-env-id", default=None,
                        help="Taiga environment ID for links")
    parser.add_argument("--taiga-prefix", default="",
                        help="Prefix for problem IDs in Taiga (e.g., 'rasp-decompile-')")
    parser.add_argument("--code-dir", default=None,
                        help="Directory containing code files (optional)")
    args = parser.parse_args()

    # Load data
    with open(args.dpp_json) as f:
        dpp_data = json.load(f)

    with open(args.actions_json) as f:
        actions_data = json.load(f)

    problems = dpp_data["problem_order"]
    embeddings = dpp_data["embeddings"]
    dpp_selected = set(dpp_data["dpp_selected"])

    print(f"Problems: {len(problems)}")
    print(f"DPP selected: {len(dpp_selected)}")

    # Group by wave for coloring
    wave_groups = {}
    for name in problems:
        wave = get_wave(name)
        if wave not in wave_groups:
            wave_groups[wave] = []
        wave_groups[wave].append(name)

    waves = sorted(wave_groups.keys())
    wave_palette = generate_colors(len(waves))
    wave_color_map = {w: c for w, c in zip(waves, wave_palette)}

    # Load code if available
    code_lookup = {}
    if args.code_dir:
        code_dir = Path(args.code_dir)
        for f in code_dir.glob("*.py"):
            name = f.stem.replace("_", "-")
            code_lookup[name] = f.read_text()[:2000]

    # Build points data
    points_data = []
    for i, name in enumerate(problems):
        wave = get_wave(name)
        is_dpp = name in dpp_selected
        pdata = actions_data["problems"].get(name, {})

        color = wave_color_map.get(wave, "#888888")
        size = 12 if is_dpp else 6
        n_actions = pdata.get("num_actions", 0)
        n_unique = len(pdata.get("unique_actions", []))

        code = code_lookup.get(name, "")

        points_data.append({
            "name": name,
            "wave": wave,
            "is_dpp": is_dpp,
            "color": color,
            "size": size,
            "n_actions": n_actions,
            "n_unique": n_unique,
            "sample_actions": pdata.get("unique_actions", [])[:25],
            "code": code,
        })

    # Generate DPP table rows
    dpp_table_rows = []
    for i, p in enumerate(sorted(dpp_selected), 1):
        if args.taiga_env_id:
            url = f"https://taiga.ant.dev/problems/view?q={p}&version=latest&source=all&transcripts=all&problemId={args.taiga_prefix}{p}&environmentId={args.taiga_env_id}&referrer=environment"
            row = f'<tr><td style="padding:4px 8px">{i}</td><td style="padding:4px 8px"><code>{p}</code></td><td style="padding:4px 8px"><a href="{url}" target="_blank">view</a></td></tr>'
        else:
            row = f'<tr><td style="padding:4px 8px">{i}</td><td style="padding:4px 8px"><code>{p}</code></td><td style="padding:4px 8px">-</td></tr>'
        dpp_table_rows.append(row)

    # Taiga URL template for JS
    if args.taiga_env_id:
        taiga_url_template = f"https://taiga.ant.dev/problems/view?q=${{p.name}}&version=latest&source=all&transcripts=all&problemId={args.taiga_prefix}${{p.name}}&environmentId={args.taiga_env_id}&referrer=environment"
    else:
        taiga_url_template = ""

    html = generate_html(
        title=args.title,
        embeddings=embeddings,
        points_data=points_data,
        dpp_table_rows=dpp_table_rows,
        taiga_url_template=taiga_url_template,
        n_dpp=len(dpp_selected),
        n_total=len(problems),
    )

    with open(args.output_html, "w") as f:
        f.write(html)

    print(f"Saved to {args.output_html}")


def generate_html(
    title: str,
    embeddings: list,
    points_data: list,
    dpp_table_rows: list,
    taiga_url_template: str,
    n_dpp: int,
    n_total: int,
) -> str:
    """Generate the full HTML page."""

    # Escape braces for f-string compatibility in JS
    html = '''<!DOCTYPE html>
<html>
<head>
    <title>''' + title + '''</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <script src="https://unpkg.com/umap-js@1.3.3/lib/umap-js.js"></script>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .controls {
            display: flex;
            flex-direction: column;
            gap: 15px;
            margin-bottom: 20px;
            max-width: 600px;
        }
        .slider-row {
            display: flex;
            align-items: center;
            gap: 15px;
        }
        .slider-label {
            width: 120px;
            font-weight: bold;
        }
        .slider {
            flex: 1;
            height: 25px;
        }
        .slider-value {
            width: 50px;
            font-family: monospace;
            text-align: right;
        }
        #plot { width: 100%; height: 600px; }
        #details {
            background: #f5f5f5;
            border: 1px solid #ddd;
            border-radius: 4px;
            padding: 12px;
            margin-top: 10px;
            font-family: monospace;
            font-size: 11px;
            white-space: pre-wrap;
            max-height: 400px;
            overflow-y: auto;
        }
        #details.empty { color: #999; }
        .main-container {
            display: flex;
            gap: 20px;
        }
        .plot-section {
            flex: 1;
        }
        .curation-panel {
            width: 300px;
            border: 1px solid #ddd;
            border-radius: 4px;
            padding: 10px;
            max-height: 700px;
            overflow-y: auto;
            font-size: 12px;
        }
        .curation-panel h3 {
            margin: 0 0 10px 0;
            font-size: 14px;
        }
        .curation-list {
            list-style: none;
            padding: 0;
            margin: 0;
        }
        .curation-list li {
            padding: 3px 5px;
            cursor: pointer;
            border-radius: 3px;
            margin: 2px 0;
        }
        .curation-list li:hover {
            background: #eee;
        }
        .curation-list li.dpp {
            background: #d4edda;
            font-weight: bold;
        }
        .curation-list li.non-dpp {
            color: #888;
        }
        .export-btn {
            width: 100%;
            margin-top: 10px;
            padding: 8px;
            cursor: pointer;
        }
        .stats {
            background: #f0f0f0;
            padding: 8px;
            border-radius: 4px;
            margin-bottom: 10px;
        }
        .legend {
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
            margin: 10px 0;
        }
        .legend-item {
            display: flex;
            align-items: center;
            gap: 5px;
            font-size: 12px;
        }
        .legend-dot {
            width: 12px;
            height: 12px;
            border-radius: 50%;
        }
        #status {
            font-style: italic;
            color: #666;
            margin: 10px 0;
        }
        .button-row {
            display: flex;
            gap: 10px;
        }
        button {
            padding: 8px 16px;
            cursor: pointer;
        }
        .toggle-btn {
            background: #4CAF50;
            color: white;
            border: none;
            padding: 5px 10px;
            cursor: pointer;
            border-radius: 3px;
            margin-left: 10px;
        }
        .toggle-btn.remove {
            background: #f44336;
        }
    </style>
</head>
<body>
    <h2>''' + title + '''</h2>
    <details style="background:#f8f9fa; padding:12px; border-radius:6px; margin-bottom:15px; max-width:900px">
        <summary style="cursor:pointer; font-weight:bold">Methodology (click to expand)</summary>
        <div style="margin-top:10px; font-size:13px; line-height:1.5">
            <p><b>Goal:</b> Select a diverse subset of problems that maximizes coverage of the action space.</p>
            <p><b>Pipeline:</b></p>
            <ol style="margin:5px 0 5px 20px">
                <li><b>Extract actions</b> from transcripts: bash commands, file edits, etc.</li>
                <li><b>Build trajectory</b> for each problem by concatenating its unique actions into a text string</li>
                <li><b>Embed trajectories</b> using sentence-transformers (all-MiniLM-L6-v2) &rarr; 384-dim vectors</li>
                <li><b>DPP sampling</b>: Build kernel L = embeddings @ embeddings.T (cosine similarity). Sample from P(S) &prop; det(L_S) &mdash; similar items "repel" each other, selecting a diverse subset</li>
                <li><b>UMAP</b>: Project 384-dim embeddings to 3D for visualization</li>
            </ol>
            <p><b>Result:</b> ''' + str(n_dpp) + ''' problems selected from ''' + str(n_total) + ''', maximizing trajectory diversity (like fermions avoiding the same state).</p>
        </div>
    </details>

    <details style="background:#f8f9fa; padding:12px; border-radius:6px; margin-bottom:15px; max-width:900px">
        <summary style="cursor:pointer; font-weight:bold">DPP-Selected Problems (click to expand)</summary>
        <div style="margin-top:10px; font-size:12px; max-height:400px; overflow-y:auto">
            <table style="border-collapse:collapse; width:100%">
                <tr style="background:#e9ecef"><th style="padding:4px 8px; text-align:left">#</th><th style="padding:4px 8px; text-align:left">Problem</th><th style="padding:4px 8px; text-align:left">Link</th></tr>
                ''' + "\n                ".join(dpp_table_rows) + '''
            </table>
        </div>
    </details>

    <div class="controls">
        <div class="slider-row">
            <span class="slider-label">n_neighbors:</span>
            <input type="range" id="nn-slider" class="slider" min="1" max="200" value="15">
            <span id="nn-value" class="slider-value">15</span>
        </div>
        <div class="slider-row">
            <span class="slider-label">min_dist:</span>
            <input type="range" id="md-slider" class="slider" min="0" max="100" value="0">
            <span id="md-value" class="slider-value">0.00</span>
        </div>
        <div class="button-row">
            <button id="recompute-btn">Recompute UMAP</button>
            <span id="status">Ready</span>
        </div>
    </div>

    <div class="legend">
        <div class="legend-item"><span class="legend-dot" style="background:#4CAF50; width:14px; height:14px"></span> DPP Selected (large)</div>
        <div class="legend-item"><span class="legend-dot" style="background:#888; width:8px; height:8px"></span> Other (small)</div>
        <div class="legend-item"><span style="font-size:10px">Colors by wave/category</span></div>
    </div>

    <div class="main-container">
        <div class="plot-section">
            <div id="plot"></div>
            <div id="details" class="empty">Click a point to see problem details and sample actions</div>
        </div>
        <div class="curation-panel">
            <h3>Problem Selection <small style="font-weight:normal">(right-click to toggle)</small></h3>
            <div class="stats" id="stats"></div>
            <button class="export-btn" onclick="exportSelected()">Export Selected JSON</button>
            <button class="export-btn" onclick="exportSelected(true)">Export Non-Selected JSON</button>
            <hr style="margin: 10px 0">
            <ul class="curation-list" id="problem-list"></ul>
        </div>
    </div>

    <script>
        const featureMatrix = ''' + json.dumps(embeddings) + ''';
        const pointsData = ''' + json.dumps(points_data) + ''';
        const taigaUrlTemplate = "''' + taiga_url_template + '''";

        let isComputing = false;

        // Initialize plot
        const trace = {
            type: 'scatter3d',
            mode: 'markers',
            x: [], y: [], z: [],
            marker: {
                size: pointsData.map(p => p.size),
                color: pointsData.map(p => p.color),
                opacity: 0.85,
                line: {
                    width: pointsData.map(p => p.is_dpp ? 2 : 0.5),
                    color: pointsData.map(p => p.is_dpp ? '#000' : '#fff')
                }
            },
            text: pointsData.map(p =>
                `<b>${p.name}</b><br>Wave: ${p.wave}<br>Actions: ${p.n_actions} (${p.n_unique} unique)<br>${p.is_dpp ? 'DPP SELECTED' : ''}`
            ),
            hoverinfo: 'text'
        };

        const layout = {
            scene: {
                xaxis: {title: 'UMAP 1'},
                yaxis: {title: 'UMAP 2'},
                zaxis: {title: 'UMAP 3'},
                camera: {eye: {x: 1.5, y: 1.5, z: 1.2}}
            },
            margin: {l: 0, r: 0, t: 0, b: 0},
            showlegend: false
        };

        Plotly.newPlot('plot', [trace], layout);

        function updateList() {
            const list = document.getElementById('problem-list');
            const dppCount = pointsData.filter(p => p.is_dpp).length;
            const nonDppCount = pointsData.filter(p => !p.is_dpp).length;

            document.getElementById('stats').innerHTML =
                `<b>DPP Selected:</b> ${dppCount}<br>` +
                `<b>Other:</b> ${nonDppCount}<br>` +
                `<b>Total:</b> ${pointsData.length}`;

            const sorted = [...pointsData].sort((a, b) => {
                if (a.is_dpp !== b.is_dpp) return b.is_dpp - a.is_dpp;
                return a.name.localeCompare(b.name);
            });

            list.innerHTML = sorted
                .map((p, i) => {
                    const idx = pointsData.findIndex(x => x.name === p.name);
                    const cls = p.is_dpp ? 'dpp' : 'non-dpp';
                    return `<li class="${cls}" onclick="showDetails(${idx})" oncontextmenu="toggleDPP(event, ${idx})">${p.name}</li>`;
                })
                .join('');
        }

        function toggleDPP(event, idx) {
            event.preventDefault();
            const p = pointsData[idx];
            p.is_dpp = !p.is_dpp;

            const newSizes = pointsData.map(p => p.is_dpp ? 12 : 6);
            const newLineWidths = pointsData.map(p => p.is_dpp ? 2 : 0.5);
            const newLineColors = pointsData.map(p => p.is_dpp ? '#000' : '#fff');
            Plotly.restyle('plot', {
                'marker.size': [newSizes],
                'marker.line.width': [newLineWidths],
                'marker.line.color': [newLineColors]
            });

            updateList();
            showDetails(idx);
        }

        function showDetails(idx) {
            const p = pointsData[idx];
            const detailsDiv = document.getElementById('details');
            detailsDiv.classList.remove('empty');

            const escapeHtml = (str) => str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
            const status = p.is_dpp ? 'DPP SELECTED' : '';
            const btnClass = p.is_dpp ? 'toggle-btn remove' : 'toggle-btn';
            const btnText = p.is_dpp ? 'Remove from selection' : 'Add to selection';

            const actionPreview = p.sample_actions
                .slice(0, 15)
                .map(a => escapeHtml(a.replace(/\\n/g, ' | ').substring(0, 300) + (a.length > 300 ? '...' : '')) + ' ||')
                .join('\\n');

            const codePreview = p.code ? escapeHtml(p.code) : '(no code available)';

            let nameHtml;
            if (taigaUrlTemplate) {
                const taigaUrl = taigaUrlTemplate.replace(/\\$\\{p\\.name\\}/g, p.name);
                nameHtml = `<a href="${taigaUrl}" target="_blank" style="color:#0066cc">${p.name}</a>`;
            } else {
                nameHtml = p.name;
            }

            detailsDiv.innerHTML = `<b>${nameHtml}</b>  |  Wave: ${p.wave}  |  Actions: ${p.n_actions} (${p.n_unique} unique)  |  ${status}
<button class="${btnClass}" onclick="toggleDPP(event, ${idx})">${btnText}</button>

<b>━━━ Code ━━━</b>
${codePreview}

<b>━━━ Sample Actions (trajectory) ━━━</b>
${actionPreview}`;
        }

        function exportSelected(exportNonSelected = false) {
            const items = exportNonSelected
                ? pointsData.filter(p => !p.is_dpp).map(p => p.name)
                : pointsData.filter(p => p.is_dpp).map(p => p.name);
            const blob = new Blob([JSON.stringify(items, null, 2)], {type: 'application/json'});
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = exportNonSelected ? 'non_selected.json' : 'selected.json';
            a.click();
        }

        document.getElementById('plot').on('plotly_click', function(data) {
            showDetails(data.points[0].pointNumber);
        });

        updateList();

        async function computeUMAP() {
            if (isComputing) return;

            const nNeighbors = parseInt(document.getElementById('nn-slider').value);
            const minDist = parseInt(document.getElementById('md-slider').value) / 100;

            document.getElementById('status').textContent = `Computing UMAP (n=${nNeighbors}, d=${minDist.toFixed(2)})...`;
            isComputing = true;

            await new Promise(resolve => setTimeout(resolve, 10));

            try {
                const UMAPClass = (typeof UMAP !== 'undefined' && UMAP.UMAP) ? UMAP.UMAP : UMAP;
                const umap = new UMAPClass({
                    nComponents: 3,
                    nNeighbors: nNeighbors,
                    minDist: minDist,
                    spread: 1.0
                });

                const embedding = await umap.fitAsync(featureMatrix);

                const x = embedding.map(e => e[0]);
                const y = embedding.map(e => e[1]);
                const z = embedding.map(e => e[2]);

                Plotly.restyle('plot', {x: [x], y: [y], z: [z]});

                document.getElementById('status').textContent =
                    `Done (n=${nNeighbors}, d=${minDist.toFixed(2)})`;
            } catch (err) {
                document.getElementById('status').textContent = 'Error: ' + err.message;
                console.error(err);
            }

            isComputing = false;
        }

        document.getElementById('nn-slider').addEventListener('input', function() {
            document.getElementById('nn-value').textContent = this.value;
        });

        document.getElementById('md-slider').addEventListener('input', function() {
            const val = this.value / 100;
            document.getElementById('md-value').textContent = val.toFixed(2);
        });

        let debounceTimer = null;
        function debouncedCompute() {
            clearTimeout(debounceTimer);
            debounceTimer = setTimeout(computeUMAP, 300);
        }

        document.getElementById('nn-slider').addEventListener('change', debouncedCompute);
        document.getElementById('md-slider').addEventListener('change', debouncedCompute);
        document.getElementById('recompute-btn').addEventListener('click', computeUMAP);

        computeUMAP();
    </script>
</body>
</html>
'''
    return html


if __name__ == "__main__":
    main()
