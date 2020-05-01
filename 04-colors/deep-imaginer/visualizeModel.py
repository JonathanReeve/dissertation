import json

with open('colorize-results.json') as f:
    modelRaw = f.read()

model = json.loads(modelRaw)

colorBlocksHtml = '\n'.join([ f"""
    <div style="display: inline-block;
                text-align: center;
                width: 100px; height: 100px;
                background-color: {hexCode};">{word}</div>"""
                    for word, hexCode in model.items() ])
html = f"<html><body>{colorBlocksHtml}</body></html>"

with open('colorblocks.html', 'w') as f:
    f.write(html)
