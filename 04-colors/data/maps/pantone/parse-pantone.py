import json
import pandas as pd

with open('pantone-colors.json') as f: 
    parsed = json.load(f)

df = pd.DataFrame(parsed)

print(df.to_csv(sep='\t', index=False))
