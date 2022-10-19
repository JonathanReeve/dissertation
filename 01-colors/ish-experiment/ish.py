import pandas as pd
import plotly.express as px

xkcdLines = open('../data/maps/xkcd/rgb.txt').readlines()
colorsOnly = list(set([line.split('\t')[0] for line in xkcdLines]))
xkcdDict = {line.split('\t')[0]: line.split('\t')[1] for line in xkcdLines}

notIshey = ["british", "irish"]
additionalIshey = [ "purpley", "bluey", "yellowy", "pinkey", 'greeney', 'browney' ]
isheyOnly = [color for color in colorsOnly
             if any([w.endswith('ish')
                     or w in additionalIshey
                     for w in color.split()])
             and color.split()[0] not in notIshey
             ]

isheyTwoWords = [color for color in isheyOnly if len(color.split()) > 1]

open('wordList.txt', 'w').write(','.join(isheyTwoWords))

# Initialize an skeleton dictionary
byFirsts = {color.split()[0]: [] for color in isheyOnly} 
for color in isheyOnly:
    firstWord = color.split()[0]
    if firstWord in byFirsts:
        byFirsts[firstWord].append(color)


lastWords = [color.split()[-1] for color in isheyOnly]

byLastsToo = {color.split()[0]: {} for color in isheyOnly} 
for rowName, colorList in byFirsts.items():
    for color in colorList: 
        lastWord = color.split()[-1]
        for w in lastWords:
            if lastWord == w and color != w:
                byLastsToo[rowName][lastWord] = color
            
# print(byLastsToo)
        
df = pd.DataFrame(byLastsToo)


def plotStats(data, label):
    applicabilityCounts = pd.DataFrame(data, columns=[label])
    applicabilityCounts['colorName'] = applicabilityCounts.index
    applicabilityCounts = applicabilityCounts.sort_values(label)
    categoryOrders = {'colorName': list(applicabilityCounts.index)}
    hexVals = [xkcdDict.get(color, '#ffffff') for color in applicabilityCounts.index]
    fig = px.bar(applicabilityCounts, x='colorName', y=label, color='colorName',
                 color_discrete_sequence=hexVals, category_orders=categoryOrders)
    fig.write_html(f'{label}.html')

plotStats(df.count(), 'applicability')
colorCounts = df.count(axis=1)
colorCounts2 = colorCounts[colorCounts > 1]
plotStats(colorCounts2, 'ishability')

def getCssMaybe(colorName):
    colorHex = xkcdDict.get(colorName, '')
    return '' if colorHex == '' else f'style="background-color: {colorHex};"'

# Put them in order
colNames = df.columns

out = ""
out += '<table><thead><th>color</th>'
for col in df.iloc[0].index:
    out += f'<th {getCssMaybe(col)}>{col}</th>'
out += '</thead><tbody>'
for i, row in df.iterrows():
    out += f'<tr><td {getCssMaybe(i)}>{i}</td>'
    for col in colNames: # index
        out += f'<td class="{col}" {getCssMaybe(row[col])}>{row[col]}</td>'
    out += f'</tr>'
out += '</tbody></table>'

with open('ish-matrix.html', 'w') as f:
    f.write(out)
