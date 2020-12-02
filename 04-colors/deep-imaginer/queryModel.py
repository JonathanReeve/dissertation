import json
import argparse
import logging
import plotly.express as px
import pandas as pd

# This should now be a dictionary with color words as keys,
# e.g. {"blue": {"word": score ...} ... }

dataDir = "../data"

def query(word, model):
    # Construct a dictionary like {"word": {color: score, color: score...} }
    wordDict = {word: {}}
    for color in model:
        if word in model[color]:
            wordDict[word][color] = model[color][word]
    sortedColors = {k: v for k, v in sorted(wordDict[word].items(), key=lambda x: x[1], reverse=True)}
    mixedColors = mix(sortedColors) 
    wordDict[word] = {"colors": sortedColors, "mixed": mixedColors}
    return wordDict

def combineHexes(colorDict):
    """
    Proportionally mix hex colors from dict like {"ffffff": 1.0, "0000ff": 0.5 ... }
    """
    colors = sorted(colorDict.items())
    weights = sum(colorDict.values())
    if weights == 0:
        return
    red = int(sum([int(k[:2], 16)*v for k, v in colors])/weights)
    green = int(sum([int(k[2:4], 16)*v for k, v in colors])/weights)
    blue = int(sum([int(k[4:6], 16)*v for k, v in colors])/weights)
    zpad = lambda x: x if len(x)==2 else '0' + x
    return zpad(hex(red)[2:]) + zpad(hex(green)[2:]) + zpad(hex(blue)[2:])

def mix(colorDict):
    """
    Given a dictionary like {'grey': 0.333, 'coral': 0.33, 'ocean': 0.33, ... },
    Mix these colors proportionally according to those scores.
    """
    colorDict = {lookup(color): proportion for color, proportion in colorDict.items()}
    return combineHexes(colorDict)

def xkcdMap(xkcdRaw):
    lines = xkcdRaw.split('\n')
    colorDict = {}
    for line in lines:
        splitLine = line.split('\t')
        if len(splitLine) < 2:
            continue
        color, hexCode = line.split('\t')[:2]
        color = color.strip()
        hexCode = hexCode.strip()[1:]
        colorDict[color] = hexCode
    # print(colorDict)
    return colorDict

def lookup(color):
    return colorMap.get(color, 'dddddd')

def piechart(modelData, word, model):
    """
    Takes JSON in this format:
    {"green": 380.1666666666666,
     "white": 71.58333333333333,
     "sea": 63.31666666666667,
     "stone": 49.166666666666686,
     "sky": 41.083333333333336,
     "yellow": 39.416666666666664,
     "grey": 36.38333333333333,
     "blue": 33.21666666666667
     [ ... ]
    }

    And makes a pie chart.
    """
    labeled = [{"name": name, "value": value} for name, value in modelData.items()]

    df = pd.DataFrame(labeled)
    # df = pd.DataFrame(s, columns=['names', 'values'])
    # df['hex'] = df['colors'].apply(lookup)
    df.loc[df['value'] < (0.02*df['value'].sum()), 'name'] = 'Other'
    colors = ['#' + lookup(color) for color in df['name']]
    fig = px.pie(df, values='value', names='name', color='name',
                 color_discrete_sequence=colors, width=600, height=600)
    fig.update_traces(textposition='inside', textinfo='percent+label')
    open(f'charts/{word}-{model}-pie.html', 'w').write(fig.to_html())

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(description='Query the color word association model.')
    parser.add_argument('--colorMap', help='the color map to use. Default: XKCD rgb.txt')
    parser.add_argument('--model', help='the model to use. Default: model.json in this directory.')
    parser.add_argument('--pieChart', help='Output a pie chart to html, instead of printing JSON.',
                        action='store_true')
    parser.add_argument('word', help='the word to query')
    args = parser.parse_args()

    if not args.model:
        with open('model.json') as f:
            model = json.load(f)
    else:
        with open(args.model) as f:
            model = json.load(f)

    if args.colorMap:
        colorMap = args.colorMap
    else:
        xkcdRaw = open(f"{dataDir}/maps/xkcd/rgb.txt").read()
        colorMap = xkcdMap(xkcdRaw)

    queryOut = query(args.word, model)
    queryData = queryOut[args.word]['colors']

    modelNiceName = args.model.split('/')[-1].split('.')[0]

    if args.pieChart:
        piechart(queryData, args.word, modelNiceName)
    else:
        print(queryOut)
