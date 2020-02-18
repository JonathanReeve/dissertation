"""
Find and annotate colors in a text.
"""

import altair as alt
import pandas as pd
import re
from matplotlib.colors import to_rgb, rgb_to_hsv
import argparse
import colorMaps
import plotly.graph_objects as go
import categorizeColors

class ColorText():
    def __init__(self, filename, colorMap, nChunks, nColors):
        self.filename = filename
        self.text = self.getText(filename)
        self.colorMap = colorMap
        self.annotatedText, self.matchLocs = self.annotateColors(self.text, colorMap)
        self.chunkedPlot = self.getChunkedPlot(self.text, colorMap, nChunks, nColors)
        self.df = self.matchesToDf(self.matchLocs)
        self.dfWithBase = self.df.append(self.baseColorDf)
        self.sunburstPlot = self.getSunburstPlot(self.dfWithBase)

    def getText(self, filename):
        with open(filename) as f:
            text = f.read()
        return text

    def annotateColors(self, text, colorMap):
        matchLocs = {}
        for item in colorMap:
            wordBoundary = '[\b\s\W]+'
            pattern = wordBoundary + item.replace(' ', '[-\s+]') + wordBoundary
            matches = re.finditer(pattern, text, flags=re.IGNORECASE)
            matchStarts =  [match.start() for match in matches if matches is not None]
            if matchStarts != []:
                matchLocs[item] = (len(matchStarts), matchStarts)
                color = colorMap[item]
                replacement = f' <span class="color" style="color: {color}">{item}</span> '
                text = re.sub(pattern, replacement, text, flags=re.IGNORECASE)
        return text, matchLocs

    def chunk(self, text, n=10):
        """ Split a text into N equal parts. """
        chunkSize = round(len(text)/n)
        return [text[i:i + chunkSize] for i in range(0, len(text), chunkSize)][:-1]

    def count(self, text, nChunks):
        chunks = self.chunk(text, nChunks)
        allCounts = []
        for thisChunk in chunks:
            matches = self.annotateColors(thisChunk, self.colorMap)[1] # {'dust': (3, [locations)
            counts = {color: pair[0] for color, pair in matches.items()}
            allCounts.append(counts)
        df = pd.DataFrame(allCounts)
        return df.fillna(0)

    def melt(self, df):
        df['chunk']=df.index
        return df.melt(id_vars='chunk', var_name='color', value_name='count')

    def plotM(self, df, nColors):
        """ Plots with matplotlib, via pandas. """
        df.plot(kind='area', stacked=True, color=["xkcd:"+color for color in topColors], figsize=(12,8))

    def plotA(self, df, colorMap):
        """ Plots with Altair """
        df['hex'] = df['color'].apply(lambda x: colorMap[x])
        df['hsv'] = df['hex'].apply(lambda x: rgb_to_hsv(to_rgb(x))[0])
        df = df.sort_values('hsv') # Sloppy alphabetical color sort
        return alt.Chart(df, width=800, height=600).mark_area().encode(
            x='chunk:O', y='count:Q', color=alt.Color('hex', scale=None), tooltip='color')

    def topColors(self, df, nColors=10):
        topColors = list(df.sum().sort_values(ascending=False)[:nColors].index)
        return df[topColors]

    def matchesToDf(self, matches):
        occurrences = [val[0] for val in matches.values()]
        locations = [val[1] for val in matches.values()]
        names = matches.keys()
        df = pd.DataFrame([names, occurrences, locations], index = ['name', 'n', 'locs']).T
        #print(df)
        # More stuff here

        df['hex'] = df['name'].apply(self.colorMap.get)
        df['parent'] = df['hex'].apply(lambda hex: categorizeColors.closestColor(hex, self.baseColorMap))
        df['parentHex'] = df['parent'].apply(self.baseColorMap.get)
        df['id'] = df['parent'] + '-' + df['name']
        return df

    def getChunkedPlot(self, text, colorMap, nChunks=20, nColors=10):
        """ Text text (string, loaded in) and returns a plot
        which can then be saved with plot.save()
        """
        df = self.count(text, nChunks)
        # print(df)
        topColorsOnlyDf = self.topColors(df, nColors)
        # print(topColorsOnlyDf)
        meltedDf = self.melt(topColorsOnlyDf)
        return self.plotA(meltedDf, colorMap)

    def writeChunkedPlot():
        self.chunkedPlot.save(self.filename + '-chunked.html')

    @property
    def baseColorMap(self):
        baseColorMap = {"red": "#FF0000", "orange": "#FFA500",
                        "yellow": "#FFFF00", "green": "#00FF00",
                        "blue": "#0000FF", "violet": "#EE82EE",
                        "grey": "#BEBEBE", "brown": "#A52A2A",
                        "black": "#000000", "white": "#FFFFFF"}
        # Actually, nevermind. Use the XKCD colors for the base colors, too.
        baseColorMap = {color: self.colorMap.get(color) for color in baseColorMap.keys()}
        return baseColorMap

    @property
    def baseColorDf(self):
        # print(baseColorMap)
        baseDf = pd.DataFrame(self.baseColorMap.items(), columns=['name', 'hex'])
        baseDf['parent'] = "color" # Ur-color
        baseDf['parentHex'] = baseDf['hex']
        baseDf['locs'] = ""
        baseDf['id'] = baseDf['name']
        totals = self.df.groupby(['parent'])['n'].sum()
        baseDf['n'] = baseDf['name'].apply(dict(totals).get)
        # Add one root color
        baseDf = baseDf.append({'name': 'color', 'hex': '#FFFFFF', 'id': 'color'
                                'parent': '', 'parentHex': '', 'n': 100}, ignore_index=True)
        return baseDf

    def getSunburstPlot(self, df):
        """
        Uses Plotly to make a Sunburst plot of all the colors,
        synchronically. Needs to be passed the data frame including
        the base colors.
        """
        # We need IDs, since we will probably have repeating names for things,
        # and this throws off Plotly

        fig = go.Figure()
        fig.add_trace(go.Sunburst(
            ids=df['id'],
            labels=df['name'],
            parents=df['parent'],
            values=df['n'],
            # domain=dict(column=1),
            #maxdepth=2,
            insidetextorientation='radial',
            marker = {"colors": df['hex']},  
            branchvalues='total'
        ))

        # fig.update_layout(
        #     margin = dict(t=10, l=10, r=10, b=10)
        # )

        # fig.show()
        return fig

    def writeSunburstPlot(self):
        self.sunburstPlot.write_html(self.filename + '-sunburst.html')

if __name__ == "__main__":
    # Make plot
    # pride = open('../data/text/pride.html').read()
    # print(pride[:200])
    parser = argparse.ArgumentParser()
    parser.add_argument("filename")
    args = parser.parse_args()
    filename = args.filename
    print('Analyzing file: ', filename)

    # Default colormap
    colorMap = colorMaps.xkcdMap

    # Initialize object
    colorText = ColorText(filename, colorMap, nChunks=40, nColors=20)

    colorText.dfWithBase.to_html(open(filename + '-df.html', 'w'))

    # colorText.writeChunkedPlot()
    colorText.writeSunburstPlot()
