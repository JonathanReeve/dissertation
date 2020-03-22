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
import logging

class ColorText():
    def __init__(self, filename, fileContents, label, colorMap, nChunks, nColors):
        self.filename = filename
        self.label = label
        self.colorMap = colorMap
        self.text = fileContents
        self.annotatedText, self.matchLocs = self.annotateColors(self.text, colorMap)
        self.df = self.matchesToDf(self.matchLocs)
        self.dfWithBase = self.df.append(self.baseColorDf)

        # self.contexts = self.getMatchContexts(self.text, self.matchLocs)
        self.chunkedPlot = self.getChunkedPlot(self.text, colorMap, nChunks, nColors)
        self.chunkedPlotHtml = self.chunkedPlot.to_html(fullhtml=False)

        self.sunburstPlot = self.getSunburstPlot(self.dfWithBase)
        self.sunburstPlotHtml = self.getSunburstHtml()

    def getText(self, filename):
        with open(filename) as f:
            text = f.read()
        return text

    def getMatchContexts(self, text, matchLocs, context=10):
        """
        Returns a dictionary with colorwords and a list of their contexts.
        Ex: {"red": ["she wore a <b>red</b> dress", ...] ... }
        Context is number of characters of context to be included before and after.

        MatchLocs should look like {'grey blue': (3, [(3694, 3705), ...])}
        """
        contexts = {color: [] for color in matchLocs}
        for color, data in matchLocs.items():
            print(f"Color: {color} data: {data}")
            n, spanList = data
            for span in spanList:
                # print(f"Spanlist: {spanList}")
                start, end = span
                # beforeIdx = (start - context) if (start - context > 0) else 0
                # afterIdx = (end + context) if ((end + context) < len(text)) else len(text)
                # textWithContext = text[beforeIdx:afterIdx]
                textWithContext = text[span[0]:span[1]] # .replace('\n', ' ') # Debugging
                print(f"TextWithContext: {textWithContext}")
                contexts[color].append(textWithContext)
        return contexts

    def annotateColors(self, text, colorMap):
        matchLocs = {}
        for item in colorMap:
            wordBoundary = '[\b\s\W]+'
            pattern = wordBoundary + '(' + item.replace(' ', '[-\s+]') + ')' + wordBoundary
            matches = re.finditer(pattern, text, flags=re.IGNORECASE | re.MULTILINE)
            if matches is not None:
                matchSpans =  [match.span(1) for match in matches]
            else:
                matchSpans = []
            if matchSpans != []:
                # matchTexts = [match.group(0) for match in matches]
                matchLocs[item] = (len(matchSpans), matchSpans)
                color = colorMap[item]
                replacement = f' <span class="color" style="color: {color}">\\1</span> '
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

    def writeChunkedPlot(self):
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
        rootColorName = self.label
        baseDf['parent'] = rootColorName
        baseDf['parentHex'] = baseDf['hex']
        baseDf['locs'] = ""
        baseDf['id'] = baseDf['name']
        totals = self.df.groupby(['parent'])['n'].sum()
        baseDf['n'] = baseDf['name'].apply(dict(totals).get)
        # Add one root color
        grandTotal = baseDf['n'].sum()
        baseDf = baseDf.append({'name': rootColorName, 'hex': '#FFFFFF', 'id': rootColorName,
                                'parent': '', 'parentHex': '', 'n': grandTotal}, ignore_index=True)
        return baseDf

    def getSunburstPlot(self, df):
        """
        Uses Plotly to make a Sunburst plot of all the colors,
        synchronically. Needs to be passed the data frame including
        the base colors.
        """

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
            branchvalues='total',
            hovertemplate=''
        ))

        # fig.update_layout(
        #     margin = dict(t=10, l=10, r=10, b=10)
        # )

        # fig.show()
        return fig

    def getSunburstHtml(self):
        outFilename = self.filename + '-sunburst.html'
        logging.info(f"Wrote to {outFilename}")
        html = self.sunburstPlot.to_html(outFilename, full_html=False)
        # print("---sunburstPlot---", self.sunburstPlot)
        # print("---PLOT---", plot)
        return html

    def writeSunburstPlot(self):
        outFilename = self.filename + '-sunburst.html'
        self.sunburstPlot.write_html(outFilename)
        logging.info(f"Wrote to {outFilename}")

    def writeCSV(self):
        outFilename = self.filename + '-colors.csv'
        self.dfWithBase.to_csv(outFilename)
        logging.info(f"Wrote to {outFilename}")


if __name__ == "__main__":
    # Make plot
    # pride = open('../data/text/pride.html').read()
    # print(pride[:200])
    parser = argparse.ArgumentParser()
    parser.add_argument("filename")
    parser.add_argument("label")
    args = parser.parse_args()
    filename = args.filename
    label = args.label
    print('Analyzing file: ', filename)

    # Default colormap
    colorMap = colorMaps.xkcdMap

    # Initialize object
    with open(filename) as f:
        fileContents = f.read()

    colorText = ColorText(filename, fileContents, label, colorMap, nChunks=40, nColors=20)

    # colorText.writeCSV()

    # print(colorText.annotatedText)

    # colorText.writeChunkedPlot()
    # colorText.writeSunburstPlot()
