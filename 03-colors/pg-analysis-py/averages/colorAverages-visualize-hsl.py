import json
import plotly.express as px
import pandas as pd
import logging
import argparse
import random

from colormath.color_objects import sRGBColor, HSLColor
from colormath.color_conversions import convert_color


def readHex(colorHex):
    return sRGBColor(0, 0, 0).new_from_rgb_hex(colorHex)

def visualizeModel(avgs, colorSpace="HSL"):
    modelDict = {}
    for fn, colorHex in avgs.items():
        obj = readHex(colorHex)
        if colorSpace == "RGB":
            modelDict[fn] = {"red": obj.rgb_r, "green": obj.rgb_g,
                             "blue": obj.rgb_b, "text": fn}
        elif colorSpace == "HSL":
            obj = convert_color(obj, HSLColor)
            modelDict[fn] = {"hue": obj.hsl_h, "saturation": obj.hsl_s,
                             "luminosity": obj.hsl_l, "text": fn}
    rgbArgs = {'x': 'red', 'y': 'green', 'z': 'blue'}
    hslArgs = {'x':'hue', 'y':'saturation', 'z':'luminosity'}
    args = hslArgs if colorSpace == "HSL" else rgbArgs
    return modelDict, args

# print(dir(fig))

def subset(avgs):
    byYear = {}
    for fn, val in avgs.items():
        year = fn[:4]
        if year not in byYear:
            byYear[year] = [fn]
        else:
            byYear[year].append(fn)
    subset = []
    for year, fns in byYear.items():
        subset.append(random.choice(fns))
    return {fn: avgs[fn] for fn in subset}

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    with open('averages.json') as f:
        avgs = json.load(f)

    # model = {word: '#' + colorHex for word, colorHex in model.items()
    #          if colorHex is not None}

    colorSpace = 'HSL'

    avgs = subset(avgs)

    modelDict, args = visualizeModel(avgs, colorSpace)

    avgs = {word: '#' + colorHex for word, colorHex in avgs.items()
              if colorHex is not None}

    df = pd.DataFrame(modelDict).T
    df = df.head(100)
    fig = px.scatter_3d(df, **args, text="text",
                        color="text", color_discrete_map=avgs)

    url = f"colorAverage-{colorSpace}-3D.html"
    fig.write_html(url, include_plotlyjs=True)
