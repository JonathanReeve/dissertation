import json
import plotly.express as px
import pandas as pd
import logging
import argparse

from colormath.color_objects import sRGBColor, HSLColor
from colormath.color_conversions import convert_color


def readHex(colorHex):
    return sRGBColor(0, 0, 0).new_from_rgb_hex(colorHex)

def visualizeModel(colorSpace="HSL"):
    modelDict = {}
    for word, colorHex in model.items():
        obj = readHex(colorHex)
        if colorSpace == "RGB":
            modelDict[word] = {"red": obj.rgb_r, "green": obj.rgb_g,
                               "blue": obj.rgb_b, "text": word}
        elif colorSpace == "HSL":
            obj = convert_color(obj, HSLColor)
            modelDict[word] = {"hue": obj.hsl_h, "saturation": obj.hsl_s,
                               "luminosity": obj.hsl_l, "text": word}
    rgbArgs = {'x': 'red', 'y': 'green', 'z': 'blue'}
    hslArgs = {'x':'hue', 'y':'saturation', 'z':'luminosity'}
    args = hslArgs if colorSpace == "HSL" else rgbArgs
    return modelDict, args

# print(dir(fig))


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(description='Visualize a model using a 3D scatter plot.')
    parser.add_argument('--model', help='The model to visualize')
    parser.add_argument('--colorSpace', help='The color space to use.', default='RGB')

    args = parser.parse_args()

    if args.model:
        with open(args.model) as f:
            model = json.load(f)
    else:
        defaultModel = '../models/downloaded-images-model.json'
        logging.info(f'Using model {defaultModel}')
        with open(defaultModel) as f:
            model = json.load(f)

    model = {word: '#' + colorHex for word, colorHex in model.items()
             if colorHex is not None}

    modelDict, args = visualizeModel(colorSpace=args.colorSpace)

    df = pd.DataFrame(modelDict).T
    df = df.head(100)
    fig = px.scatter_3d(df, **args, text="text",
                        color="text", color_discrete_map=model)

    fig.write_html('image-based-vector-visualization.html', include_plotlyjs=False)
