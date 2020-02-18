
from colormath.color_objects import sRGBColor, LabColor
from colormath.color_conversions import convert_color
from colormath.color_diff import delta_e_cie2000

from colorMaps import xkcdMap

def colorDelta(colorAHex, colorBHex):
    """ Compute delta-E measure of color difference,
    using the CIE Lab color space.
    """
    srgbA = sRGBColor.new_from_rgb_hex(colorAHex)
    srgbB = sRGBColor.new_from_rgb_hex(colorBHex)

    colorAlab = convert_color(srgbA, LabColor);
    colorBlab = convert_color(srgbB, LabColor);

    return delta_e_cie2000(colorAlab, colorBlab)

# print(colorDelta('#ff0000', '#00ff00'))

baseColors = ["red", "orange", "yellow", "green", "blue",
              "violet", "grey", "brown", "black", "white"]

# Old values from the X11 color set. Don't use.
# baseColorMap = {"red": "#FF0000", "orange": "#FFA500",
#                 "yellow": "#FFFF00", "green": "#00FF00",
#                 "blue": "#0000FF", "violet": "#EE82EE",
#                 "grey": "#BEBEBE", "brown": "#A52A2A",
#                 "black": "#000000", "white": "#FFFFFF"}


def makeBaseColorMap(colorMap = xkcdMap):
    return {color: colorMap.get(color) for color in baseColors}

baseColorMap = makeBaseColorMap(xkcdMap)

def closestColor(colorHex, baseColorMap):
    scores = {colorDelta(colorHex, baseHex): word
              for word, baseHex in baseColorMap.items()}
    return scores[min(scores)]
