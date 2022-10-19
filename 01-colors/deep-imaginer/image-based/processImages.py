import colorgram
import argparse
from glob import glob
import json
import logging
import os

# My own module
import queryModel

def main(word, n):


    files = glob(f"../models/img/{word}/*")

    logging.info(f"Processing {len(files)} images for word {word}")

    colorGrams = [colorgram.extract(f, n) for f in files]

    def rgbToHex(colorObj):
        return '%02x%02x%02x' % colorObj.rgb

    # print(colorGrams)

    hexes = [[(rgbToHex(color), color.proportion) for color in colors] for colors in colorGrams]

    logging.info(f"Hexes: {hexes}")

    def dominantColors(hexes):
        """
        Get the dominant colors. Ignore proportions.
        """
        return [color[0][0] for color in hexes]

    def secondColors(hexes):
        """
        Assume the dominant color is in fact background. Get the second color instead.
        Ignore proportions.
        """
        return [item[1][0] for item in hexes]

    def mixColors(hexes):
        mixed = []
        for imageHexes in hexes:
            mixed.append(queryModel.combineHexes(dict(imageHexes)))
        return mixed

    # print(mixed)

    mixed = mixColors(hexes)
    dominant = dominantColors(hexes)
    try:
        secondary = secondColors(hexes)
    except IndexError:
        # Fall back on dominant colors if secondary
        # is not working. 
        secondary = dominant

    def bigBlock(processed):
        """
        We now have ten hexes, maybe, and we want to mix them all into one big block.
        """
        combined = queryModel.combineHexes({hexCode: 1/len(processed) for hexCode in processed})
        return combined

    jsonData = {}
    jsonData[word] = { "dominant-colors": dominant,
                            "dominant-mixed": bigBlock(dominant),
                            "mixed-colors": mixed,
                            "mixed-mixed": bigBlock(mixed),
                            "secondary-colors": secondary,
                            "secondary-mixed": bigBlock(secondary)
                            }


    # Make some HTML

    def bigBlockHtml(bigBlock):
        return f'<div style="display: inline-block; width: 100px; height: 100px; background-color: #{bigBlock}"></div>'

    def blockHtml(color):
        return f'<div style="display: inline-block; width: 50px; height: 50px; background-color: #{color}"></div>'

    def sectionHtml(technique):
        if technique == "mixed":
            colors = mixed
        elif technique == "dominant":
            colors = dominant
        elif technique == "secondary":
            colors = secondary
        html = f"<div><h2>Technique: {technique}</h2></div>"
        html += bigBlockHtml(bigBlock(colors))
        html += "\n".join([blockHtml(color) for color in colors])
        return html

    blocksHtml = '\n'.join([sectionHtml(technique) for technique in ["mixed", "dominant", "secondary"]])

    html = f"<html><body><h1>{word}</h1>{blocksHtml}</body></html>"

    with open(f'../models/html/{word}.html', 'w') as f:
        f.write(html)

    with open(f'../models/json/{word}.json', 'w') as f:
        f.write(json.dumps(jsonData))

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(description='Process photos and get the dominant color(s) for them.')
    parser.add_argument('--n', help='number of dominant colors to extract')
    parser.add_argument('--all', action='store_true', help='Whether to process all the color directories.')
    parser.add_argument('word', nargs='*', help='the word to query, if not all')
    args = parser.parse_args()

    if not args.n:
        n = 2
    else:
        n = int(args.n)

    if args.all:
        allImgDirs = os.listdir('img')
        logging.info(f'Processing all {len(allImgDirs)} image directories.')
        for imgDir in allImgDirs:
            logging.info(f'Processing images in dir {imgDir}')
            main(imgDir, n)
    else:
        main(args.word, n)
