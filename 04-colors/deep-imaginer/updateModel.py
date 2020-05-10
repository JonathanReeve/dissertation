import json
from glob import glob
import os
import logging

import queryModel
import processModel

def readModel(technique="downloaded", algo="secondary"):
    if technique == "colorize":
        with open('colorize-model.json') as f:
            model = json.load(f)

    if technique == "downloaded":
        filesList = glob("json/*.json")
        model = {}
        for fn in filesList:
            with open(fn) as f:
                jsonData = json.load(f)
            word = list(jsonData.keys())[0]
            color = jsonData[word][f"{algo}-mixed"]
            if color != "" and color is not None:
                model[word] = color

    return model

def visualizeModel(model, technique="downloaded", algo="secondary"):
    """
    Create an HTML file visualizing the mappings.
    """

    colorBlocksHtml = '\n'.join([ f"""
        <div style="display: flex;">
          <div class="colorBlock" style="display: inline-block;
                                         text-align: center;
                                         width: 100px; height: 100px;
                                         background-color: {hexCode};">
            {word}
          </div>
          <div class="images" style="display: inline-block;">
            <img src="img/{word}/pixabay-{word}-0.jpg" style="width: 100px; height: 100px;" />
            <img src="img/{word}/pixabay-{word}-1.jpg" style="width: 100px; height: 100px;" />
            <img src="img/{word}/pixabay-{word}-2.jpg" style="width: 100px; height: 100px;" />
            <img src="img/{word}/pixabay-{word}-3.jpg" style="width: 100px; height: 100px;" />
            <img src="img/{word}/pixabay-{word}-4.jpg" style="width: 100px; height: 100px;" />
          </div>
        </div>
        """
                        for word, hexCode in model.items() ])
    html = f"<html><body>{colorBlocksHtml}</body></html>"

    with open(f'{technique}-{algo}-colorblocks.html', 'w') as f:
        f.write(html)

def updateModel(newModel, oldModelFn='model.json', combinedModelFn='combinedModel.json'):
    """
    Update the model.json file.
    """

    with open(oldModelFn) as f:
        oldModel = json.load(f)

    # Old model is based on XKCD color words,
    # so we have to look those up.
    with open('../data/maps/xkcd/rgb.txt') as f:
        xkcdRaw = f.read()
    xkcdDict = queryModel.xkcdMap(xkcdRaw)

    # Load any existing combined data
    try:
      with open(combinedModelFn) as f:
          try:
              combinedModel = json.load(f)
              # print(f"Combinedmodel: {combinedModel}")
          except json.decoder.JSONDecodeError:
              logging.info("Error in decoding existing model.")
              combinedModel = {}
    except FileNotFoundError:
        open(combinedModelFn, 'a').close() # Create an empty file
        combinedModel = {}

    # Parse old model into new format
    for color in oldModel:
        hexCode = xkcdDict.get(color)
        for word, score in oldModel[color].items():
            if word in combinedModel:
                combinedModel[word].append((hexCode, score))
            else:
                combinedModel[word] = [(hexCode, score)]

    # New model is just a {word: hex} pair
    for word, hexCode in newModel.items():
        if word in combinedModel:
            combinedModel[word].append((hexCode, score))
        else:
            combinedModel[word] = [(hexCode, score)]

    with open(combinedModelFn, 'w') as f:
        # print(combinedModel)
        json.dump(combinedModel, f)

    return combinedModel

def normalizeDictionaryModel(oldModel, colorMap):
    # Parse old model into new format
    newFormat = {}
    for color in oldModel:
        hexCode = colorMap.get(color)
        for word, score in oldModel[color].items():
            word = word.lower()
            if word in newFormat:
                newFormat[word].append((hexCode, score))
            else:
                newFormat[word] = [(hexCode, score)]

    normalized = processModel.processModel(newFormat)
    logging.info(f"Normalized dictionary model: {str(normalized)[:50]}")
    return normalized

def cascadeModels():
    """
    Use a cascading algorithm to combine models.
    1. Is it in XKCD? Go with that one first.
    2. Is it in Jaffer master? Go with that.
    3. Is it in the dictionary mapping? Go with that.
    4. Is it in the Pantone mapping? Go with that.
    5. Is it in the downloaded images map? Go with that.

    Accomplish this by building a dictionary starting from #4
    and working our way up. Each step overwrites the last,
    therefore giving priority to it.
    """

    model = {}

    with open('downloaded-images-model.json') as f:
        imagesModel = json.load(f)
    model.update(imagesModel)

    with open('../data/maps/pantone/pantone-model.json') as f:
        pantoneModel = json.load(f)
    pantoneExtended = extendPantone(pantoneModel)
    model.update(pantoneExtended)

    with open('../data/maps/xkcd/rgb.txt') as f:
        xkcdRaw = f.read()
    xkcdDict = queryModel.xkcdMap(xkcdRaw)

    with open('dictionaryModel.json') as f:
        dictionaryModel = json.load(f)
    dictionaryModel = normalizeDictionaryModel(dictionaryModel, xkcdDict)
    logging.info(f"Dictionary model normalized preview: {str(dictionaryModel)[:50]}")
    model.update(dictionaryModel)

    with open('../data/maps/jaffer/master.json') as f:
        jafferModel = json.load(f)
    model.update(jafferModel)

    with open('../data/maps/xkcd/rgb.json') as f:
        xkcdModel = json.load(f)
    model.update(xkcdModel)

    with open('cascadeModel.json', 'w') as f:
        json.dump(model, f)

    return model

def extendPantone(pantoneModel):
    """
    Pantone colors often contain base color names, e.g. "oyster white." Let's
    remove those base color names so that we can recognize "oyster" instead.
    """
    baseColors = ['black', 'gray', 'grey', 'brown', 'white', 'red', 'orange',
                  'yellow', 'green', 'blue', 'indigo', 'violet', 'mauve']
    extendedModel = {}
    for color in pantoneModel:
        colorSpaces = color.replace('-', ' ')
        splitColor = colorSpaces.split()
        if splitColor[-1] in baseColors:
            newColor = ' '.join(splitColor[:-1])
            extendedModel[newColor] = pantoneModel[color]
        else:
            extendedModel[colorSpaces] = pantoneModel[color]
    return extendedModel

def processColorizeAndImageModel():
    colorizeModel = readModel(technique="colorize")
    downloadedModel = readModel(technique="downloaded")
    combinedModel = updateModel(colorizeModel)
    combinedModel = updateModel(downloadedModel)
    visualizeModel(combinedModel)

def processImageModel():
    downloadedModel = readModel(technique="downloaded")
    with open('downloaded-images-model.json', 'w') as f:
        json.dump(downloadedModel, f)
    visualizeModel(downloadedModel)

def jsonToHs():
    with open('cascadeModel.json') as f:
        model = json.load(f)
    for k, v in model.items():
        print(f'("{k}", "{v}"),')

if __name__ == "__main__":
    # processImageModel()
    # cascadeModels()
    jsonToHs()

