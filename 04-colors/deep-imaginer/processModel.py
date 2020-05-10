import json

def processModel(model, method='max'):
    bestGuesses = {}
    for word, colorPairs in model.items():
        bestGuesses[word] = getMaxCombinedScoreHex(word, colorPairs)
    return bestGuesses

def getMaxCombinedScoreHex(word, colorPairs):
    """
    Add up all scores, return the hex code with the highest combined score
    """
    # First transform into a dictionary {hex: [list of scores]}
    wordHexes = {}
    for pair in colorPairs:
        hexCode, score = pair
        if hexCode.startswith('#'):
            hexCode = hexCode[1:]
        if hexCode in wordHexes:
            wordHexes[hexCode].append(score)
        else:
            wordHexes[hexCode] = [score]
    scoresSums = {hexCode: sum(scores) for hexCode, scores in wordHexes.items()}
    return max(scoresSums, key=scoresSums.get)

def visualizeModel(model):
    """
    Create an HTML file visualizing the mappings.
    """
    colorBlocksHtml = '\n'.join([ f"""
        <div style="display: inline-block;
                    text-align: center;
                    width: 100px; height: 100px;
                    background-color: #{hexCode};">{word}</div>"""
                        for word, hexCode in model.items() ])
    html = f"<html><body>{colorBlocksHtml}</body></html>"

    with open(f'processed-model-colorblocks.html', 'w') as f:
        f.write(html)

if __name__ == '__main__':
    # TODO: read in existing file
    model = processModel()
    with open('processedModel.json', 'w') as f:
        json.dump(model, f)

    visualizeModel(model)
