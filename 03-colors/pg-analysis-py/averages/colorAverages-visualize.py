import json
import sys
import argparse

def wordToLink(fn):
    """
    Take a filename like 1891-OneofOurConquerorsComplete-4476
    and turn it into a link to that book's Project Gutenberg page.
    """
    bookId = fn.split('-')[-1]
    url = f"http://www.gutenberg.org/ebooks/{bookId}"
    link = f"<a href={url}>{fn}</a>"
    return link

def visualizeModel(model, modelName):
    """
    Create an HTML file visualizing the mappings.
    """
    colorBlocksHtml = '\n'.join([ f"""
        <div class="colorBlock" style="background-color: #{hexCode};">{wordToLink(word)}</div>"""
                        for word, hexCode in model.items() ])
    css = """
        body { }
        div.blockWrapper { height: 40em; overflow-y: scroll; display: flex; flex-wrap: wrap; }
        div.colorBlock { width: 10em; height: 10em; word-wrap: break-word; }"
        a { background-color: white !important; color: black !important; text-shadow: none !important;} 
    """
    html = f"<html><head><style>{css}</style></head><body><div class=\"blockWrapper\">{colorBlocksHtml}</div></body></html>"

    with open(f'{modelName}-colorblocks.html', 'w') as f:
        f.write(html)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Visualize json averages.')
    parser.add_argument('model', type=str, default='averages.json',
                        help='the model to visualize')

    args = parser.parse_args()

    with open(args.model) as f:
        model = json.load(f)

    modelName = args.model.split('.')[0]

    visualizeModel(model, modelName)
