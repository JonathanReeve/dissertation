import os
import os.path

import cherrypy
from cherrypy.lib import static

#HTML DSL
import dominate
from dominate.tags import *
from dominate.util import raw

import hashlib

# NLP
# import spacy

import findColors
import colorMaps

localDir = os.path.dirname(__file__)
absDir = os.path.join(os.getcwd(), localDir)

class FileDemo(object):
    uploadForm = form("Filename: ",
                    input(text="Select file", type="file", name="myFile"),
                    input("Upload", type="submit"),
                    action="upload", method="post", enctype="multipart/form-data")

    doc = html(
            body(
                h2("Upload a file"),
                uploadForm)).render()

    @cherrypy.expose
    def index(self):
        return self.doc

    @cherrypy.expose
    def upload(self, myFile):

        if str(myFile.content_type) != "text/plain":
            return "File type is: " + str(myFile.content_type) + \
                "Error: uploaded file is not a plain text file." + \
                "I can only handle .txt plain text files."

        cherrypy.log('Reading file...')
        fileContents = ""
        while True:
            data = myFile.file.read(8192)
            if not data:
                break
            fileContents += str(data, encoding='utf-8')

        cherrypy.log('Analyzing file...')
        # doc = nlp(fileContents)

        filename = myFile.filename

        analysisOutput = AnalysisOutput(filename, fileContents)

        analysisHtml = analysisOutput.getAnalysis(filename, fileContents)

        # text, matches = findColors.annotateColors(fileContents)

        # Then do something else with the doc here.

        return analysisHtml


class AnalysisOutput():
    def __init__(self, filename, fileContents):
        self.filename = filename
        self.fileContents = fileContents
        self.fileHash = hashlib.sha256(fileContents.encode('utf-8')).hexdigest()[:16]
        cwd = os.getcwd()
        self.outFile = f"{cwd}/analysis-cache/{self.fileHash}.html"

    def getAnalysis(self, filename, fileContents):
        """
        Check if we've already done this analysis,
        by checking the cache folder for a file with that hash.
        If we haven't, make a new analysis.
        """

        cherrypy.log(f"Looking to see if we've already analyzed {self.outFile}")
        if os.path.isfile(self.outFile):
            with open(self.outFile) as f:
                html = f.read()
                return html
        else:
            cherrypy.log(f"Looks like we haven't already analyzed {self.outFile}")
            html = self.makeHtml(filename, fileContents)
            # Save it for later
            self.writeHtml(html)

        return html

    def makeHtml(self, filename, fileContents):
        """
        Make analysis output HTML file.
        Now has three things:
        - File metadata (filename, etc)
        - Sunburst chart
        - Area chart
        """
        label = "label" # TODO
        nChunks = 40
        nColors = 10
        colorMap = colorMaps.xkcdMap
        text = findColors.ColorText(filename, fileContents, label, colorMap, nChunks, nColors)

        doc = html(
                # head(
                #     ) 
                body(
                    div(
                        h1(text.label),
                        section(
                            h2('Overview'),
                            div(
                                raw(text.sunburstPlotHtml),
                                class_="sunburst"
                            )
                        ),
                        section(
                            h2('Narrative Timeseries'),
                            div(
                                raw(text.chunkedPlotHtml),
                                class_="chunked"
                                )
                        ),
                        section(
                            h2('Annotated Text'),
                            div(
                                raw(text.annotatedText),
                                class_="annotated"
                                ),
                        ),
                    class_="container")
                )
              ).render()
        return doc

    def writeHtml(self, fileContents):
        """ Write the resulting HTML """
        cherrypy.log(f"Writing to: {self.outFile}")
        with open(self.outFile, 'w') as f:
            f.write(fileContents)


if __name__ == '__main__':
    # CherryPy always starts with app.root when trying to map request URIs
    # to objects, so we need to mount a request handler root. A request
    # to '/' will be mapped to HelloWorld().index().
    # print('Starting SpaCy...')
    # nlp = spacy.load('en_core_web_md')

    cherrypy.quickstart(FileDemo()) #, config=tutconf)
