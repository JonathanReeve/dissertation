import os
import os.path

import cherrypy
from cherrypy.lib import static

#HTML DSL
import dominate
from dominate.tags import *

# NLP
# import spacy

import findColors

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
        out = html(
                body(
                  """Stats for file: %s<br />
                  myFile mime-type: %s
                  contents: %s
                  """
                    ))

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
        text, matches = findColors.annotateColors(fileContents) 

        # Then do something else with the doc here.

        return text


if __name__ == '__main__':
    # CherryPy always starts with app.root when trying to map request URIs
    # to objects, so we need to mount a request handler root. A request
    # to '/' will be mapped to HelloWorld().index().
    # print('Starting SpaCy...')
    # nlp = spacy.load('en_core_web_md')

    cherrypy.quickstart(FileDemo()) #, config=tutconf)
