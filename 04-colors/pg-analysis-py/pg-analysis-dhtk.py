import sys
sys.path.append('/home/jon/Code/dhtk')
import dhtk
from dhtk.catalogs.gutenberg.data import GutenbergData
from dhtk.catalogs.gutenberg.book import GutenbergBook
from dhtk.catalogs.gutenberg.texts import GutenbergTexts
from lxml import etree as ET


gutenbergData = GutenbergData("http://dhtk.unil.ch:3030/gutenberg/sparql")


def getBritishLiterature():
    query = """
    PREFIX dcterms: <http://purl.org/dc/terms/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX purl: <http://purl.org/dc/terms/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX pgterms: <http://www.gutenberg.org/2009/pgterms/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX marcrel: <http://id.loc.gov/vocabulary/relators/>

    select ?book ?title ?author where {
    ?book dcterms:subject ?subj .
    ?subj <http://purl.org/dc/dcam/memberOf> dcterms:LCC .
    ?subj <http://www.w3.org/1999/02/22-rdf-syntax-ns#value> "PR" .
    ?book dcterms:title ?title .
    ?book dcterms:creator ?author .
    ?author pgterms:name ?authName .
    ?book dcterms:language ?lang .
    ?lang rdf:value "en"^^dcterms:RFC4646
    }
    """
    results = gutenbergData._get_query_results(query)

    bookIds = [item['book'] for item in results]

    return bookIds


# This is painfully slow.
def getBookObjs(bookIds):
    bookObjs = []
    for bookId in bookIds:
        print("Getting book: ", bookId)
        bookObj = gutenbergData.book_from_book_id(bookId)
        bookObjs.append(bookObj)
        print("Retrieved: ", bookObj)
    return bookObjs


bookIds = getBritishLiterature()

for bookId in bookIds:
    print("Getting book: ", bookId)
    bookObj = gutenbergData.book_from_book_id(bookId)
    print("Got object:", bookObj)
    pickle = bookObj.to_pickle()
    sanitizedId = bookId.split('/')[-1]
    with open(f"pickles/{sanitizedId}.pickle", "wb") as f:
        f.write(pickle)
    print("Done.")
