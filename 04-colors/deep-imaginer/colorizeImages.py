import subprocess
import requests
import json
import argparse
import logging
import os
from bs4 import BeautifulSoup

def main(word):
    logging.basicConfig(level=logging.INFO)

    params = {"q": word}
    response = requests.get("https://alexbeals.com/projects/colorize/search.php", params=params)

    if response.ok:
        text = response.text
        # print(text)
        soup = BeautifulSoup(text, features='lxml')
        hexCode = soup.find("span", {"class": "hex"})
    else:
        exit('Response not OK')

    return hexCode.contents[0]

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Get photos from Colorize for a keyword.')
    parser.add_argument('word', help='the word to query')
    args = parser.parse_args()

    result = main(args.word)
    encoded = json.dumps(result)

    with open('colorize-results.jsonl', 'a') as f:
        f.write(encoded)

    print(result)
