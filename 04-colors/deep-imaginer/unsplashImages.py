import subprocess
import requests
import json
import argparse
import logging
import os

def main(word):
    logging.basicConfig(level=logging.INFO)

    # Load secrets.
    apikey = subprocess.getoutput("pass unsplash-api-key")

    params = {"client_id": apikey, "query": word }
    response = requests.get("https://api.unsplash.com/search/photos", params=params)

    urls = []
    if response.ok:
        text = response.text
        parsed = json.loads(text)
        for result in parsed['results']:
          try:
              urls.append(result['urls']['small'])
          except:
            print("Couldn't find small URL")
            continue
    else:
        exit('Response not OK')

    if len(urls) == 0:
        return False

    logging.info(f"Downloading {len(urls)} images.")

    os.system(f"mkdir -p 'img/{word}'")

    for url in urls:
        response = requests.get(url)
        if response.ok:
            file = open(f'img/{word}/{word}-{urls.index(url)}.jpg', 'wb')
            file.write(response.content)
            file.close()

    # print(urls)
    return True

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Get photos from unsplash for a keyword, and get their dominant colors.')
    parser.add_argument('word', help='the word to query')
    args = parser.parse_args()

    main(args.word)
