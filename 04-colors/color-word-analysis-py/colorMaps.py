"""
Handle importing and processing of various color maps.
"""

import re
import pandas as pd

def makeXkcdMap(xkcdMapRaw):
    """
    Makes a color dictionary, e.g. {"blue": "#0000ff"}
    from the XKCD data set.
    """
    xkcdMap = {}
    for line in xkcdMapRaw:
        if not line.startswith('#'):
            splitLine = line.split('\t')
            if len(splitLine) != 3:
                print('error on: ', splitLine)
                continue
            else:
                name, val, _ = line.split('\t')
                xkcdMap[name] = val
    return xkcdMap

xkcdMapRaw = open('../data/maps/xkcd/rgb.txt')
xkcdMap = makeXkcdMap(xkcdMapRaw)

# Ridgway, via Jaffer
# <tr><td width="175*" nowrap>absinthe green<td title="120" style="background-color:#8A9A5B">120


def makeRidgwayMap(ridgwayRaw):
    """
    Makes a color dictionary, e.g. {"blue": "#0000ff"}
    from the XKCD data set.
    """
    ridgwayMap = {}
    for line in ridgwayRaw:
        if line.startswith('<tr><td'):
            color, colorHex = re.match('.*?nowrap>(.*?)<.*?color:(.*?)[";]', line).groups()
            ridgwayMap[color] = colorHex
    return ridgwayMap

ridgwayRaw = open('../data/maps/jaffer/ridgway.html')
ridgwayMap = makeRidgwayMap(ridgwayRaw)

def makeMasterMap(raw):
    masterMap = {}
    for line in raw:
        splitLine = line.split('\t')
        if len(splitLine) != 2:
            print('error on: ', splitLine)
            continue
        name, val = line.split('\t')
        masterMap[name] = val.strip()
    return masterMap

masterRaw = open('../data/maps/jaffer/master.tsv')
masterMap = makeMasterMap(masterRaw)
