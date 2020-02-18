"""
Handle importing and processing of various color maps.
"""

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
            else:
                name, val, _ = line.split('\t')
                xkcdMap[name] = val
    return xkcdMap

xkcdMapRaw = open('../data/maps/xkcd/rgb.txt')
xkcdMap = makeXkcdMap(xkcdMapRaw)
