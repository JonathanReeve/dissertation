import logging
import sqlite3

logging.basicConfig(format='%(asctime)s %(message)s', level=logging.DEBUG)
dataLocation = '/media/jon/Sekurkopioj/Corpora'

conn = sqlite3.connect(f"{dataLocation}/pg-text-7.db")
c = conn.cursor()

# Get only those books with Library of Congress Category "PR"
# (British Literature), and which are written in English.
# c.execute('select id from meta where LCC like "%PR%" and languages like "%en%";')
c.execute(
    """select id from meta
    where title like "%Encyclopaedia%"
    """)
idList = [item[0] for item in c.fetchall()]
