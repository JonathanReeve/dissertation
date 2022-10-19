set db /run/media/jon/Sekurkopioj/Corpora
cat $db/ids | while read -l id
  echo Processing book with ID: $id
  sqlite3 $db/pg-text-7.db -batch "select text from text where id=$id" > /tmp/$id
  python nerFindColors.py /tmp/$id >> pg-analysis-results.jsonl
  rm /tmp/$id
end
