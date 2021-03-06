set db /run/media/jon/Sekurkopioj/Corpora

cat $db/ids | head -n 20 | while read -l id
  echo Processing book with ID: $id
  sqlite3 $db/pg-text-7.db -batch "select text from text where id=$id" > /tmp/$id
  ~/Dropbox/Columbia/Dissertation/04-colors/color-word-analyzer/color-word-analyzer-cli --colorMap=XKCD /tmp/$id >> pg-analysis-results
  rm /tmp/$id
end
