{
  files[$1][length(files[$1])] = $0
}

END {
  for (hash in files) {
    if (length(files[hash]) > 1) {
      print hash;
      for (idx in files[hash]) {
        print " " files[hash][idx];
      }
    }
  }
}