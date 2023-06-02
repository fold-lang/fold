/*

Roc lang: https://www.youtube.com/watch?v=6qzWm_eoUXM

Task.await (File.read "username.txt") \username ->
  Http.get "foo.com/\(username)"

username <- Task.await (File.read "username.txt")
Http.get "foo.com/\(username)"

*/