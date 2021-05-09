# Package

version       = "0.4.0"
author        = "Roland Sadowski"
description   = "Tools for working with re-frame ClojureScript projects"
license       = "EPL-2.0"
srcDir        = "src"
skipDirs      = @["test-data"]
bin           = @["reframe", "clojure_keyword_index"]


# Dependencies

requires "nim >= 1.4.0"
requires "edn 0.2.3"
