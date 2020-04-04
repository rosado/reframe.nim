# Package

version       = "0.3.0"
author        = "Roland Sadowski"
description   = "Tools for working with re-frame ClojureScript projects"
license       = "EPL-2.0"
srcDir        = "src"
skipDirs      = @["test-data"]
bin           = @["reframe", "clojure_keyword_index"]


# Dependencies

requires "nim >= 0.20.2"
requires "edn 0.2.1"
