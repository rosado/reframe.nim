# To run these tests, simply execute `nimble test`.

import unittest

import os
import edn, reframe

test "everything":
  var opts: Options
  var
    src_a = SourceRoot(dir_path: "test-data/src-a", platform_type: clojure_source)
    src_b = SourceRoot(dir_path: "test-data/src-b", platform_type: clojure_source)
  opts.source_roots = @[src_a, src_b]

  let env = new_environment()
  var reframe_defs: seq[ReframeItem] = @[]
  for path in walk_dir_rec("test-data"):
    var updated_opts = opts
    updated_opts.file_name = path
    find_reframe_items(updated_opts, env, reframe_defs)

  assert reframe_defs.len == 4
  
  block:
    let symb = new_edn_symbol("s", "fn")
    assert resolve_symbol(env, "foo.app", symb).category == NamespaceNotFound

    var r: SymbolResolutionResultObj
    r = resolve_symbol(env, "foo.app", new_edn_symbol("", "bogus1")) 
    assert r.category == NotFound

    r = resolve_symbol(env, "foo.app", new_edn_symbol("", "c->d"))
    assert r.category == FoundRefer

    r = resolve_symbol(env, "foo.app" , new_edn_symbol("", "libspec?"))
    #assert r.category == FoundDefinition

    r = resolve_symbol(env, "foo.app" , new_edn_symbol("", "max-value"))
    assert r.category == FoundDefinition

    r = resolve_symbol(env, "foo.app" , new_edn_symbol("", "reg-event-db"))
    assert r.category == FoundRefer
    assert r.refered == ("re-frame.core", "reg-event-db")

  block:
    var r: bool

    r = is_symbol_resolved_as(env, "foo.app", new_edn_symbol("s", "fn"), new_edn_symbol("schema.core", "fn"))
    assert r

    r = is_symbol_resolved_as(env, "foo.app", new_edn_symbol("", "c->d"), new_edn_symbol("mix.c", "c->d"))
    assert r

    r = is_symbol_resolved_as(env, "foo.app", new_edn_symbol("", "not-defined"), new_edn_symbol("mix.c", "something"))
    assert (not r)

    r = is_symbol_resolved_as(env, "foo.app", new_edn_symbol("", "c->d"), new_edn_symbol("mix.c", "other-name"))
    assert (not r)

    r = is_symbol_resolved_as(env, "foo.events", new_edn_symbol("", "zoo-fn"), new_edn_symbol("foo.zoo", "zoo-fn"))
    assert r
    
  for item in reframe_defs:
    echo $item.reframe_type & " " & $item.event_key , " ", item.line, " ", $item.kind, " ", $item.file_name
  #check add(5, 5) == 10
