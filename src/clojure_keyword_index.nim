import reframe, os, parseopt, strutils, streams, edn, tables, options, sequtils, sets

type
  KeywordOccurrence* = object
    keyword*: EdnNode
    appears_as*: EdnNode
    file*: string
    line*: int
    column*: Option[int]

proc common_parse_opts(eof_val: EdnNode): edn.ParseOptions =
  var parser_opts: edn.ParseOptions
  parser_opts.eof_is_error = false
  parser_opts.suppress_read = false
  parser_opts.conditional_exprs = cljSource
  parser_opts.eof_value = eof_val
  parser_opts.comments_handling = discardComments
  return parser_opts

proc fill_env_with_namespaces(opts: Options, src_root: SourceRoot, env: Environment, occurrences: var seq[KeywordOccurrence]): void =
  let eof_val : EdnNode = edn.new_edn_keyword("", "eof")
  var parser_opts = common_parse_opts(eof_val)

  init_edn_readers(parser_opts)
  var p: EdnParser
  p.options = parser_opts
  var cond_expr_handling: ConditionalExpressionsHandling
  case src_root.platform_type
  of clojure_source:
    cond_expr_handling = cljSource
  of clojurescript_source:
    cond_expr_handling = cljsSource
  var edn_stream = newFileStream(opts.file_name)
  p.open(edn_stream, opts.file_name)
  defer: p.close()

  var node: EdnNode
  try:
    node = read(p)
  except ParseError:
    echo "error: " & $opts.file_name
    raise
  var current_ns: EdnNode
  while node != nil and cast[pointer](node) != cast[pointer](eof_val):
    if is_ns_form(node):
      current_ns = process_ns_form(node, env, opts.file_name)
    else:
      discard
    try:
      node = skip_nils(p, node)
    except ParseError:
      let ei = err_info(p)
      echo "error: " & $opts.file_name & " (L:" & $ei.line & ", COL:" & $ei.col & ")"
      raise

iterator node_iter(node: EdnNode): EdnNode =
  discard
  case node.kind
  of EdnKeyword:
    yield node
  of EdnList:
    for item in node.list: yield item
  of EdnMap:
    for item in node.map:
      yield item.key
      yield item.value
  of EdnVector:
    for item in node.vec: yield item
  of EdnSet:
    for item in node.set_elems: yield item.key
  # of EdnTaggedValue:
  #   TODO: impl this
  else:
    discard

proc find_keywords_in_expr(node: EdnNode, keywords: var seq[EdnNode]): void =
  case node.kind
  of EdnKeyword:
    keywords.add(node)
  of EdnList:
    for elem in node_iter(node):
      find_keywords_in_expr(elem, keywords)
  of EdnVector:
    for elem in node_iter(node):
      find_keywords_in_expr(elem, keywords)
  of EdnMap:
    for elem in node.map:
      for key_item in node_iter(elem.key):
        find_keywords_in_expr(key_item, keywords)
      for val_item in node_iter(elem.value):
        find_keywords_in_expr(val_item, keywords)
  of EdnSet:
    for elem in node_iter(node):
      find_keywords_in_expr(elem, keywords)
  of EdnTaggedValue:
    find_keywords_in_expr(node.value, keywords)
  else:
    discard
  # TODO: keywords in meta data

proc resolve_keyword_namespace(keyword_node: EdnNode, env: Environment, namespace: NamespaceObj): Option[EdnNode] =
  case keyword_node.namespacing
  of NonLocalNamespace:
    if namespace.aliases.has_key(keyword_node.keyword.ns):
      let target_ns_name = namespace.aliases[keyword_node.keyword.ns]
      let ns_symbol = new_edn_symbol("", target_ns_name)
      let resolved_node = EdnNode(kind: EdnKeyword, 
                                  keyword: (target_ns_name, keyword_node.keyword.name),
                                  namespacing: FullNamespace,
                                  line: keyword_node.line, column: keyword_node.column)
      return some(resolved_node)
    else:
      return none(EdnNode)
  of LocalNamespace:
    let resolved_node = EdnNode(kind: EdnKeyword, 
                                keyword: (namespace.name, keyword_node.keyword.name),
                                namespacing: FullNamespace,
                                line: keyword_node.line, column: keyword_node.column)
    return some(resolved_node)
  of FullNamespace, NoNamespace:
    return some(keyword_node)
  return none(EdnNode)

proc find_keywords_in_ns(opts: Options, env: Environment, ns: NamespaceObj): seq[KeywordOccurrence] =
  # TODO: too much duplication here, come up with somthing better
  let eof_val : EdnNode = edn.new_edn_keyword("", "eof")
  var parser_opts = common_parse_opts(eof_val)

  init_edn_readers(parser_opts)
  var p: EdnParser
  p.options = parser_opts
  var edn_stream = newFileStream(ns.file_name)
  p.open(edn_stream, ns.file_name)
  defer: p.close()

  var node: EdnNode
  try:
    node = read(p)
  except ParseError:
    echo "error: " & $ns.file_name
    raise

  var occurrences: seq[KeywordOccurrence] = @[]
  var keywords: seq[EdnNode] = @[]
  while node != nil and cast[pointer](node) != cast[pointer](eof_val):
    if is_ns_form(node):
      discard
    else:
      case node.kind
      of EdnKeyword, EdnList, EdnMap, EdnVector, EdnSet, EdnTaggedValue:
        find_keywords_in_expr(node, keywords)
      else:
        discard
    try:
      node = skip_nils(p, node)
    except ParseError:
      let ei = err_info(p)
      echo "error: " & $opts.file_name & " (L:" & $ei.line & ", COL:" & $ei.col & ")"
      raise      
  for kw in keywords:
    let resolved_node = resolve_keyword_namespace(kw, env, ns)
    var occurrence: KeywordOccurrence
    if resolved_node.is_some():
      occurrence = KeywordOccurrence(keyword: resolved_node.get(), 
                                     file: ns.file_name,
                                     appears_as: kw,
                                     line: kw.line, column: some(kw.column))
    else:
      occurrence = KeywordOccurrence(keyword: kw,
                                     file: ns.file_name,
                                     appears_as: kw,
                                     line: kw.line, column: some(kw.column))
    occurrences.add(occurrence)
  return occurrences

proc find_keyword_occurrences_in_root(opts: Options, env: Environment, source_root: SourceRoot): seq[KeywordOccurrence] =
  assert exists_dir(source_root.dir_path)
  var keyword_occurrs: seq[KeywordOccurrence] = @[]

  for path in walk_dir_rec(source_root.dir_path):
    if not (path.endsWith(".clj") or path.endsWith(".cljc") or path.endsWith(".cljs")):
      continue
    try:
      var updated_opts = opts
      updated_opts.file_name = path
      case opts.command
      of "index": fill_env_with_namespaces(updated_opts, source_root, env, keyword_occurrs)
    except:
      echo "Failed when processing " & $path
      raise

  for ns_symbol, namespace in env.namespaces:
      #echo "NS: " & $ns_symbol
      let occurrences = find_keywords_in_ns(opts, env, namespace)
      for o in occurrences:
        #echo "\t" & $o.keyword & " <-- " & $o.appears_as
        echo format("$#,$#,$#,$#,$#", o.keyword, o.appears_as, o.file, o.line, o.column.get(-1))
  return keyword_occurrs

proc find_and_print_keyword_occurrences(opts: Options): Environment =
  let env = new_environment()
  var occurrs: seq[KeywordOccurrence] = @[] #ALT
  let src_roots = opts.source_roots
  for root in src_roots:
    if exists_dir(root.dir_path):
      try:
        # TODO: debug report branch should happen here
        occurrs.add(find_keyword_occurrences_in_root(opts, env, root))
      except:
        echo "Unknown exception in source root: " & $root.dir_path
        raise
    else:
      echo "not a directory: " & root.dir_path
  #process_item_defs(opts, env, occurrs)#ALT
  # for k,v in env.namespaces:
  #   echo "FOUND: " & v.name
  return env

proc parse_command_line(): Option[Options] =
  let cmd_line = cast[seq[string]](command_line_params())
  var parsed = init_opt_parser(cmd_line)
  var opts: Options
  var expect_platform = false
  var dir_path: string
  opts.source_roots = @[]
  for kind, key, value in parsed.getopt():
    case kind
    of cmd_argument:
      if key != "index":
        return none(Options) 
      else:
        opts.command = key
    of cmd_long_option, cmd_short_option:
      # -- platform
      if expect_platform:
        case key
        of "platform", "p":
          if value in @["clj", "cljs"]:
            var plat: PlatformType
            case value
            of "clj":
              plat = clojure_source
            of "cljs":
              plat = clojurescript_source
            else:
              return none(Options)
            opts.source_roots.add(SourceRoot(dir_path: dir_path, platform_type: plat))
            expect_platform = false
            dir_path = ""
        else:
          return none(Options)

      # -- root dirs
      else:
        case key
        of "root", "r":
          if value != "":
            dir_path = value
            expect_platform = true
          else:
            return none(Options)
        else:
          return none(Options)
    else:
      return none(Options)
  return some(opts)

proc print_usage_and_quit(): void =
  echo "usage: clojure_keyword_index index -r=src_root -p=clj -r=other_root index -p=cljs"
  quit(1)

if is_main_module:
  let opts = parse_command_line()
  if opts.is_some:
    discard find_and_print_keyword_occurrences(opts.get())
  else:
    print_usage_and_quit()

