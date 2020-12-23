# (c) Roland Sadowski <r AT rosado c c>

import os, parseopt2, strutils, streams, edn, tables, options, sequtils, sets

type
  OperationMode = enum
    resolveSymbols

  PlatformType* = enum
    clojure_source
    clojurescript_source
    
  SourceRoot* = object
    dir_path*: string
    platform_type*: PlatformType

  Options* = object
    source_roots*: seq[SourceRoot] #TODO: source_roots | filename
    file_name*: string
    command*: string

  FunctionArityInfo = object
    arglist: EdnNode
    body: EdnNode

  FunctionInfo = object
    name: EdnNode #TODO: should we fill the ns when parsing?
    arities: TableRef[int, FunctionArityInfo]

  NamespaceObj* = object
    name*: string
    file_name*: string
    aliases*:  TableRef[string, string] # Symbol->Namespace in clojure (:refer)
    mappings*: TableRef[string, (string,string)] # Symbol->Var in clojure (:as)
    required*: TableRef[string, bool]   # Namespace -> bool (not in clojure)
    defines*:  TableRef[string, EdnNode]

  # ResolutionDataObj = object
  #   unresolved_defines: TableRef[EdnNode, ] # namespace->
  
  Environment* = ref EnvironmentObj
  EnvironmentObj* = object
    namespaces*: TableRef[EdnNode, NamespaceObj]

  FnDefinitionKind = enum
    InlineDefinition
    VarReference
  
  ReframeType = enum
    Sub
    Event
    FX
    CoFX
  
  ReframeItem* = object
    reframe_type*: ReframeType
    file_name*: string
    line*: int
    event_key*: EdnNode
    case kind*: FnDefinitionKind
    of InlineDefinition:
      current_ns*: Option[EdnNode]
    of VarReference:
      symbol*: EdnNode
      target_ns*: EdnNode
      target_file_name*: string
      referenced_line*: int

let
  # KEYWORDS
  NS_SYMBOL = new_edn_symbol("", "ns")
  REQUIRE_KEYWORD = new_edn_keyword("", "require")
  AS_KEYWORD = new_edn_keyword("", "as")
  ALL_KEYWORD = new_edn_keyword("", "all")
  REFER_KEYWORD = new_edn_keyword("", "refer")
  # SYMBOLD
  LET_SYMBOL = new_edn_symbol("", "let")
  DEF_SYMBOL = new_edn_symbol("", "def")
  DEFN_SYMBOL = new_edn_symbol("", "defn")
  DEFN_PRIV_SYMBOL = new_edn_symbol("", "defn-")
  DEFNK_SYMBOL = new_edn_symbol("", "defnk")
  REG_EVENT_DB_SYMBOL = new_edn_symbol("", "reg-event-db")
  REG_EVENT_FX_SYMBOL = new_edn_symbol("", "reg-event-fx")
  REG_SUB_SYMBOL = new_edn_symbol("", "reg-sub")
  REG_SUB_RAW_SYMBOL = new_edn_symbol("", "reg-sub-raw")
  REG_FX_SYMBOL = new_edn_symbol("", "reg-fx")
  REG_COFX_SYMBOL = new_edn_symbol("", "reg-cofx")

# proc find_node_matching(pattern: EdnNode, nodes: seq[EdnNode]): EdnNode =
#   for node in nodes:
#     if node.kind == pattern.kind:
#
# proc find_node*(to_find: EdnNode, nodes: seq[EdnNode]): EdnNode =
#   let found = find(nodes, to_find)
#   if found >= 0: return nodes[found]
#   else: return nil

proc is_form_starting_with*(node: EdnNode, symbols: HashSet[EdnNode], min_length: int = 2): bool = 
  if node.kind == EdnList and node.list.len >= min_length:
    let elem = node.list[0]
    # if elem != nil and elem.kind == EdnSymbol:
    #   echo "CHECKING " & $elem
    if symbols.contains(elem):
      return true
    return false

proc is_form_starting_with*(node: EdnNode, sym: EdnNode, min_length: int = 2): bool = 
  if node.kind == EdnList and node.list.len >= min_length:
    let elem = node.list[0]
    return elem == sym

proc is_let_form*(node: EdnNode): bool = 
  return is_form_starting_with(node, LET_SYMBOL, 2)

proc is_defn_form*(node: EdnNode): bool = 
  var alternatives = init_hash_set[EdnNode]()
  alternatives.incl(DEFN_SYMBOL)
  alternatives.incl(DEFN_PRIV_SYMBOL)
  alternatives.incl(DEFNK_SYMBOL)
  return is_form_starting_with(node, alternatives, 3)

proc is_def_form*(node: EdnNode): bool =
  return is_form_starting_with(node, DEF_SYMBOL)

proc is_ns_form*(node: EdnNode): bool =
  if node.kind == EdnList and node.list.len >= 2:
    let elem = node.list[0]
    if elem == NS_SYMBOL:
      return true
  return false

proc next_ns_require_pos(list: seq[EdnNode], index: int): Option[int] =
  var index = index
  while list.high >= index:
    let elem = list[index]
    if elem.kind == EdnList:
      if elem.list.len > 0 and elem.list[0] == REQUIRE_KEYWORD:
        return some(index)
    inc(index)
  return none(int)

proc ns_name(ns_prefix, name: string): string =
  if ns_prefix == "":
    return name
  else:
    return ns_prefix & "." & name

proc is_ns_as_subspec(subspec: seq[EdnNode]): bool =
  assert subspec.len == 3
  let
    p1 = subspec[0].kind == EdnSymbol
    p2 = subspec[1] == AS_KEYWORD
    p3 = subspec[2].kind == EdnSymbol
  return p1 and p2 and p3


proc is_ns_refer_subspec(subspec: seq[EdnNode]): bool =
  assert subspec.len == 3
  let
    p1 = subspec[0].kind == EdnSymbol
    p2 = subspec[1] == REFER_KEYWORD
    p3 = (subspec[2].kind == EdnVector) or (subspec[2] == ALL_KEYWORD)
  return p1 and p2 and p3

proc process_ns_subspec(subspec: seq[EdnNode],
                        ns: NamespaceObj,
                        ns_prefix: string): Option[NamespaceObj] =
  if subspec.len != 3:
    return none(NamespaceObj)
  var ns = ns
  if is_ns_as_subspec(subspec):
    let
      suffix = subspec[0].symbol.name
      alias  = subspec[2].symbol.name
      aliased_ns_name = ns_name(ns_prefix, suffix)
    ns.aliases[alias] = aliased_ns_name
    ns.required[aliased_ns_name] = true
  elif is_ns_refer_subspec(subspec):
    let
      suffix = subspec[0].symbol.name
      referred_ns_name = ns_name(ns_prefix, suffix)
    ns.required[referred_ns_name] = true
    if subspec[2] == ALL_KEYWORD:
      discard # not much to do here
    else:
      for s in subspec[2].vec:
        ns.mappings[s.symbol.name] = (referred_ns_name, s.symbol.name)
  else:
    return none(NamespaceObj)

  return some(ns)

proc is_libspec(spec: EdnNode): bool =
  case spec.kind
  of EdnSymbol:
    return true
  of EdnVector:
    return spec.vec.len < 2 or spec.vec[1].kind == EdnKeyword
  else:
    return false #TODO: blow up here?

proc is_regevent_form*(node: EdnNode): bool =
  var alternatives = init_hash_set[EdnNode]()
  alternatives.incl(REG_EVENT_DB_SYMBOL)
  alternatives.incl(REG_EVENT_FX_SYMBOL)
  return is_form_starting_with(node, alternatives)

proc is_regsub_form*(node: EdnNode): bool =
  var alternatives = init_hash_set[EdnNode]()
  alternatives.incl(REG_SUB_SYMBOL)
  alternatives.incl(REG_SUB_RAW_SYMBOL)
  return is_form_starting_with(node, alternatives)

proc is_regfx_form*(node: EdnNode): bool =
  var alternatives = init_hash_set[EdnNode]()
  alternatives.incl(REG_FX_SYMBOL)
  return is_form_starting_with(node, alternatives)

proc is_regcofx_form*(node: EdnNode): bool =
  var alternatives = init_hash_set[EdnNode]()
  alternatives.incl(REG_COFX_SYMBOL)
  return is_form_starting_with(node, alternatives)


proc process_libspec(spec: EdnNode,
                     ns: NamespaceObj,
                     ns_prefix: string): NamespaceObj =
  ## Returns the updated ns object
  var ns = ns
  case spec.kind
  of EdnSymbol:
    let full_ns_name = ns_name(ns_prefix, spec.symbol.name)
    ns.required[full_ns_name] = true
  of EdnVector:
    if spec.vec.len < 2:
      for s in spec.vec:
        if s.kind == EdnSymbol:
          ns.required[ns_name(ns_prefix, s.symbol.name)] = true
        else:
          raise new_exception(Exception, "illegal spec: " & $s.kind)
    else:
      var index = 1
      # it's either CLJS or JS require (e.g. (require ["left-pad" :as left-pad])
      assert spec.vec[0].kind == EdnSymbol or spec.vec[0].kind == EdnString
      var full_ns: string
      case spec.vec[0].kind
      of EdnSymbol:
        full_ns = ns_name(ns_prefix, spec.vec[0].symbol.name)
      of EdnString:
        full_ns = spec.vec[0].str
      else:
        raise new_exception(Exception, "unsupported lib spec: " & $spec.vec[0].kind)
      while index <= spec.vec.high and (index + 1 <= spec.vec.high):
        #if spec.vec[index] == AS_KEYWORD:
        assert spec.vec[index].kind == EdnKeyword
        let
          subspec = @[spec.vec[0], spec.vec[index], spec.vec[index+1]]
          updated = process_ns_subspec(subspec, ns, ns_prefix)
        if updated.is_some:
          ns = updated.get
        else:
          ns.required[full_ns] = true
        index = index + 2
  else:
    raise new_exception(Exception, "unhandled type: " & $spec.kind)

  return ns


proc process_require_elem*(spec_elem: EdnNode,
                           ns: NamespaceObj,
                           ns_prefix: string): NamespaceObj =
  # spec should be the contents of the form after :require kw
  var
    ns = ns
    index = 1
    #x_guard = 0

  if is_libspec(spec_elem):
    ns = process_libspec(spec_elem, ns, "")
  elif spec_elem.kind == EdnVector:
    assert spec_elem.vec[0].kind == EdnSymbol
    let prefix = spec_elem.vec[0].symbol.name
    while index <= spec_elem.vec.high:
    #while index <= spec_elem.vec.high and x_guard < 10:
      let current = spec_elem.vec[index]
      if is_libspec(current):
        ns = process_libspec(current, ns, prefix)
      elif current.kind == EdnVector:
        discard
      else:
        let kind = current.kind
        raise new_exception(Exception, "libspec elem is of type: " & $kind)
      inc(index)
      #inc(x_guard)
  return ns

proc form_line_num(node: EdnNode): int =
  assert node.kind == EdnList
  return node.line


proc resove_implementation(node, ns_symbol: EdnNode,
                                  resolve_as: ReframeType,
                                  env: Environment,
                                  file_name: string): ReframeItem =
  ## node - the EdnNode we're trying to resolve
  ## ns_symbol - currently processed namespace
  let last_form = node.list[node.list.high()]
  let item_key = node.list[1]
  # var anon_fn_alternatives = init_hash_set[EdnNode]()
  # anon_fn_alternatives.incl(new_edn_symbol("", "fn"))
  # anon_fn_alternatives.incl(new_edn_symbol("schema.core", "fn"))
  # anon_fn_alternatives.incl(new_edn_symbol("s", "fn"))
  # anon_fn_alternatives.incl(new_edn_symbol("", "letfn"))
  assert (item_key != nil)
  result = ReframeItem(event_key: item_key, reframe_type: resolve_as)

  case last_form.kind
  of EdnList:
    # could be an anonymous fn but also a fn call returnign a fn.
    # either way, we treat it as inline definition
    #if is_form_starting_with(last_form, anon_fn_alternatives):
    let inline_fn_line_num = form_line_num(last_form)
    result = ReframeItem(event_key: item_key,
                         reframe_type: resolve_as,
                         kind: InlineDefinition,
                         file_name: file_name,
                         line: inline_fn_line_num)
    if item_key.namespacing == LocalNamespace:
      result.current_ns = some(ns_symbol)
    else:
      result.current_ns = none(EdnNode)
  of EdnSymbol:
    result = ReframeItem(kind: VarReference,
                         event_key: item_key,
                         reframe_type: resolve_as,
                         symbol: last_form,
                         file_name: file_name,
                         line: node.line)
    # let's find the where the symb is defined
    let current_ns = env.namespaces[ns_symbol]
    let no_ns = last_form.symbol.ns == ""
    if no_ns and current_ns.mappings.contains(last_form.symbol.name):
      # we fill some details. The line number will be filled later
      assert (not current_ns.defines.contains(last_form.symbol.name))
      result.target_ns = new_edn_symbol("", current_ns.mappings[last_form.symbol.name][0])
    elif no_ns:
      result.target_ns = new_edn_symbol("", current_ns.name)
      if current_ns.defines.contains(last_form.symbol.name):
        let define = current_ns.defines[last_form.symbol.name]
        result.target_file_name = current_ns.file_name
        result.referenced_line = define.line
      else:
        # TODO: handle declares?
        result.referenced_line = -1
        result.target_file_name = ""
    else: # symbol has ns
      var target_ns_symbol: EdnNode
      var debugFlag = false
      if current_ns.aliases.contains(last_form.symbol.ns):
        let aliased_ns_name = current_ns.aliases[last_form.symbol.ns]
        target_ns_symbol = new_edn_symbol("", aliased_ns_name)
      else:
        target_ns_symbol = new_edn_symbol("", last_form.symbol.ns)
      
      if env.namespaces.contains(target_ns_symbol):
        let ns = env.namespaces[target_ns_symbol]
        result.target_file_name = ns.file_name
        result.target_ns = target_ns_symbol
      else:
        # must be fully qualified symbol
        result.target_file_name = ""
        result.target_ns = target_ns_symbol
  else:
    raise new_exception(Exception, "Unsupported last form in event/sub registration: " & $last_form.kind)

  if result.kind == VarReference and result.target_ns == nil:
    raise new_exception(Exception, "result.target_ns not set for " & $result.event_key &
                        " in file " & $file_name)

  if result.file_name == "":
    raise new_exception(Exception, "result.file_name not set for " & $result.event_key &
                        " in file " & $file_name)
  return result

proc process_regevent_form*(node, ns_symbol: EdnNode,
                            env: Environment,
                            file_name: string): ReframeItem =
  return resove_implementation(node, ns_symbol, Event, env, file_name)

proc process_regsub_form*(node, ns_symbol: EdnNode,
                          env: Environment,
                          file_name: string): ReframeItem =
  return resove_implementation(node, ns_symbol, Sub, env, file_name)

proc process_regfx_form*(node, ns_symbol: EdnNode,
                         env: Environment,
                         file_name: string): ReframeItem =
  return resove_implementation(node, ns_symbol, FX, env, file_name)

proc process_regcofx_form*(node, ns_symbol: EdnNode,
                           env: Environment,
                           file_name: string): ReframeItem =
  return resove_implementation(node, ns_symbol, CoFX, env, file_name)

proc process_ns_form*(node: EdnNode, env: Environment, file_name: string): EdnNode =
  let ns_symbol = node.list[1]
  var ns: NamespaceObj
  if contains(env.namespaces, ns_symbol):
    ns = env.namespaces[ns_symbol]
  else:
    ns.aliases   = new_table[string,string]()
    ns.mappings  = new_table[string,(string,string)]()
    ns.required  = new_table[string,bool]()
    ns.defines   = new_table[string, EdnNode]()
    ns.name      = ns_symbol.symbol.name
    ns.file_name = file_name

  # process :require-s within a ns form
  var
    start_pos = 2
    req = next_ns_require_pos(node.list, start_pos)
  while req.is_some:
    start_pos = req.get
    let require_form = node.list[start_pos]
    if require_form.list.len >= 2:
      for i in 1 .. require_form.list.high:
        #assert require_form.list[index].kind == EdnVector
        let elem = require_form.list[i]
        if elem.kind == EdnVector or elem.kind == EdnList:
          ns = process_require_elem(elem, ns, "")
        elif elem.kind == EdnSymbol:
          ns.required[elem.symbol.name] = true
        elif elem.kind == EdnCommentLine:
            discard
        else:
          assert false
    inc(start_pos)
    req = next_ns_require_pos(node.list, start_pos)

  env.namespaces[ns_symbol] = ns
  assert ns_symbol != nil
  return ns_symbol

# name, args (what about arities?), body
# - name
# - meta (or just leave it on the name symbol)
# - arities (int->??)
#   - arglist
#   - body
# (defn ?meta foo ([x] ...) ([x y] ...))
proc process_defn_form(node, ns_symbol: EdnNode, env: Environment): FunctionInfo =
  let name_symbol = node.list[0]
  var info: FunctionInfo
  info.name = name_symbol
  info.arities = new_table[int,FunctionArityInfo]()
  if ns_symbol in env.namespaces:
    let ns = env.namespaces[ns_symbol]
    ns.defines[node.list[1].symbol.name] = node
  # echo "meta of that defn: " & $node.line
  # if node.list[2].kind == EdnString:
  #   echo "DEFN: docstring first"
  # elif node.list[2].kind == EdnVector:
  #   echo "DEFN: arglist first"
  # elif node.list[2].kind == EdnList:
  #   echo "DEFN: arity form"
  # else:
  #   echo "nothing to say about this fn: " & $(node.list[2].kind)
  return

proc process_def_form*(node: EdnNode, ns_symbol: EdnNode, env: Environment): bool =
  assert ns_symbol.kind == EdnSymbol
  if ns_symbol in env.namespaces:
    let ns = env.namespaces[ns_symbol]
    ns.defines[node.list[1].symbol.name] = node
    return true
  return false

proc skip_nils*(p: var EdnParser, node: EdnNode): EdnNode =
  var n: EdnNode
  while n == nil:
    n = read(p)
  return n

proc common_parse_opts(eof_val: EdnNode): edn.ParseOptions =
  var parser_opts: edn.ParseOptions
  parser_opts.eof_is_error = false
  parser_opts.suppress_read = false
  parser_opts.conditional_exprs = cljSource
  parser_opts.eof_value = eof_val
  parser_opts.comments_handling = discardComments#keepComments
  return parser_opts

type SymbolResolutionResultCategory* = enum
  NamespaceNotFound
  FoundDefinition
  FoundRefer
  FoundNamespaceOnly
  NotFound

type SymbolResolutionResultObj* {.acyclic.} = object
  case category*: SymbolResolutionResultCategory
  of NamespaceNotFound:
    nil
  of FoundNamespaceOnly:
    nil
  of NotFound:
    nil
  of FoundRefer:
    refered*: (string, string)
  of FoundDefinition:
    node*: EdnNode
  
# take `symbol` (found in code, so it can be sth like: s/fn) and try to find
# where it is defined (then maybe return it's metadata, file:line etc)
proc resolve_symbol*(env: Environment, namespace: string, symbol: EdnNode): SymbolResolutionResultObj =
  assert symbol.kind == EdnSymbol
  let ns_symb = new_edn_symbol("", namespace)
  if ns_symb in env.namespaces:
    let ns = env.namespaces[ns_symb]
    if symbol.symbol.ns != "":
      if symbol.symbol.ns in ns.aliases:
        # now lookup the symbol.name in the alias target ns
        let target_ns_symb = new_edn_symbol("", ns.aliases[symbol.symbol.ns])
        if target_ns_symb in env.namespaces:
          let target_ns = env.namespaces[target_ns_symb]
          if symbol.symbol.name in target_ns.defines:
            let define = target_ns.defines[symbol.symbol.name]
            return SymbolResolutionResultObj(category: FoundDefinition,
                                             node: define)
          else:
            return SymbolResolutionResultObj(category: FoundNamespaceOnly)
        else:
          return SymbolResolutionResultObj(category: NamespaceNotFound)
      elif symbol.symbol.ns in ns.required:
        # symbol potentially in 'required' ns, look it up in that ns if possible
        let target_ns_symb = new_edn_symbol("", symbol.symbol.ns)
        if target_ns_symb in env.namespaces:
          let target_ns = env.namespaces[target_ns_symb]
          if symbol.symbol.name in target_ns.defines:
            return SymbolResolutionResultObj(category: FoundDefinition)
        else:
          return SymbolResolutionResultObj(category: FoundNamespaceOnly)
      else:
        # this means the symbol's namespace is not required in current ns
        return SymbolResolutionResultObj(category: NotFound)
    else:
      if symbol.symbol.name in ns.mappings:
        var refered  = ns.mappings[symbol.symbol.name]
        # we try to follow through to the refered ns if possible
        var resolved_refer = resolve_symbol(env, refered[0], symbol)
        case resolved_refer.category
        of FoundDefinition:
          return resolved_refer
        else:
          return SymbolResolutionResultObj(category: FoundRefer,
                                           refered: refered)
      elif symbol.symbol.name in ns.defines:
        let define = ns.defines[symbol.symbol.name]
        return SymbolResolutionResultObj(category: FoundDefinition,
                                         node: define)
      else:
        return SymbolResolutionResultObj(category: NotFound)
  return SymbolResolutionResultObj(category: NamespaceNotFound)

proc is_symbol_resolved_as*(env: Environment, 
                           namespace: string,
                           found, target: EdnNode): bool =
  assert target.kind == EdnSymbol
  let resolution = resolve_symbol(env, namespace, found)
  case resolution.category
  of FoundDefinition:
    return true
  of FoundRefer:
    return target.symbol == resolution.refered
  of NamespaceNotFound:
    return found.symbol.name == target.symbol.name
  else:
    return false

proc find_reframe_items*(opts: Options, env: Environment, item_defs: var seq[ReframeItem]): void =
  ## Modifies 'item_defs' argument.
  let eof_val : EdnNode = edn.new_edn_keyword("", "eof")
  var parser_opts = common_parse_opts(eof_val)
  #let initial_max_index = max(item_defs.high(), 0)

  init_edn_readers(parser_opts)
  var p: EdnParser
  p.options = parser_opts
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
    elif is_regevent_form(node):
      var def_item = process_regevent_form(node, current_ns, env, opts.file_name)
      ##****** NEED ABILITY TO LOOKUP schema.core/fn in env (e.g. resolve file's s/fn to fully qualified symbol)
      item_defs.add(def_item)
    elif is_regsub_form(node):
      var def_item = process_regsub_form(node, current_ns, env, opts.file_name)
      item_defs.add(def_item)
    elif is_regfx_form(node):
      var def_item = process_regfx_form(node, current_ns, env, opts.file_name)
      item_defs.add(def_item)
    elif is_regcofx_form(node):
      var def_item = process_regcofx_form(node, current_ns, env, opts.file_name)
      item_defs.add(def_item)
    elif is_def_form(node):
      discard process_def_form(node, current_ns, env)
    elif is_defn_form(node):
      discard process_defn_form(node, current_ns, env)
    else:
      #echo "It's a " & $node.kind
      discard
    try:
      node = skip_nils(p, node)
    except ParseError:
      echo "error: " & $opts.file_name
      raise

proc new_environment*(): Environment =
  result = new(Environment)
  result.namespaces = new_table[EdnNode, NamespaceObj]()
  
proc DEBUG_print_report_internal(opts: Options, env: Environment): void =
  let eof_val : EdnNode = edn.new_edn_keyword("", "eof")
  var parser_opts = common_parse_opts(eof_val)

  init_edn_readers(parser_opts)
  var p: EdnParser
  p.options = parser_opts
  var edn_stream = newFileStream(opts.file_name)
  p.open(edn_stream, opts.file_name)
  defer: p.close()

  var node = read(p)
  var current_ns: EdnNode
  while node != nil and cast[pointer](node) != cast[pointer](eof_val):
    echo "* " & $node.kind
    if node.kind == EdnList:
      echo "line: " & $node.line
      echo "col:  " & $node.column
    if is_ns_form(node):
      current_ns = process_ns_form(node, env, opts.file_name)
    elif is_defn_form(node):
      discard process_defn_form(node, current_ns, env)
    else:
      discard
    node = skip_nils(p, node)

  # echo "is 'foo' defined?" & $is_defined(env, "foo")
  # echo "is foo/whatever" & $is_defined(env, "b/foo")

proc DEBUG_print_env_data(env: Environment): void =
  # print the report about namespaces
  echo "\L== REPORT =="
  for k,v in env.namespaces.pairs():
    echo k.symbol.name
    for req_ns,_ in v.required:
      echo "\tR: ", req_ns
    for k,v in v.aliases:
      echo "\tA: ", k, " -> ", v
    for k,v in v.mappings:
      echo "\tM: ", k, " -> ",v

proc format_event_key(def: ReframeItem): string =
  case def.kind
  of VarReference:
    if def.event_key.namespacing == LocalNamespace:
      return format(":$#/$#", def.target_ns, def.event_key.keyword.name)
    else:
      return $def.event_key
  of InlineDefinition:
    if def.event_key.namespacing == LocalNamespace and def.current_ns.is_some():
      return format(":$#/$#", def.current_ns.get(), def.event_key.keyword.name)
    else:
      return $def.event_key

proc find_reframe_items_in_root(opts: Options, env: Environment, source_root: string): seq[ReframeItem] =
  assert dir_exists(source_root)
  var item_defs: seq[ReframeItem] = @[]

  proc is_clj_file(path: string): bool =
    path.ends_with(".clj") or path.ends_with(".cljs") or path.ends_with(".cljc")
  for path in walk_dir_rec(source_root):
    if not is_clj_file(path):
      continue
    try:
      var updated_opts = opts
      updated_opts.file_name = path
      case opts.command
      of "index": find_reframe_items(updated_opts, env, item_defs)
    except:
      echo "Failed when processing " & $path
      raise
  
  return item_defs

proc process_item_defs(opts: Options, env: Environment, item_defs: var seq[ReframeItem]): void =
  ## processes ReframeItems found and fills in meta data when possible
  ## and prints prints them in line format.
  for index in item_defs.low()..item_defs.high():
    var def = item_defs[index]
    case def.kind
    of VarReference:
      if env.namespaces.contains(def.target_ns):
        let target_ns = env.namespaces[def.target_ns]
        let resolution = resolve_symbol(env, target_ns.name, new_edn_symbol("", def.symbol.symbol.name))
        case resolution.category
        of FoundDefinition:
          def.referenced_line = resolution.node.line
          def.target_file_name = target_ns.file_name
        else:
          def.referenced_line = -1
          def.target_file_name = ""
        def.target_ns = new_edn_symbol("", target_ns.name)
      else:
        def.target_file_name = ""
        def.referenced_line = -1
      item_defs[index] = def
    else:
      # inline, no need to resolve the definition
      discard

  for def in item_defs:
    case def.kind
    of VarReference:
      var target_file_name: string = ""
      if def.target_file_name == "":
        target_file_name = "*unknown*"
      else:
        target_file_name = def.target_file_name
      echo format("$# var $# $# $# $# $#",
                  format_event_key(def),
                  target_file_name,
                  def.referenced_line,
                  def.reframe_type,
                  def.file_name,
                  def.line)
    of InlineDefinition:
      echo format("$# inline $# $# $# $# $#",
                  format_event_key(def),
                  def.file_name,
                  def.line,
                  def.reframe_type,
                  def.file_name,
                  def.line)
  return

proc not_empty*(s: string): bool = not is_empty_or_whitespace(s)

proc find_and_print_reframe_data(opts: Options): Environment =
  let env = new_environment()
  var item_defs: seq[ReframeItem] = @[]
  let src_roots = opts.source_roots#filter(opts.source_roots, not_empty)
  for root in src_roots:
    if dir_exists(root.dir_path):
      try:
        # TODO: debug report branch should happen here
        item_defs.add(find_reframe_items_in_root(opts, env, root.dir_path))
      except:
        echo "Unknown exception in source root: " & $root.dir_path
        raise
    else:
      echo "not a directory: " & root.dir_path
  process_item_defs(opts, env, item_defs)
  return env

proc print_usage_and_quit(): void =
  echo "usage: reframe cmd -r=src_root -r=other_root"
  quit(1)

proc parse_command_line(): Option[Options] =
  let cmd_line = cast[seq[string]](command_line_params())
  var parsed = init_opt_parser(cmd_line)
  var opts: Options
  var opt_count = 0
  opts.source_roots = @[]
  for kind, key, value in parsed.getopt():
    inc(opt_count)
    case kind
    of cmd_long_option, cmd_short_option:
      if value == "":
        raise new_exception(Exception, "Unsupported option: " & key)
      else:
        if key == "r" or key == "root":
          if value != "":
            opts.source_roots.add(SourceRoot(dir_path: value, platform_type: clojurescript_source))
          else:
            raise new_exception(Exception, "missing 'source_root' value")
        else:
          raise new_exception(Exception, "unsupported option: " & key)
    of cmd_argument:
      if opts.command != "":
        return none(Options)
      if key != "index":
        return none(Options)
      else:
        opts.command = key
    else:
      return none(Options)
  if opt_count == 0: return none(Options)
  return some(opts)

when is_main_module:
  let opts = parse_command_line()
  if opts.is_some:
    discard find_and_print_reframe_data(opts.get())
  else:
    print_usage_and_quit()

