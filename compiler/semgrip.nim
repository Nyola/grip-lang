# included from sem.nim

proc semGrip*(context: PPassContext, n: PNode): PNode =
  var c = PContext(context)
  template nim(n) =
    result = myProcess(context, n)

  if n.kind == nkCall:
    case n.sons[0].ident.s
    of "def":
      var def = n.sons[2]
      def.sons[0] = n.sons[1]
      var params = def[2]
      for i in 1.. <params.sons.len:
        let exp = params[i]
        echo "DEF PARAM"
        debug exp
        if exp.kind == nkIdent:
          params.sons[i] = newNode(nkIdentDefs, exp.info, @[exp, emptyNode, emptyNode])
        elif exp.kind == nkCall:
          params.sons[i] = newNode(nkIdentDefs, exp.info, @[exp[1], exp[2], emptyNode])
  
      def.kind = nkProcDef
      nim(def)
    else:
      nim(n)

