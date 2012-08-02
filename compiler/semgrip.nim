# included from sem.nim

import macros

dumpTree:
  for x,y in 1..10:
    echo "Bar"

proc toStmtList(n: PNode): PNode =
  result = n

proc toIfStmt(n: PNode): PNode =
  result = newNode(nkIfStmt, n.info, @[
    newNode(nkElifBranch, n.info, @[n[2], n[3].toStmtList])
  ])

proc buildIfFromElse(n: PNode): Pnode =
  let previous = n[1]
  case previous.ident.id
  of ord(wIf):
    result = previous.toIfStmt
  of ord(wElse):
    result = previous.buildIfFromElse
  else:
    internalAssert false

  result.addSon(newNode(nkElse, n.info, @[n[2].toStmtList]))

proc semGrip*(context: PPassContext, n: PNode): PNode =
  var c = PContext(context)
  template nim(n) =
    result = myProcess(context, n)

  echo "ENTERING SEM GRIP"
  debug n

  if n.kind == nkCall:
    case n.sons[0].ident.id
    of ord(wDef):
      var def = n.sons[2]
      def.sons[0] = n.sons[1]
      var params = def[2]
      for i in 1.. <params.sons.len:
        let exp = params[i]
        if exp.kind == nkIdent:
          params.sons[i] = newNode(nkIdentDefs, exp.info, @[exp, emptyNode, emptyNode])
        elif exp.kind == nkCall:
          params.sons[i] = newNode(nkIdentDefs, exp.info, @[exp[1], exp[2], emptyNode])
  
      def.kind = nkProcDef
      nim(def)
    of ord(wIf):
      nim(n.toIfStmt)
    of ord(wElse):
      result = n.buildIfFromElse
      nim(result)
    of ord(wFor):
      echo "for"
    of ord(wWhile):
      echo "while"
    else:
      nim(n)

  echo "EXITING SEM GRIP"

