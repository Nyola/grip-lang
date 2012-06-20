import
  passes, ast, llstream, msgs, idents, sem, semdata, cgen, transf, strutils, astalgo

type
  TPassData = tuple[input: PNode, closeOutput: Pnode]
  TIndentMode = enum Tabs, Spaces, Unknown
  
  TLines = seq[string]
  TGripFile = object
    fileIdx: int32
    lines: TLines # consider gap buffers for these
    indents: seq[int]
    indentMode: TIndentMode

  PGripFile = ref TGripFile

const
  IDStartChars = { 'a'..'z', 'A'..'Z' }
  IDChars = { 'a'..'z', 'A'..'Z', '0'..'9' }
  NumStartChars = { '0'..'9' }
  NumChars = { '0'..'9', '.' }
  StrStartChars = { '\'', '\"' }
  OpChars = { ':', '=', '.', ';', ',', '~', '@', '#', '$', '\\',
              '<', '>', '&', '|', '!', '?', '%', '^', '+', '-', '*', '/' }

  EOF = '\0'

var gGripFiles: seq[PGripFile] = @[]

proc countIndent(line: string, c: char): int =
  for i in countup(0, line.len - 1):
    if line[i] != c: return i
  return -1

proc countIndents(lines: var TLines, indentMode: var TIndentMode): seq[int] =
  newSeq(result, lines.len)
  for i in countup(0, lines.len - 1):
    if lines[i].len == 0:
        result[i] = -1
        continue
      
    case indentMode
    of Unknown:
      if lines[i][0] == '\t':
        indentMode = Tabs
        result[i] = countIndent(lines[i], '\t')
      elif lines[i][0] == ' ':
        indentMode = Spaces
        result[i] = countIndent(lines[i], ' ')
      else: result[i] = 0
    of Tabs: result[i] = countIndent(lines[i], '\t')
    of Spaces: result[i] = countIndent(lines[i], ' ')
  
    if result[i] != 0:
      lines[i] = substr(lines[i], result[i])

    echo "LINE ", i, " ", result[i], ": ", lines[i]

proc debugCursor(g: PGripFile, cursor: TPos) =
  echo g.lines[cursor.line]
  echo(RepeatChar(cursor.col, ' ') & "^")

proc readGripFile(filename: string, stream: PLLStream): PGripFile =
  new(result)
  result.fileIdx = fileInfoIdx(filename)
  result.indentMode = Unknown
  result.lines = stream.LLStreamReadAll.splitLines
  result.indents = countIndents(result.lines, result.indentMode)

  gGripFiles.setLen(result.fileIdx + 1)
  gGripFiles[result.fileIdx] = result

proc charAt(g: PGripFile, line, col: int): char =
  if line >= g.lines.len or col >= g.lines[line].len: return EOF
  return g.lines[line][col]

proc charAt(g:PGripFile, p: TPos): char =
  return charAt(g, p.line, p.col)

proc eatChar(g: PGripFile, p: var TPos): char =
  let curLineLen = g.lines[p.line].len
  if p.col == curLineLen:
    p.col = int16(0)
    p.line += int16(1)
    if p.line >= g.lines.len: return EOF
  else:
    p.col += int16(1)

  return charAt(g, p)

iterator eatCharsInLine(g: PGripFile, pos: var TPos): char =
  # @@ yeild is broken right now and we can't yeild here
  # cheat a little bit so the yield inside the loop will
  # catch the first character
  if pos.col > int16(0): pos.col -= int16(1)

  while pos.col < g.lines[pos.line].len - 1:
    inc pos.col
    yield charAt(g, pos)

proc stepBack(g: PGripFile, p: TPos): TPos =
  result = p
  if p.col > 0:
    result.col -= int16(1)
  else:
    result.line -= int16(1)
    result.col = int16(g.lines[p.line].len - 1)

proc stepForward(g: PGripFile, p: TPos): TPos =
  result = p
  discard eatChar(g, result)

proc lineAt(g:PGripFile, p: TPos): string =
  return g.lines[p.line]

proc lineinfo(g:PGripFile, p: TPos): TLineInfo =
  return newLineInfo(g.fileIdx, p.line, p.col)

proc lineinfo(g:PGripFile, line, col: int): TLineInfo =
  return newLineInfo(g.fileIdx, line, col)

template scanLine(g, p, charClass: expr, body: stmt): stmt =
  let
    scanStart = p
    line = lineAt(g, p)
    e = line.len
    i = int(p.col)

  while i < e:
    let c = line[i]
    if c notin charClass: break
    body
    i += 1

  p.col = int16(i)

proc scanNum(g: PGripFile, p: var TPos): PNode =
  var n = 0
  scanLine(g, p, NumChars): n = n*10 + ord(c) - ord('0')

  result = newIntNode(nkIntLit, n)
  result.info = lineinfo(g, scanStart)

proc scanId(g: PGripFile, p: var TPos): PNode =
  scanLine(g, p, IDChars): nil

  let id = getIdent(line.substr(scanStart.col, i-1))
  result = newIdentNode(id, lineinfo(g, scanStart))

proc scanOp(g: PGripFile, p: var TPos): PNode =
  scanLine(g, p, OpChars): nil

  let id = getIdent(line.substr(scanStart.col, i))
  result = newIdentNode(id, lineinfo(g, p))

proc error(g: PGripFile, p: TPos, msg: TMsgKind, arg = "") =
  GlobalError(lineinfo(g, p), msg, arg)

proc tostring(c: char): string =
  result = newString(1)
  result[0] = c

proc scanToEnd(g: PGripFile, s,e: char, pos: var TPos): PNode =
  var count = 1
  
  var p = pos
  var start = stepForward(g, pos)
  
  while true:
    var c = eatChar(g, p)
    if c == EOF: error(g, p, errClosingXExpected, tostring(e))
    elif c == e:
      count -= 1
      if count == 0:
        result = newNodeI(nkSourceRange, lineinfo(g, start))
        result.ends = stepBack(g, p)
        pos = stepForward(g, p)
        return
    elif c == s:
      count += 1

proc scanWhiteSpace(g: PGripFile, pos: var TPos): tuple[spaces, tabs: int] =
  for c in eatCharsInLine(g, pos):
    if c notin {' ', '\t'}: break
    if c == ' ': result.spaces += 1
    else: result.tabs += 1

proc parseExpr(g: PGripFile, pos: var TPos): PNode
proc parseCall(g: PGripFile, s, e: int): PNode

proc parsePostExpr(g: PGripFile, pos: var TPos, n: PNode): PNode =
  result = n
  let c = charAt(g, pos)
  case c
  of '(':
    let exp = scanToEnd(g, '(', ')', pos)
    result = newNode(nkCall, n.info, @[n, exp])
  of '[':
    let exp = scanToEnd(g, '[', ']', pos)
    # result.n = newNode(nkCall, e.n.info, @[getIdent"[]", result.n, exp.n])
    # kuresult.ends = exp.ends
  of OpChars:
    var op = scanOp(g, pos)
    let next = charAt(g, op.ends)
    if next in { ' ', '\t' }:
      result = newNode(nkPostfix, n.info, @[n, op])
    else:
      let nextExp = parseExpr(g, pos)
      if op.ident.s == ".":
        result = newNode(nkDotExpr, n.info, @[n, nextExp])
      else:
        result = newNode(nkCall, n.info, @[op, n, nextExp])

      result = parsePostExpr(g, pos, result)
  of ' ', '\t':
    let ws = scanWhiteSpace(g, pos)
  of EOF: nil
  else:
    echo "in POST PARSE EXPR"
    debugCursor g, pos
    error(g, pos, errUnexpectedCharacter, tostring(c))

proc firstCall(g: PGripFile, line: int): int =
  for i in line .. < g.lines.len:
    if g.indents[i] == -1: continue
    return i

  return g.lines.len

proc scanCall(g: PGripFile, start: int): int =
  let indent = g.indents[start]
  for i in countup(start + 1, g.indents.len - 1):
    if g.indents[i] != -1 and g.indents[i] <= indent:
      return i
  return g.indents.len

proc parseBlock(g: PGripFile, pos: var TPos): PNode =
  var 
    line = firstCall(g, pos.line)
    indent = g.indents[line]
    totalLines = g.lines.len
    callEnd = 0

  result = newNode(nkStmtList)

  while true:
    callEnd = scanCall(g, line)
    let call = parseCall(g, line, callEnd)
    echo indent, " BLOCK ELEM ", line
    debug call
    result.addSon(call)
    if callEnd == totalLines or g.indents[callEnd] < indent: break
    line = callEnd

  pos = newPos(int16(callEnd), int16(0))

proc parseNestedBlock(g: PGripFile, pos: var TPos): PNode =
  var
    parentIndent = g.indents[pos.line]
    nextLine = firstCall(g, pos.line + 1)
  
  pos = newPos(int16(nextLine), int16(0))

  if pos.line == g.lines.len or g.indents[pos.line] <= parentIndent:
    result = newNode(nkStmtList, lineinfo(g, pos), @[])
  else:
    result = parseBlock(g, pos)
  
proc parseExpr(g: PGripFile, pos: var TPos): PNode =
  var start = pos
  let c = charAt(g, pos)
  echo "PARSE EXPR ", c
  case c
  of IDStartChars:
    result = scanId(g, pos)

  of NumStartChars:
    result = scanNum(g, pos)

  of '(':
    var parens = scanToEnd(g, '(', ')', pos)
    result = newNode(nkPar, lineinfo(g, start), @[parens])

  of '[':
    var args = scanToEnd(g, '[', ']', pos)
    var ws = scanWhiteSpace(g, pos)
    let c = charAt(g, pos)
    var retType, body: PNode
    if c == ':':
      discard eatChar(g, pos)
      ws = scanWhiteSpace(g, pos)
      retType = parseExpr(g, pos)
      echo "after type"
      debugCursor g, pos
    if charAt(g, pos) != EOF:
      echo "trying to parse body"
      body = parseExpr(g, pos)
    else:
      echo "trying nested block"
      body = parseNestedBlock(g, pos)

    result = newNode(nkLambda, lineinfo(g, start), @[args, retType, body])

  of StrStartChars:
    result = scanToEnd(g, c, c, pos)

  else:
    echo "in PARSE EXPR"
    debugCursor g, pos
    error(g, pos, errUnexpectedCharacter, tostring(c))

  if pos.line == result.info.line:
    result = parsePostExpr(g, pos, result)

proc isLineEnd(g: PGripFile, p: TPos): bool =
  return p.col >= g.lines[p.line].len

proc parseCall(g: PGripFile, s, e: int): PNode =
  var pos = newPos(int16(s), int16(0))
  let actionWord = parseExpr(g, pos)
  
  result = newNode(nkCall, actionWord.info, @[actionWord])
  echo "pre loop"
  debugCursor g, pos
  while not isLineEnd(g, pos):
    echo "in loop"
    debugCursor g, pos
    let param = parseExpr(g, pos)
    result.addSon(param)
    
  let hasText = isLineEnd(g, pos)

proc parse(filename: string, stream: PLLStream): TPassData =
  var
    g = readGripFile(filename, stream)
    pos = newPos(int16(0), int16(0))
  
  var topBlock = parseBlock(g, pos)
  # result.input = topBlock.n
  result.input = newNode(nkStmtList, UnknownLineInfo(), @[])
    
proc carryPass(p: TPass, module: PSym, filename: string, m: TPassData): TPassData =
  var c = p.open(module, filename)
  result.input = p.process(c, m.input)
  result.closeOutput = p.close(c, m.closeOutput)

proc sem(module: PSym, filename: string, m: TPassData): TPassData =
  result = carryPass(semPass(), module, filename, m)
  result = carryPass(transfPass(), module, filename, result)
  discard carryPass(cgenPass(), module, filename, result)

proc CompileGrip(module: PSym, filename: string, stream: PLLStream) =
  discard sem(module, filename, parse(filename, stream))

passes.grip = CompileGrip

