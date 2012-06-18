import passes, ast, llstream, msgs, idents, sem, semdata, cgen, transf

type TPassData = tuple[input: PNode, closeOutput: Pnode]

proc parse(filename: string): TPassData =
  var info = newLineInfo(filename, 1, 1)
  result.input = newNode(nkStmtList, info, @[
    newNode(nkCall, info, @[
      newIdentNode(getIdent"echo", info),
      newStrNode(nkStrLit, "test")])
    ])

proc carryPass(p: TPass, module: PSym, filename: string, m: TPassData): TPassData =
  var c = p.open(module, filename)
  result.input = p.process(c, m.input)
  result.closeOutput = p.close(c, m.closeOutput)

proc sem(module: PSym, filename: string, m: TPassData): TPassData =
  result = carryPass(semPass(), module, filename, m)
  result = carryPass(transfPass(), module, filename, result)
  discard carryPass(cgenPass(), module, filename, result)

proc CompileGrip(module: PSym, filename: string, stream: PLLStream) =
  discard sem(module, filename, parse(filename))

passes.grip = CompileGrip

