import ast, parser

{.pragma: libgrip, exportc: "grip_$1", dynlib, cdecl.}

proc parse(code: cstring): int {.libgrip.} =
  var n = parseString($code)
  return ord(n != nil)

proc hello {.libgrip.} =
  nil

