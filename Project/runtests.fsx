
#r "bin/FsLexYacc.Runtime.dll"
#load "Environ.fs"
#load "Absyn.fs"
#load "PlcParserAux.fs"
#load "PlcParser.fs"
#load "PlcLexer.fs"
#load "Parse.fs"
#load "TestAux.fs"

open Absyn
open TestAux
let fromString = Parse.fromString // string parser function

#load "Test.fs"

testAll Test.cases
