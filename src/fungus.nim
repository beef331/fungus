import std/[macros, genasts, strutils, macrocache, decls, sets]
import pkg/micros


const adtTable = CacheTable"FungusTable"

proc hashName(n: NimNode): string =
  if n.kind == nnkBracketExpr:
    n[0].signatureHash
  else:
    n.signatureHash

macro isAdt(t: typed): untyped =
  newLit(t.getTypeInst.hashName() in adtTable)

type ADTBase* = concept t
  isAdt(t)

type FungusConvDefect = object of Defect

macro subscribeAdt(name: typed, enumFields, typeNames, dataNames: untyped) =
  adtTable[name.hashName] = newStmtList(name, enumFields, typenames, dataNames)

macro adtEnum*(origName, body: untyped): untyped =
  var typeNames, enumFields, addons, dataNames: seq[NimNode]
  let
    name =
      if origName.kind == nnkBracketExpr:
        origName[0]
      else:
        origName

    caseDef = caseStmt(NimName ident"kind")
    genericParams =
      if origName.kind == nnkBracketExpr:
        origName[1..^1]
      else:
        @[newEmptyNode()]
    instantiatedType =
      if origName.kind == nnkBracketExpr:
        let expr = nnkBracketExpr.newTree(origName[0])
        for param in genericParams:
          if param.kind == nnkIdent:
            expr.add param
          elif param.kind == nnkExprColonExpr:
            expr.add param[0]
        expr
      else:
        origName

  for entry in body:
    case entry.kind
    of nnkIdent:
      typeNames.add entry
      dataNames.add newEmptyNode()
      let enumName = ident($entry & "Kind")
      enumFields.add NimNode enumField(enumName)
      caseDef.add ofBranch(enumName, newNilLit())

      let
        typ =
          if origName.kind == nnkBracketExpr:
            let theExpr = copyNimTree(instantiatedType)
            theExpr[0] = entry
            theExpr
          else:
            entry

      addons.add:
        genAst(
          name = instantiatedType,
          enumName,
          typ,
          procName = ident("to" & $entry),
          res = ident"result"
        ):

          proc to(val: name, _: typedesc[typ]): lent typ =
            if val.kind != enumName:
              raise (ref FungusConvDefect)(msg: "Cannot convert '$#' to '$#'." % [$val.kind, $enumName])
            typ name(val)

          proc init(_: typedesc[typ]): typ = typ name(kind: enumName)

          proc `$`(val: typ): string =
            $typ & "()"

    of nnkCall, nnkCommand:
      if entry.len != 2 or (entry[1].kind != nnkStmtList and entry[1][0].kind != nnkTupleTy):
        error("Invalid entry expected `name: tuple[...].")
      typeNames.add entry[0]
      let
        enumName = ident($entry[0] & "Kind")
        dataName = ident(entry[0].repr & "Data")
        typ =
          if origName.kind == nnkBracketExpr:
            let theExpr = copyNimTree(instantiatedType)
            theExpr[0] = entry[0]
            theExpr
          else:
            entry[0]

      dataNames.add dataName

      enumFields.add NimNode enumField(enumName)
      caseDef.add ofBranch(enumName, NimNode identDef(NimName dataName, typ = entry[1]))
      addons.add:
        genAst(
          name,
          enumName,
          dataName,
          typ,
          tupl = entry[1],
          procName = ident("to" & $entry[0]),
          res = ident"result",
          instTyp = instantiatedType
        ):
          converter `to name`(arg: typ): instTyp = instTyp(arg)
          converter `to name`(arg: var typ): var instTyp = instTyp(arg)

          proc to(val: instTyp, _: typedesc[typ]): lent typ =
            if val.kind != enumName:
              raise (ref FungusConvDefect)(msg: "Cannot convert '$#' to '$#'." % [$val.kind, $enumName])
            typ instTyp(val)

          proc to(val: var instTyp, _: typedesc[typ]): var typ =
            if val.kind != enumName:
              raise (ref FungusConvDefect)(msg: "Cannot convert '$#' to '$#'." % [$val.kind, $enumName])
            typ instTyp(val)

          proc `$`(val: typ): string =
            res = astToStr(typ)
            res.add $instTyp(val).dataName

      for iDef in entry[1][0]:
        let fieldTyp = iDef[^2]
        for field in iDef[0..^3]:
          addons.add:
            genast(field, typ, fieldTyp, name, dataName, instTyp = instantiatedType):
              proc field(val: typ): lent fieldTyp = instTyp(val).dataName.field
              proc field(val: var typ): var fieldTyp = instTyp(val).dataName.field
              proc `field=`(val: var typ, newVal: fieldTyp) = instTyp(val).dataName.field = newVal

      let
        initProc = routineNode("init")
        tupleConstr = nnkTupleConstr.newTree()
      initProc.addParam identDef(NimName ident"_", makeTypeDesc typ)
      initProc.returnType = typ
      for val in entry[1][0]:
        for name in val[0..^3]:
          initProc.addParam identDef(NimName name, val[^2])
          tupleConstr.add name

      initProc.addToBody:
        genast(enumName, dataName, tupleConstr, typ, instTyp = instantiatedType):
          typ instTyp(kind: enumName, dataName: tupleConstr)

      addons[^1].add NimNode initProc
    else:
      error("Invalid entry, expected either an 'name' or 'name: tuple[...]'.", entry)

  let enumName = ident $name & "Kind"
  result = newStmtList(NimNode enumDef(NimName enumName, enumFields))
  NimNode(caseDef)[0] = NimNode identDef(NimName NimNode(caseDef)[0], NimNode enumName)
  let
    objDef = objectDef(NimName name)
    recCase = nnkRecCase.newTree()
  NimNode(caseDef).copyChildrenTo(recCase)
  objDef.recList = nnkRecList.newTree recCase

  if origName.kind == nnkBracketExpr:
    for param in origName[1..^1]:
      case param.kind
      of nnkExprColonExpr:
        objDef.addGeneric identDef(NimName param[0], param[1])
      of nnkIdent:
        objDef.addGeneric identDef(NimName param, newEmptyNode())
      else:
        error("Unexpected generic constraint", param)

  result[0].add NimNode objDef

  for i, typeName in typeNames:
    let def =
      genast(instantiatedType, typeName, field = enumFields[i]):
        type typeName = distinct instantiatedType
    def[0][1] = objDef.genericParamList()
    result[0].add def[0]

  for node in addons:
    for subNode in node:
      if subNode.kind in {nnkProcDef, nnkFuncDef, nnkConverterDef}:
        subNode[2] = objDef.genericParamList


  result.add addons
  result.add newCall(bindSym"subscribeAdt", name,
    nnkBracket.newTree(enumFields),
    nnkBracket.newTree(typeNames),
    nnkBracket.newTree(dataNames)
  )

proc getKindAndDataName(data, toLookFor: NimNode): (NimNode, NimNode) =
  for i, name in data[2]:
    if name.eqIdent(toLookFor):
      return (data[1][i], data[3][i])
  error("Invalid `kind`.", toLookFor)


macro match*(val: ADTBase, branches: varargs[untyped]): untyped =
  result = nnkIfStmt.newTree()
  let
    adtData = adtTable[val.getTypeInst.signatureHash]
    valIsNotMut = val.kind != nnkSym or val.symKind != nskVar

  var implemented: HashSet[string]

  for branch in branches:
    if branch.kind in {nnkElse, nnkElifBranch}:
      result.add branch
    else:
      case branch[0].kind
      of nnkInfix: # We're doing a named match
        if branch[0][0].kind != nnkIdent or not branch[0][0].eqIdent"as":
          error("Invaid operation expected 'as'.", branch[0][0])

        let (kind, dataName) = getKindAndDataName(adtData, branch[0][1])
        case branch[0][^1].kind
        of nnkIdent: # emit a `let`
          let injection = branch[^1].copyNimTree
          injection.insert 0, newLetStmt(branch[0][^1], newCall("to", val, branch[0][1]))
          result.add nnkElifBranch.newTree(
            infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
            injection)

        of nnkCall, nnkCommand: # Check if it's 'mut', and `val` is mut, emit `var name {.byaddr.} = val`...?
          if not branch[0][^1][0].eqIdent"mut":
            error("Can only make a 'mut' call.", branch[0][^1][0])

          if valIsNotMut:
            error("Can only make a 'mut' reference to a mutable variable.", val)

          let injection = branch[^1].copyNimTree
          injection.insert 0:
            genAst(val, byaddr = bindSym"byaddr", name = branch[0][^1][1], destType = branch[0][1]):
              var name {.byaddr.} = to(val, destType)
          result.add nnkElifBranch.newTree(
            infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
            injection)


        of nnkTupleConstr: # same as a call check if each a param is a `mut` if so emit a `byAddr` per field, also perhaps should check field count
          let injection = branch[^1].copyNimTree
          for i, x in branch[0][^1]:
            case x.kind
            of nnkCall, nnkCommand:
              if not x[0].eqIdent"mut":
                error("Invalid call inside match.", x)

              if valIsNotMut:
                error("Can only make a 'mut' reference to a mutable variable.", val)

              injection.insert 0:
                genast(val, dataName, name = x[1], index = newLit(i), destType = branch[0][1], byAddr = bindSym"byaddr"):
                  var name {.byaddr.} = val.dataName[index]


            of nnkIdent:
              if not x.eqIdent"_":
                injection.insert 0:
                  genast(val, dataName, name = x, index = newLit(i), destType = branch[0][1]):
                    let name = val.dataName[index]

            else:
              error("Invalid capture statement.", x)
          result.add nnkElifBranch.newTree(
            infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
            injection)

        else:
          error("Invalid alias statement", branch[0][^1])
        implemented.incl branch[0][1].repr

      of nnkIdent: # Just a kind match
        let (kind, _)= getKindAndDataName(adtData, branch[0])
        result.add nnkElifBranch.newTree(
          infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
          branch[^1])
        implemented.incl branch[0].repr
      else: error("Invalid branch not dong a match.", branch)

  if result[^1].kind != nnkElse:
    var unimplemented: HashSet[string]
    for kind in adtData[^1]:
      let theRepr = kind.repr
      if theRepr notin implemented:
        unimplemented.incl theRepr
    if unimplemented.len > 0:
      error("Unhandled type branch for: " & $unimplemented)

proc getDeclInfo(matcher: NimNode): (NimNode, NimNode, bool) =
  let matcher =
    if matcher.kind in {nnkStmtList, nnkStmtListExpr}:
      matcher[0]
    else:
      matcher

  case matcher.kind
  of nnkLetSection, nnkVarSection:
    if matcher.len != 1 or matcher[0].len != 3:
      error("Too many declared variables.")
    let def = matcher[0]
    if def[^1].kind != nnkEmpty:
      error("No reason to have a value")

    result = (def[0], def[1], matcher.kind == nnkVarSection)

  of nnkTupleConstr:
    if matcher.len != 1:
      error("Attempting to declare more than one match.", matcher)

    if matcher[0].kind != nnkExprColonExpr:
      error("Invalid match declaration expected 'a: type'.", matcher)

    result = (matcher[0][0], matcher[0][1], false)
  else:
    error("Invalid match, expected '(a: type)' or '(var a: type)'", matcher)


macro kindOf(name: untyped, typ: typed): untyped =
  let typ = typ.getTypeInst
  result =
    if typ.kind == nnkBracketExpr:
      nnkBracketExpr.newTree(@[name] & typ[1..^1])
    else:
      name

macro `from`*(matcher, val: untyped): untyped =
  let (name, T, isVar) = getDeclInfo(matcher)
  if isVar:
    result =
      genast(name, T, val, typKind = ident($T & "Kind")):
        val.kind == typKind and
        (let name {.byaddr.} = kindOf(T, val)(val); true)
  else:
    result =
      genast(name, T, val, typKind = ident($T & "Kind")):
        val.kind == typKind and
        (var name {.byaddr.} = kindOf(T, val)(val); true)

