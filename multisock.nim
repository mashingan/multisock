# macro example for creating a function which both operates on Socket
# and AsyncSocket. multisock pragma and async pragma cannot be mixed together

import std/[asyncnet, net, macros]
from std/strutils import
  toLowerAscii,
  parseInt
from std/sugar import dump

export Socket

const
  multisockVersion* = "1.0.0"
  multisockVerbose {.booldefine.} = false

when multisockVerbose:
  template inspect(n: untyped) =
    echo "========="
    dump `n`.kind
    dump `n`.len
    dump `n`.repr
    echo "========="

template nodeIs(n: NimNode, str: string): bool =
  n.kind == nnkIdent and ($n).toLowerAscii == str.toLowerAscii
template nodeIsAsyncSocket(n: NimNode): bool =
  n.nodeIs "AsyncSocket"
template nodeIsEmpty(n: NimNode): bool = n.kind == nnkEmpty


proc recReplaceForBracket(n: var NimNode, newident = "TheSocket") =
  if n.kind == nnkBracketExpr:
    var start = 1
    if n[0].nodeIs("Future"):
      n = n[1]
      start = 0
    for i in start ..< n.len:
      if n[i].kind == nnkEmpty: continue
      if n[i].nodeIsAsyncSocket:
        n[i] = ident newident
      else:
        var nn = n[i]
        nn.recReplaceForBracket newident
        n[i] = nn

template removeAndAssign(n: untyped) =
  var nn = `n`
  removeAwaitAsyncCheck nn
  `n` = nn

proc removeAwaitAsyncCheck(n: var NimNode) =
  if n.len < 1: return
  case n.kind
  of nnkEmpty: discard
  of nnkVarSection, nnkLetSection:
    for i, section in n:
      removeAndAssign n[i]
  of nnkCommand:
    if n[0].kind notin [nnkCall, nnkDotExpr, nnkEmpty] and $n[0] in ["await", "asyncCheck"]:
      n = newStmtList(n[1..^1])
      n.removeAwaitAsyncCheck
    else:
      for i in 0..<n.len:
        removeAndAssign n[i]
  of nnkAsgn:
    removeAndAssign n[1]
  of nnkOfBranch, nnkElse:
    removeAndAssign n[^1]
  of nnkBracketExpr:
    n.recReplaceForBracket "Socket"
  of nnkCall:
    if n[0].nodeIs("all") and n.len == 2: # removing the call of all in: `await all(op)`
      n = n[1]
      n.removeAwaitAsyncCheck
    else:
      for i in 0..<n.len:
        removeAndAssign n[i]
  else:
    for i in 0..<n.len:
      removeAndAssign n[i]

template replaceWaitforNode(n: untyped) =
  var nn = `n`
  nn.replaceWaitfor
  `n` = nn

proc replaceWaitfor(n: var NimNode) =
  case n.kind
  of nnkEmpty: return
  of nnkCall, nnkCommand:
    if n[0].nodeIs "waitfor":
      n[0] = ident "await"
      for i in 1 ..< n.len:
        replaceWaitforNode n[i]
  else:
    for i in 0 ..< n.len:
      replaceWaitforNode n[i]


type MultiSock* = AsyncSocket | Socket

proc multiproc(prc: NimNode): NimNode =
  let prcsync = prc.copy
  let syncparam = prcsync[3]
  if syncparam.kind != nnkEmpty:
    if syncparam[0].kind == nnkBracketExpr:
        if syncparam[0][0].kind == nnkIdent and $syncparam[0][0] == "Future":
          syncparam[0] = syncparam[0][1]
        var traverse = syncparam[0]
        traverse.recReplaceForBracket "Socket"
        syncparam[0] = traverse
    for i in 1..<syncparam.len:
      let socksync = syncparam[i]
      if socksync[1].nodeIsAsyncSocket:
        socksync[1] = ident "Socket"
      else:
        var ss1 = socksync[1]
        ss1.recReplaceForBracket "Socket"
        socksync[1] = ss1
  var prcbody = prcsync[^1]
  prcbody.removeAwaitAsyncCheck
  if prc[^3].kind != nnkEmpty:
    prc[^3].add(ident "async")
  else:
    prc[^3] = quote do: {.async.}
  replaceWaitforNode prc[^1]
  when multisockVerbose:
    inspect prcsync
    inspect prc
  result = quote do:
    `prcsync`
    `prc`

proc multitype(ty: NimNode): NimNode =
  let genParamIsEmpty = ty[1].kind == nnkEmpty
  if not genParamIsEmpty and ty[1].kind == nnkGenericParams:
    ty[1].add quote do:
      TheSocket: AsyncSocket|Socket
  else:
    ty[1] = nnkGenericParams.newTree(newIdentDefs(ident "TheSocket", 
      nnkInfix.newTree(ident "|", ident "AsyncSocket", ident "Socket")))
    discard
  result = ty
  if ty[^1].kind != nnkObjectTy and ty[^1].kind == nnkEmpty:
    return

  var obj = ty[2]
  if ty[2].kind == nnkRefTy:
    obj = obj[0]
  if obj.nodeIsEmpty:
    when multisockVerbose: inspect obj
    return
  if obj[^1].nodeIsEmpty:
    when multisockVerbose: inspect obj[^1]
    return
  for i in 0 ..< obj[^1].len:
    let identdef = obj[2][i]
    if identdef.kind != nnkIdentDefs: continue
    if identdef[1].kind == nnkEmpty: continue
    if identdef[1].nodeIsAsyncSocket:
      identdef[1] = ident "TheSocket"
    else:
      var idf = identdef[1]
      idf.recReplaceForBracket
      identdef[1] = idf


macro multisock*(def: untyped): untyped =
  ## multisock macro operates on async proc definition
  ## with first param is AsyncSocket and returns Future
  ## which then creating the sync version of the function
  ## overload by removing `await` and `asyncCheck`
  case def.kind
  of nnkProcDef:
    result = def.multiproc
  of nnkIteratorDef:
    result = def
  else:
    let defg = def.multitype
    result = defg

when isMainModule:
  ## This part of example is to illustrate the proc that defined with
  ## multisock pragma operates on both AsyncSocket and Socket even though
  ## the implementation is defined only for async.

  import std/[threadpool, asynchttpserver, os, asyncdispatch]
  from std/tables import TableRef
  proc myp(sock: AsyncSocket): Future[int] {.multisock.} =
    let
      msg1 = await sock.recv(5)
      msg2 = await sock.recv(5)
      msg3 = await sock.recv(5)
      msg4 = await sock.recv(5)
    asyncCheck sock.send("haha")
    for m in [msg1, msg2, msg3, msg4]:
      let val = try: parseInt(m) except ValueError: 0 
      result += val


  proc casestry(sock: AsyncSocket): Future[int] {.multisock.} =
    let cmd = await sock.recv(1)
    if cmd.len < 1:
      result = 0
      return
    case cmd[0]
    of '0':
      result = 0
    of '1':
      result = await sock.myp
    else:
      result = await sock.myp

  proc main(s: AsyncSocket): Future[void] {.multisock, used.} =
    discard

  proc emptyArg: Future[void] {.multisock, used.} = discard

  type
    Dummy = object

  template dummy1() {.pragma.}
  template dummy2(key: string) {.pragma.}

  proc randomAsyncSocketArgpos(arg1: int, arg2: float, sock: AsyncSocket):
    Future[Dummy] {.multisock, inline, dummy1, dummy2("dummy2"), used.} =
    asyncCheck sock.send("dum-dum-dummy")
    discard await sock.casestry
    result = Dummy()

  type
    ObjectMulti* {.multisock, dummy1.} = object
      hehe: int
      sock: AsyncSocket

    OOmulti {.multisock.} = object
      ob: ObjectMulti[AsyncSocket]
      connections: TableRef[int, ObjectMulti[AsyncSocket]]

    OoRef {.multisock, used.} = ref object
      s: AsyncSocket
      OOMulti: ref OOmulti[AsyncSocket]

  proc getConn(o: OOMulti[AsyncSocket]):
    Future[(int, ObjectMulti[AsyncSocket])] {.multisock, used.} =
    var o {.used.}: ObjectMulti[AsyncSocket]

  let o = ObjectMulti[AsyncSocket](sock: newAsyncSocket())
  discard o

  var
    count = 0
    ready = false

  proc mainserver =
    # simple server that responds with "Hello world"
    var server = newAsyncHttpServer()
    proc cb(req: Request) {.async.} =
      echo (req.reqMethod, req.url, req.headers)
      let headers = {"Content-Type": "text/plain; charset=utf-8"}
      asyncCheck req.respond(Http200, "Hello world", newHttpHeaders headers)

    server.listen(Port 3000)
    let port = server.getPort
    echo "server listening on port: ", $port.uint16
    ready = true
    while count < 2:
      waitfor server.acceptRequest(cb)

  spawn mainserver()

  # our simple get request
  const bodyreq = "GET / HTTP/1.1\r\LHost: 127.0.0.1:3000\r\L\r\L"

  proc req2server(sock: AsyncSocket): Future[void] {.multisock.} =
    inc count
    asyncCheck sock.connect("127.0.0.1", Port 3000)
    asyncCheck sock.send(bodyreq)
    dump(await sock.recvLine) # we're only reading the response code
    close sock

  while not ready:
    # we need this part to wait the server is ready for listening
    sleep 500

  echo "ready to make request"
  waitfor req2server(newAsyncSocket())
  req2server(newSocket())
  sync()