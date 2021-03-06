#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf, Dominik Picheta
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a simple HTTP-Server.
##
## Example:
##
## .. code-block:: nimrod
##  import strutils, sockets, httpserver
##
##  var counter = 0
##  proc handleRequest(client: TSocket, path, query: string): bool {.procvar.} =
##    inc(counter)
##    client.send("Hello for the $#th time." % $counter & wwwNL)
##    return false # do not stop processing
##
##  run(handleRequest, TPort(80))
##
## **Warning:** The API of this module is unstable, and therefore is subject
## to change.

import parseutils, strutils, os, osproc, strtabs, streams, sockets, asyncio

const
  wwwNL* = "\r\L"
  ServerSig = "Server: httpserver.nim/1.0.0" & wwwNL

# --------------- output messages --------------------------------------------

proc sendTextContentType(client: TSocket) =
  send(client, "Content-type: text/html" & wwwNL)
  send(client, wwwNL)

proc sendStatus(client: TSocket, status: string) =
  send(client, "HTTP/1.1 " & status & wwwNL)

proc badRequest(client: TSocket) =
  # Inform the client that a request it has made has a problem.
  send(client, "HTTP/1.1 400 Bad Request" & wwwNL)
  sendTextContentType(client)
  send(client, "<p>Your browser sent a bad request, " &
               "such as a POST without a Content-Length.</p>" & wwwNL)

proc cannotExec(client: TSocket) =
  send(client, "HTTP/1.1 500 Internal Server Error" & wwwNL)
  sendTextContentType(client)
  send(client, "<P>Error prohibited CGI execution." & wwwNL)

proc headers(client: TSocket, filename: string) =
  # XXX could use filename to determine file type
  send(client, "HTTP/1.1 200 OK" & wwwNL)
  send(client, ServerSig)
  sendTextContentType(client)

proc notFound(client: TSocket) =
  send(client, "HTTP/1.1 404 NOT FOUND" & wwwNL)
  send(client, ServerSig)
  sendTextContentType(client)
  send(client, "<html><title>Not Found</title>" & wwwNL)
  send(client, "<body><p>The server could not fulfill" & wwwNL)
  send(client, "your request because the resource specified" & wwwNL)
  send(client, "is unavailable or nonexistent.</p>" & wwwNL)
  send(client, "</body></html>" & wwwNL)

proc unimplemented(client: TSocket) =
  send(client, "HTTP/1.1 501 Method Not Implemented" & wwwNL)
  send(client, ServerSig)
  sendTextContentType(client)
  send(client, "<html><head><title>Method Not Implemented" &
               "</title></head>" &
               "<body><p>HTTP request method not supported.</p>" &
               "</body></HTML>" & wwwNL)

# ----------------- file serving ---------------------------------------------

proc discardHeaders(client: TSocket) = skip(client)

proc serveFile*(client: TSocket, filename: string) =
  ## serves a file to the client.
  when false: discardHeaders(client)
  var f: TFile
  if open(f, filename):
    headers(client, filename)
    const bufSize = 8000 # != 8K might be good for memory manager
    var buf = alloc(bufsize)
    while True:
      var bytesread = readBuffer(f, buf, bufsize)
      if bytesread > 0:
        var byteswritten = send(client, buf, bytesread)
        if bytesread != bytesWritten:
          dealloc(buf)
          close(f)
          OSError()
      if bytesread != bufSize: break
    dealloc(buf)
    close(f)
  else:
    notFound(client)

# ------------------ CGI execution -------------------------------------------

type
  TRequestMethod = enum reqGet, reqPost

proc executeCgi(client: TSocket, path, query: string, meth: TRequestMethod) =
  var env = newStringTable(modeCaseInsensitive)
  var contentLength = -1
  case meth
  of reqGet:
    discardHeaders(client)

    env["REQUEST_METHOD"] = "GET"
    env["QUERY_STRING"] = query
  of reqPost:
    var buf = TaintedString""
    var dataAvail = false
    while dataAvail:
      dataAvail = recvLine(client, buf) # TODO: This is incorrect.
      var L = toLower(buf.string)
      if L.startsWith("content-length:"):
        var i = len("content-length:")
        while L[i] in Whitespace: inc(i)
        contentLength = parseInt(substr(L, i))

    if contentLength < 0:
      badRequest(client)
      return

    env["REQUEST_METHOD"] = "POST"
    env["CONTENT_LENGTH"] = $contentLength

  send(client, "HTTP/1.0 200 OK" & wwwNL)

  var process = startProcess(command=path, env=env)
  if meth == reqPost:
    # get from client and post to CGI program:
    var buf = alloc(contentLength)
    if recv(client, buf, contentLength) != contentLength: 
      dealloc(buf)
      OSError()
    var inp = process.inputStream
    inp.writeData(buf, contentLength)
    dealloc(buf)

  var outp = process.outputStream
  var line = newStringOfCap(120).TaintedString
  while true:
    if outp.readLine(line):
      send(client, line.string)
      send(client, wwwNL)
    elif not running(process): break

# --------------- Server Setup -----------------------------------------------

proc acceptRequest(client: TSocket) =
  var cgi = false
  var query = ""
  var buf = TaintedString""
  discard recvLine(client, buf)
  var path = ""
  var data = buf.string.split()
  var meth = reqGet

  var q = find(data[1], '?')

  # extract path
  if q >= 0:
    # strip "?..." from path, this may be found in both POST and GET
    path = "." & data[1].substr(0, q-1)
  else:
    path = "." & data[1]
  # path starts with "/", by adding "." in front of it we serve files from cwd
  
  if cmpIgnoreCase(data[0], "GET") == 0:
    if q >= 0:
      cgi = true
      query = data[1].substr(q+1)
  elif cmpIgnoreCase(data[0], "POST") == 0:
    cgi = true
    meth = reqPost
  else:
    unimplemented(client)

  if path[path.len-1] == '/' or existsDir(path):
    path = path / "index.html"

  if not ExistsFile(path):
    discardHeaders(client)
    notFound(client)
  else:
    when defined(Windows):
      var ext = splitFile(path).ext.toLower
      if ext == ".exe" or ext == ".cgi":
        # XXX: extract interpreter information here?
        cgi = true
    else:
      if {fpUserExec, fpGroupExec, fpOthersExec} * path.getFilePermissions != {}:
        cgi = true
    if not cgi:
      serveFile(client, path)
    else:
      executeCgi(client, path, query, meth)

type
  TServer* = object of TObject  ## contains the current server state
    socket: TSocket
    port: TPort
    client*: TSocket      ## the socket to write the file data to
    reqMethod*: string    ## Request method. GET or POST.
    path*, query*: string ## path and query the client requested
    headers*: PStringTable ## headers with which the client made the request
    body*: string          ## only set with POST requests
    ip*: string            ## ip address of the requesting client
  
  PAsyncHTTPServer* = ref TAsyncHTTPServer
  TAsyncHTTPServer = object of TServer
    asyncSocket: PAsyncSocket
  
proc open*(s: var TServer, port = TPort(80)) =
  ## creates a new server at port `port`. If ``port == 0`` a free port is
  ## acquired that can be accessed later by the ``port`` proc.
  s.socket = socket(AF_INET)
  if s.socket == InvalidSocket: OSError()
  bindAddr(s.socket, port)
  listen(s.socket)

  if port == TPort(0):
    s.port = getSockName(s.socket)
  else:
    s.port = port
  s.client = InvalidSocket
  s.reqMethod = ""
  s.body = ""
  s.path = ""
  s.query = ""
  s.headers = {:}.newStringTable()

proc port*(s: var TServer): TPort =
  ## get the port number the server has acquired.
  result = s.port

proc next*(s: var TServer) =
  ## proceed to the first/next request.
  var client: TSocket
  new(client)
  var ip: string
  acceptAddr(s.socket, client, ip)
  s.client = client
  s.ip = ip
  s.headers = newStringTable(modeCaseInsensitive)
  #headers(s.client, "")
  var data = ""
  s.client.readLine(data)
  if data == "":
    # Socket disconnected 
    s.client.close()
    next(s)
    return
  var header = ""
  while true:
    s.client.readLine(header)
    if header == "\c\L": break
    if header != "":
      var i = 0
      var key = ""
      var value = ""
      i = header.parseUntil(key, ':')
      inc(i) # skip :
      i += header.skipWhiteSpace(i)
      i += header.parseUntil(value, {'\c', '\L'}, i)
      s.headers[key] = value
    else:
      s.client.close()
      next(s)
      return
  
  var i = skipWhitespace(data)
  if skipIgnoreCase(data, "GET") > 0: 
    s.reqMethod = "GET"
    inc(i, 3)
  elif skipIgnoreCase(data, "POST") > 0:
    s.reqMethod = "POST"
    inc(i, 4)
  else:
    unimplemented(s.client)
    s.client.close()
    next(s)
    return
  
  if s.reqMethod == "POST":
    # Check for Expect header
    if s.headers.hasKey("Expect"):
      if s.headers["Expect"].toLower == "100-continue":
        s.client.sendStatus("100 Continue")
      else:
        s.client.sendStatus("417 Expectation Failed")
  
    # Read the body
    # - Check for Content-length header
    if s.headers.hasKey("Content-Length"):
      var contentLength = 0
      if parseInt(s.headers["Content-Length"], contentLength) == 0:
        badRequest(s.client)
        s.client.close()
        next(s)
        return
      else:
        var totalRead = 0
        var totalBody = ""
        while totalRead < contentLength:
          var chunkSize = 8000
          if (contentLength - totalRead) < 8000:
            chunkSize = (contentLength - totalRead)
          var bodyData = newString(chunkSize)
          var octetsRead = s.client.recv(cstring(bodyData), chunkSize)
          if octetsRead <= 0:
            s.client.close()
            next(s)
            return
          totalRead += octetsRead
          totalBody.add(bodyData)
        if totalBody.len != contentLength:
          s.client.close()
          next(s)
          return

        s.body = totalBody
    else:
      badRequest(s.client)
      s.client.close()
      next(s)
      return
  
  var L = skipWhitespace(data, i)
  inc(i, L)
  # XXX we ignore "HTTP/1.1" etc. for now here
  var query = 0
  var last = i
  while last < data.len and data[last] notin whitespace: 
    if data[last] == '?' and query == 0: query = last
    inc(last)
  if query > 0:
    s.query = data.substr(query+1, last-1)
    s.path = data.substr(i, query-1)
  else:
    s.query = ""
    s.path = data.substr(i, last-1)

proc close*(s: TServer) =
  ## closes the server (and the socket the server uses).
  close(s.socket)

proc run*(handleRequest: proc (client: TSocket, 
                               path, query: string): bool {.closure.},
          port = TPort(80)) =
  ## encapsulates the server object and main loop
  var s: TServer
  open(s, port)
  #echo("httpserver running on port ", s.port)
  while true:
    next(s)
    if handleRequest(s.client, s.path, s.query): break
    close(s.client)
  close(s)

# -- AsyncIO begin

proc nextAsync(s: PAsyncHTTPServer) =
  ## proceed to the first/next request.
  var client: TSocket
  new(client)
  var ip: string
  acceptAddr(getSocket(s.asyncSocket), client, ip)
  s.client = client
  s.ip = ip
  s.headers = newStringTable(modeCaseInsensitive)
  #headers(s.client, "")
  var data = ""
  s.client.readLine(data)
  if data == "":
    # Socket disconnected 
    s.client.close()
    return
  var header = ""
  while true:
    s.client.readLine(header) # TODO: Very inefficient here. Prone to DOS.
    if header == "\c\L": break
    if header != "":
      var i = 0
      var key = ""
      var value = ""
      i = header.parseUntil(key, ':')
      inc(i) # skip :
      i += header.skipWhiteSpace(i)
      i += header.parseUntil(value, {'\c', '\L'}, i)
      s.headers[key] = value
    else:
      s.client.close()
      return
  
  var i = skipWhitespace(data)
  if skipIgnoreCase(data, "GET") > 0: 
    s.reqMethod = "GET"
    inc(i, 3)
  elif skipIgnoreCase(data, "POST") > 0:
    s.reqMethod = "POST"
    inc(i, 4)
  else:
    unimplemented(s.client)
    s.client.close()
    return
  
  if s.reqMethod == "POST":
    # Check for Expect header
    if s.headers.hasKey("Expect"):
      if s.headers["Expect"].toLower == "100-continue":
        s.client.sendStatus("100 Continue")
      else:
        s.client.sendStatus("417 Expectation Failed")
  
    # Read the body
    # - Check for Content-length header
    if s.headers.hasKey("Content-Length"):
      var contentLength = 0
      if parseInt(s.headers["Content-Length"], contentLength) == 0:
        badRequest(s.client)
        s.client.close()
        return
      else:
        var totalRead = 0
        var totalBody = ""
        while totalRead < contentLength:
          var chunkSize = 8000
          if (contentLength - totalRead) < 8000:
            chunkSize = (contentLength - totalRead)
          var bodyData = newString(chunkSize)
          var octetsRead = s.client.recv(cstring(bodyData), chunkSize)
          if octetsRead <= 0:
            s.client.close()
            return
          totalRead += octetsRead
          totalBody.add(bodyData)
        if totalBody.len != contentLength:
          s.client.close()
          return

        s.body = totalBody
    else:
      badRequest(s.client)
      s.client.close()
      return
  
  var L = skipWhitespace(data, i)
  inc(i, L)
  # XXX we ignore "HTTP/1.1" etc. for now here
  var query = 0
  var last = i
  while last < data.len and data[last] notin whitespace: 
    if data[last] == '?' and query == 0: query = last
    inc(last)
  if query > 0:
    s.query = data.substr(query+1, last-1)
    s.path = data.substr(i, query-1)
  else:
    s.query = ""
    s.path = data.substr(i, last-1)

proc asyncHTTPServer*(handleRequest: proc (server: PAsyncHTTPServer, client: TSocket, 
                        path, query: string): bool {.closure.},
                     port = TPort(80), address = ""): PAsyncHTTPServer =
  ## Creates an Asynchronous HTTP server at ``port``.
  var capturedRet: PAsyncHTTPServer
  new(capturedRet)
  capturedRet.asyncSocket = AsyncSocket()
  capturedRet.asyncSocket.handleAccept =
    proc (s: PAsyncSocket) =
      nextAsync(capturedRet)
      let quit = handleRequest(capturedRet, capturedRet.client, capturedRet.path,
                               capturedRet.query)
      if quit: capturedRet.asyncSocket.close()
  
  capturedRet.asyncSocket.bindAddr(port, address)
  capturedRet.asyncSocket.listen()
  if port == TPort(0):
    capturedRet.port = getSockName(capturedRet.asyncSocket)
  else:
    capturedRet.port = port
  
  capturedRet.client = InvalidSocket
  capturedRet.reqMethod = ""
  capturedRet.body = ""
  capturedRet.path = ""
  capturedRet.query = ""
  capturedRet.headers = {:}.newStringTable()
  result = capturedRet

proc register*(d: PDispatcher, s: PAsyncHTTPServer) =
  ## Registers a ``PAsyncHTTPServer`` with a ``PDispatcher``.
  d.register(s.asyncSocket)

proc close*(h: PAsyncHTTPServer) =
  ## Closes the ``PAsyncHTTPServer``.
  h.asyncSocket.close()

when isMainModule:
  var counter = 0

  var s: TServer
  open(s, TPort(0))
  echo("httpserver running on port ", s.port)
  while true:
    next(s)
    
    inc(counter)
    s.client.send("Hello, Andreas, for the $#th time. $# ? $#" % [
      $counter, s.path, s.query] & wwwNL)
    
    close(s.client)
  close(s)

