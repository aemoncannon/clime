import socket
import threading
import SocketServer
import sys
import os
import sexp
from sexp import key
from sexp import sym
import util
import project


proj = project.Project()

class TCPRequestHandler(SocketServer.BaseRequestHandler):

  def handle(self):
    print "Handling..."
    cur_thread = threading.currentThread()
    sys.stdout.flush()
    while True:
      data_len = int(self.request.recv(6), 16)
      print "Looking for " + str(data_len) + " bytes"
      data = self.request.recv(data_len)
      print "Sexp str: " + str(data)

      try:
        parsed_sexp = sexp.read(data)
        print "Sexp: " + str(parsed_sexp)
        self.handle_sexp(parsed_sexp)
      except Exception as e:
        print "Error handling sexp: "
        print e
        
        #self.request.send(response)



  def handle_sexp(self, in_sexp):
    call_id = in_sexp[-1]
    if in_sexp[0] == key(":swank-rpc"):
      req = in_sexp[1]
      rpc_tag = req[0]
      print "Handling swank:rpc " + str(rpc_tag) + " request."
      if rpc_tag == key("swank:connection-info"):
        proj.handle_rpc_connection_info(req, self.request, call_id)
      elif rpc_tag == key("swank:init-project"):
        proj.handle_rpc_init_project(req, self.request, call_id)
      elif rpc_tag == key("swank:check-file"):
        proj.handle_rpc_check_file(req, self.request, call_id)
      elif rpc_tag == key("swank:check-all"):
        proj.handle_rpc_check_all(req, self.request, call_id)
      elif rpc_tag == key("swank:analyze-all"):
        proj.handle_rpc_analyze_all(req, self.request, call_id)
      elif rpc_tag == key("swank:analyze-file"):
        proj.handle_rpc_analyze_file(req, self.request, call_id)
      elif rpc_tag == key("swank:completions"):
        proj.handle_rpc_completions(req, self.request, call_id)
      else:
        print "Unrecognized rpc " + str(rpc_tag)
        util.send_sexp(self.request, 
                       util.return_ok(False, call_id))
    else:
      print "Unrecognized call type " + str(in_sexp)
      util.send_sexp(self.request, 
                     util.return_ok(False, call_id))


#def client(ip, port, message):
#    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
#    sock.connect((ip, port))
#    sock.send(message)
#    response = sock.recv(1024)
#    print "Received: %s" % response
#    sock.close()

def write_port(file, port):
  f = open(file, 'w')
  f.write(str(port))
  f.close()

if __name__ == "__main__":
  
  print "Starting server..."

  HOST = "localhost"
  PORTFILE = sys.argv[1]
  assert PORTFILE

  # Port 0 means to select an arbitrary unused port
  server = SocketServer.TCPServer((HOST, 0), TCPRequestHandler)
  ip, port = server.server_address
  print "Starting server at " + str(ip) + ":" + str(port)
  write_port(PORTFILE, port)
  print "Wrote port " + str(port) + " to " + PORTFILE

  sys.stdout.flush()

  # Activate the server; this will keep running until you
  # interrupt the program with Ctrl-C
  server.serve_forever()

