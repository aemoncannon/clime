import sys
import sexp
import subprocess
import os
from sexp import key

def len_header(num):
    s = hex(num).lstrip("0x")
    while len(s) < 6:
        s = "0" + s
    return s

def return_ok(val, call_id):
    return [key(":return"), [ key(":ok"), val ], call_id]

def is_unit(filename):
    base,ext = os.path.splitext(filename)
    return ext == ".c" or ext == ".cc"

def is_header(filename):
    base,ext = os.path.splitext(filename)
    return ext == ".h"

def send_sexp(req, response_sexp):
    response_str = sexp.to_string(response_sexp)
    header = len_header(len(response_str))
    print "Writing: " + header
    print "Writing: " + response_str
    req.send(header)
    req.send(response_str)
    sys.stdout.flush()

def run_process(cmd_str):
    print cmd_str
    sys.stdout.flush()
    p = subprocess.Popen(cmd_str, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
    while(True):
        retcode = p.poll() #returns None while subprocess is running
        line = p.stdout.readline()
        yield line
        if(retcode is not None):
            break

def sexp_to_key_map(sexp):
    key_type = type(key(":key"))
    result = {}
    for i in xrange(0, len(sexp), 2):
        k,val = sexp[i],sexp[i+1]
        if type(k) == key_type:
            result[str(k)] = val
    return result

