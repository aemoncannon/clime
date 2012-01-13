import sys
import sexp
import subprocess
import os
from sexp import key
import fnmatch

def len_header(num):
    s = hex(num).lstrip("0x")
    while len(s) < 6:
        s = "0" + s
    return s

def return_ok(val, call_id):
    return [key(":return"), [ key(":ok"), val ], call_id]

def is_unit(filename):
    base,ext = os.path.splitext(filename)
    return ext == ".c" or ext == ".cc" or ext == ".cpp"

def is_header(filename):
    base,ext = os.path.splitext(filename)
    return ext == ".h"

def find_all_files_recursively(directory, glob):
    for root, dirnames, filenames in os.walk(directory):
        for filename in fnmatch.filter(filenames, glob):
            yield os.path.join(root, filename)

def send_sexp(req, response_sexp):
    response_str = sexp.to_string(response_sexp)
    header = len_header(len(response_str))
    print "Writing: " + header
    print "Writing: " + response_str
    req.send(header)
    req.send(response_str)
    sys.stdout.flush()

def run_process(cmd):
    print " ".join(cmd)
    sys.stdout.flush()
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, 
                         stderr=subprocess.STDOUT)
    while(True):
      retcode = p.poll() #returns None while subprocess is running
      line = p.stdout.readline()
      yield line
      if(retcode is not None):
        break

def run_process_and_forget(cmd):
    print " ".join(cmd)
    sys.stdout.flush()
    p = subprocess.Popen(cmd)


def run_process_for_output_lines(cmd):
    print " ".join(cmd)
    sys.stdout.flush()
    p = subprocess.Popen(cmd, 
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, 
                         universal_newlines=True)
    stdout, stderr = p.communicate()
    return (stdout.split("\n"),stderr.split("\n"))


#def run_process_for_output_lines(cmd_str):
#    print cmd_str
#    sys.stdout.flush()
#    s = subprocess.check_output(cmd_str, 
#                                stderr=subprocess.STDOUT, 
#                                shell=True,
#                                universal_newlines=True)
#    return s.split("\n")


def sexp_to_key_map(sexp):
    key_type = type(key(":key"))
    result = {}
    for i in xrange(0, len(sexp), 2):
        k,val = sexp[i],sexp[i+1]
        if type(k) == key_type:
            result[str(k)] = val
    return result

