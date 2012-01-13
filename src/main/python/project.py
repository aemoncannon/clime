import sys
import os
import sexp
from sexp import key
from sexp import sym
import util
import subprocess
import re
import depgraph
from glob import glob
import model
import fnmatch



class Job:

  def __init__(self, req, call_id):
    self.req = req
    self.call_id = call_id

  def run(self):
    self.running = True

  def cancel(self):
    self.canceled = True


class ClangJob(Job):
  
  def __init__(self, req, call_id, config):
    Job.__init__(self, req, call_id)
    self.config = config

  def nuke_all_precompiled_headers(self):
    for r in self.config['source_roots']:
      for f in glob (r + '*.h.gch'):
        os.unlink (f)

  def all_units(self):
    for root in self.config['source_roots']:
      for s in util.find_all_files_recursively(root, "*.cc"):
        yield s
      for s in util.find_all_files_recursively(root, "*.cpp"):
        yield s

  def clang_base_cmd(self):
    return (["clang++"] + 
            [ #"-fdiagnostics-print-source-range-info",
            "-fdiagnostics-fixit-info",
            "-fdiagnostics-parseable-fixits",
            "-fno-caret-diagnostics",
            "-fsyntax-only",
            "-Wall"] +
            self.config['compile_options'] + 
            self.config['compile_directives'] + 
            ["-I" + inc for inc in self.config['compile_include_dirs']] + 
            ["-include" + inc for inc in self.config['compile_include_headers']])

  def clang_completions_base_cmd(self, filename, line, col):
    return (["clang"] + 
            ["-cc1",
             "-fsyntax-only",
             "-code-completion-at=" + filename + ":" + str(line) + ":" + str(col)
             ] +
            self.config['completion_options'] + 
            self.config['compile_directives'] + 
            ["-I" + inc for inc in self.config['compile_include_dirs']] + 
            ["-include" + inc for inc in self.config['compile_include_headers']] + 
            [filename]
            )

  def analyzer_base_cmd(self):
    return (["scan-build", 
             "-analyze-headers", 
             "--keep-going"] +
            ["clang++"] + 
            ["--analyze", 
             "-fdiagnostics-fixit-info", 
             "-fdiagnostics-parseable-fixits",
             "-fno-caret-diagnostics",
             "-fvisibility=default",
             "-frtti", "-fno-exceptions", 
             "-Wall"] +
            self.config['analyzer_options'] +
            self.config['compile_directives'] + 
            ["-I" + inc for inc in self.config['compile_include_dirs']] + 
            ["-include" + inc for inc in self.config['compile_include_headers']])

  SEVERITY_MAP = {'error':'error','warning':'warn','note':'info','fatal error':'error'}
  RE_ITEM = re.compile("^(.+?):([0-9]+):([0-9]+): (error|warning|note|fatal error): (.+)$")
  def receive_syntax_checker_output(self, req, clang_output):
    for line in clang_output:
      m = self.RE_ITEM.match(line)
      if m:
        print line
        sys.stdout.flush()
        filename = m.group(1)
        line = int(m.group(2))
        col = int(m.group(3))
        item_type = m.group(4)
        msg = m.group(5)
        util.send_sexp(
            req,
            [key(":notes"), 
             [key(":notes"),
              [[key(":file"), filename,
                key(":line"), line,
                key(":col"), col,
                key(":beg"), False,
                key(":end"), False,
                key(":severity"), sym(self.SEVERITY_MAP[item_type]),
                key(":msg"), msg
                ]]]])



class ClangCompletionsJob(ClangJob):

  def __init__(self, req, call_id, config, filename, line, col, prefix):
    ClangJob.__init__(self, req, call_id, config)
    self.filename = filename
    self.line = line
    self.col = col
    self.prefix = prefix

  RE_COMPLETION = re.compile("^COMPLETION: (.+?) : (.+?)$")

  def run(self):
    #    f = open(self.filename)
    #    for l in f.readlines():
    #      print l
    case_sens = (re.compile("[A-Z]")).search(self.prefix) is not None
    cmd = self.clang_completions_base_cmd(self.filename, self.line, self.col)
    sys.stdout.flush()
    clang_output = util.run_process(" ".join(cmd))
    candidates = []
    lines = [l for l in clang_output]
    for line in lines:
      print line
      m = self.RE_COMPLETION.match(line)
      if m:
        name = m.group(1)
        tpe = m.group(2)
        if ((case_sens and name.find(self.prefix) == 0)
            or (not case_sens and name.upper().find(self.prefix.upper()) == 0)):
          member = model.make_member(tpe)
          if member is not None:
            candidates.append(
                [key(":name"), member.name,
                 key(":type-sig"), str(member),
                 key(":is-callable"), member.is_callable(),
                 key(":args-placeholder"), member.args_placeholder()
                 ])
          else:
            candidates.append(
                [key(":name"),name,
                 key(":type-sig"),tpe,
                 key(":args-placeholder"),""
                 ])
    util.send_sexp(self.req, util.return_ok(candidates, self.call_id))

class IncludeCompletionsJob(ClangJob):

  def __init__(self, req, call_id, config, filename, prefix):
    ClangJob.__init__(self, req, call_id, config)
    self.filename = filename
    self.prefix = prefix

  def run(self):
    candidates = []
    case_sens = (re.compile("[A-Z]")).search(self.prefix) is not None
    for dir in self.config['compile_include_dirs']:
      for root, dirnames, filenames in os.walk(dir):
        for filename in fnmatch.filter(filenames, "*"):
          path = os.path.join(root, filename)
          p,name = os.path.split(path)
          base,ext = os.path.splitext(name)
          if ((ext == ".h" or ext == "" or ext == ".hpp") and
              ((case_sens and name.find(self.prefix) == 0)
              or (not case_sens and name.upper().find(self.prefix.upper()) == 0))):
            candidates.append(
                [key(":name"),name,
                 key(":rel-path"),filename,
                 key(":special"), len(ext) == 0
                 ])
    util.send_sexp(self.req, util.return_ok(candidates, self.call_id))


class ClangCompileFileJob(ClangJob):

  def __init__(self, req, call_id, config, filename):
    ClangJob.__init__(self, req, call_id, config)
    self.filename = filename


  def run(self):
    cmd = None
    sys.stdout.flush()

    if util.is_unit(self.filename):
      print "Checking source unit " + self.filename
      sys.stdout.flush()
      cmd = self.clang_base_cmd() + [self.filename]
      util.send_sexp(
          self.req, [key(":clear-file-notes"), [self.filename]])
    elif util.is_header(self.filename):
      print "Checking header " + self.filename
      sys.stdout.flush()
      self.nuke_all_precompiled_headers()
      util.send_sexp(
          self.req,
          [key(":clear-all-notes"), True])

      dg = depgraph.DepGraph(self.config['source_roots'], self.config['compile_include_dirs'])
      invalidated = dg.files_including(self.filename)
      units = [f for f in invalidated if util.is_unit(f)]

      print "Recompiling " + str(len(units)) + " dependent units."
      cmd = self.clang_base_cmd() + units + [self.filename]
      sys.stdout.flush()
    else:
      assert False, "WTF. Not header OR source unit?"
      
    clang_output = util.run_process(" ".join(cmd))
    self.receive_syntax_checker_output(self.req, clang_output)

    util.send_sexp(self.req, util.return_ok(True, self.call_id))


class ClangCompileAllJob(ClangJob):

  def __init__(self, req, call_id, config):
    ClangJob.__init__(self, req, call_id, config)

  def run(self):
    cmd = self.clang_base_cmd()
    for s in self.all_units():
      cmd.append(s)
      
    clang_output = util.run_process(" ".join(cmd))

    util.send_sexp(
        self.req,
        [key(":clear-all-notes"), True])

    self.receive_syntax_checker_output(self.req, clang_output)

    util.send_sexp(self.req, util.return_ok(True, self.call_id))

    util.send_sexp(
        self.req,
        [key(":full-check-finished"), True])


class Project:

  def __init__(self):
    self.jobs = []

  def cancel_outstanding_jobs(self):
    for job in self.jobs:
      job.cancel()
      self.jobs = []

  def start_job(self, job):
    self.cancel_outstanding_jobs()
    self.jobs.append(job)
    job.run()

  def compile_config(self):
    return {
        'root_dir' : self.root_dir,
        'source_roots' : self.source_roots,
        'compile_options': self.compile_options,
        'compile_directives': self.compile_directives,
        'compile_include_headers': self.compile_include_headers,
        'compile_include_dirs': self.compile_include_dirs,
        'analyzer_options': self.analyzer_options,
        'completion_options': self.completion_options
        }

#    def clang_analyze_all(self, req, call_id):
#        cmd = self.analyzer_base_cmd()
#
#        for root in self.source_roots:
#            cmd.append(root + "/*.cc")
#
#        clang_output = util.run_process(" ".join(cmd))
#
#        util.send_sexp(
#            req,
#            [key(":clear-all-notes"), True])
#
#        self.receive_syntax_checker_output(req, clang_output)
#
#        util.send_sexp(req, util.return_ok(True, call_id))
#
#        util.send_sexp(
#            req,
#            [key(":full-check-finished"), True])
#
#
#    def clang_analyze_file(self, req, filename, call_id):
#        cmd = self.analyzer_base_cmd() + [filename]
#
#        clang_output = util.run_process(" ".join(cmd))
#
#        util.send_sexp(
#            req,
#            [key(":clear-all-notes"), True])
#
#        self.receive_syntax_checker_output(req, clang_output)
#
#        util.send_sexp(req, util.return_ok(True, call_id))
#

  def handle_rpc_connection_info(self, rpc, req, call_id):
    util.send_sexp(req, util.return_ok(                
    [key(":pid"),os.getpid(),
     key(":style"),False,
     key(":server-implementation"),[
         key(":type"),False,
         key(":name"),False,
         key(":version"),"0.0.1",
         key(":program"),False
         ],
     key(":machine"),[
         key(":instance"),False,
         key(":type"),False,
         key(":version"),False
         ],
     key(":features"),False,
     key(":package"),False,
     key(":version"), "0.0.1",
     key(":modules"),False,
     ], call_id))

  def handle_rpc_init_project(self, rpc, req, call_id):
    conf = util.sexp_to_key_map(rpc[1])
    self.root_dir = os.path.abspath(conf[':root-dir'])
    self.source_roots = [os.path.join(self.root_dir,r) for r in (conf[':source-roots'] or [])]
    self.project_name = "Unnamed Project" 
    self.compile_options = conf[':compile-options'] or []
    self.compile_directives = conf[':compile-directives'] or []
    self.compile_include_dirs = conf[':compile-include-dirs'] or []
    self.compile_include_headers = conf[':compile-include-headers'] or []
    self.analyzer_options = conf[':analyzer-options'] or []
    self.completion_options = conf[':completion-options'] or []
    util.send_sexp(req,
                   util.return_ok([
                       key(":project-name"), self.project_name,
                       key(":source-roots"), self.source_roots,
                       key(":root-dir"), self.root_dir,
                       ], call_id))

  def handle_rpc_check_file(self, rpc, req, call_id):
    filename = rpc[1]
    self.start_job(ClangCompileFileJob(req, call_id, self.compile_config(), filename))

  def handle_rpc_check_all(self, rpc, req, call_id):
    self.start_job(ClangCompileAllJob(req, call_id, self.compile_config()))

  def handle_rpc_analyze_file(self, rpc, req, call_id):
    filename = rpc[1]
    self.clang_analyze_file(req, filename, call_id)

  def handle_rpc_analyze_all(self, rpc, req, call_id):
    self.clang_analyze_all(req, call_id)

  def handle_rpc_completions(self, rpc, req, call_id):
    filename = rpc[1]
    line = rpc[2]
    col = rpc[3]
    prefix = rpc[4]
    self.start_job(ClangCompletionsJob(req, call_id, self.compile_config(), filename, line, col, prefix))

  def handle_rpc_include_completions(self, rpc, req, call_id):
    filename = rpc[1]
    prefix = rpc[2]
    self.start_job(IncludeCompletionsJob(req, call_id, self.compile_config(), filename, prefix))


