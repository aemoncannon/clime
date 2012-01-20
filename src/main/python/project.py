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
import traceback


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

  def all_units(self):
    for root in self.config['source_roots']:
      for s in util.find_all_files_recursively(root, "*.cc"):
        yield s
      for s in util.find_all_files_recursively(root, "*.cpp"):
        yield s

  def all_project_headers(self):
    for root in self.config['source_roots']:
      for s in util.find_all_files_recursively(root, "*.h"):
        yield s

  def pch_options(self):
    pch = self.config.get('pch_file')
    if pch:
      return ["-include-pch", pch]
    else:
      return []

  def clang_base_cmd(self, use_pch = False):
    pch = []
    if use_pch:
      pch = self.pch_options()
    return ([self.config["clang_cmd"], "-cc1"] + 
            [ #"-fdiagnostics-print-source-range-info",
            "-fdiagnostics-parseable-fixits",
            "-fno-caret-diagnostics",
            "-fsyntax-only"
            ] +
            pch + 
            self.config['compile_options'] + 
            self.config['compile_directives'] + 
            ["-I" + inc for inc in self.config['compile_include_dirs']] + 
            ["-include" + inc for inc in self.config['compile_include_headers']])

  def clang_completions_base_cmd(self, filename, line, col):
    return ([self.config["clang_cmd"], "-cc1"] + 
            ["-fsyntax-only",
             "-code-completion-at=" + filename + ":" + str(line) + ":" + str(col)
             ] +
            self.pch_options() + 
            self.config['completion_options'] + 
            self.config['compile_directives'] + 
            ["-I" + inc for inc in self.config['compile_include_dirs']] + 
            ["-include" + inc for inc in self.config['compile_include_headers']] + 
            [filename]
            )

  def clang_pch_base_cmd(self, headers, pch_file):
    return ([self.config["clang_cmd"], "-cc1"] + 
            headers +
            ['-emit-pch', '-o', pch_file] + 
            self.config['compile_options'] + 
            self.config['compile_directives'] + 
            ["-I" + inc for inc in self.config['compile_include_dirs']] + 
            ["-include" + inc for inc in self.config['compile_include_headers']]
            )

  def analyzer_base_cmd(self):
    return (["scan-build", 
             "-analyze-headers", 
             "--keep-going"] +
            [self.config["clang_cmd"], "-cc1"] + 
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
    (lines,err_lines) = util.run_process_for_output_lines(cmd)
    candidates = []
    for line in lines:
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
      cmd = self.clang_base_cmd(use_pch = True) + [self.filename]
      util.send_sexp(
          self.req, [key(":clear-file-notes"), [self.filename]])
    elif util.is_header(self.filename):
      print "Checking header " + self.filename
      sys.stdout.flush()
      util.send_sexp(
          self.req,
          [key(":clear-all-notes"), True])

      dg = depgraph.DepGraph(self.config['source_roots'], self.config['compile_include_dirs'])
      invalidated = dg.files_including(self.filename)
      units = [f for f in invalidated if util.is_unit(f)]

      print "Recompiling " + str(len(units)) + " dependent units."
      cmd = self.clang_base_cmd(use_pch = True) + units + [self.filename]
      sys.stdout.flush()
    else:
      assert False, "WTF. Not header OR source unit?"
      
    clang_output = util.run_process(cmd)
    self.receive_syntax_checker_output(self.req, clang_output)

    util.send_sexp(self.req, util.return_ok(True, self.call_id))


class RebuildPCHJob(ClangJob):

  def __init__(self, req, call_id, config):
    ClangJob.__init__(self, req, call_id, config)

  def run(self):
    cmd = None
    sys.stdout.flush()
    header = self.config['pch_file'][0:-4]
    print "Rebuilding " + header
    (out,err) = util.run_process_for_output_lines(
        self.clang_pch_base_cmd(
            [header],
            self.config['pch_file']
            ))

    print str(out)
    print str(err)

    if len("".join(out)) == 0 and len("".join(err)) == 0:
      util.send_sexp(self.req, util.return_ok(True, self.call_id))
    else:
      util.send_sexp(self.req, util.return_ok(False, self.call_id))



class ClangCompileAllJob(ClangJob):

  def __init__(self, req, call_id, config):
    ClangJob.__init__(self, req, call_id, config)

  def run(self):
    cmd = self.clang_base_cmd()
    for s in self.all_units():
      cmd.append(s)
      
    clang_output = util.run_process(cmd)

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
    self.config = {}

  def cancel_outstanding_jobs(self):
    for job in self.jobs:
      job.cancel()
      self.jobs = []

  def start_job(self, job):
    self.cancel_outstanding_jobs()
    self.jobs.append(job)
    try:
      job.run()
    except Exception as e:
      traceback.print_exc(file=sys.stdout)
      util.send_sexp(job.req, util.return_ok(False, job.call_id))

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
    self.config['root_dir'] = os.path.abspath(conf[':root-dir'])
    self.config['source_roots'] = [
        os.path.join(self.config['root_dir'],r) for r in 
        (conf[':source-roots'] or [])]
    self.config['project_name'] = "Unnamed Project"
    self.config['compile_options'] = conf[':compile-options'] or []
    self.config['compile_directives'] = conf[':compile-directives'] or []
    self.config['compile_include_dirs'] = conf[':compile-include-dirs'] or []
    self.config['compile_include_headers'] = conf[':compile-include-headers'] or []
    self.config['analyzer_options'] = conf[':analyzer-options'] or []
    self.config['completion_options'] = conf[':completion-options'] or []
    self.config['clang_cmd'] = conf[':clang-cmd'] or "clang"

    pch = conf.get(':pch-file')
    if pch and os.path.exists(pch):
      print "Using PCH at " + pch
      self.config['pch_file'] = pch
    else:
      print "PCH file not found: ." + pch
    sys.stdout.flush()

    util.send_sexp(req,
                   util.return_ok([
                       key(":project-name"), self.config['project_name'],
                       key(":source-roots"), self.config['source_roots'],
                       key(":root-dir"), self.config['root_dir'],
                       ], call_id))

  def handle_rpc_check_file(self, rpc, req, call_id):
    filename = rpc[1]
    self.start_job(ClangCompileFileJob(req, call_id, self.config, filename))

  def handle_rpc_check_all(self, rpc, req, call_id):
    self.start_job(ClangCompileAllJob(req, call_id, self.config))

  def handle_rpc_rebuild_pch(self, rpc, req, call_id):
    self.start_job(RebuildPCHJob(req, call_id, self.config))

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
    self.start_job(ClangCompletionsJob(req, call_id, self.config, filename, line, col, prefix))

  def handle_rpc_include_completions(self, rpc, req, call_id):
    filename = rpc[1]
    prefix = rpc[2]
    self.start_job(IncludeCompletionsJob(req, call_id, self.config, filename, prefix))


