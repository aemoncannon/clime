import sys
import os
import sexp
from sexp import key
from sexp import sym
import util
import subprocess
import re


class Project:

    def clang_base_cmd(self):
        return (["clang++"] + 
                [ "-fdiagnostics-print-source-range-info",
                  "-fdiagnostics-fixit-info",
                  "-fdiagnostics-parseable-fixits",
                  "-fno-caret-diagnostics",
                  "-fsyntax-only",
                  "-Wall"] +
                self.compile_options + 
                self.compile_directives + 
                ["-I" + inc for inc in self.compile_include_dirs] + 
                ["-include" + inc for inc in self.compile_include_headers])

    def clang_completions_base_cmd(self, filename, line, col):
        return (["clang"] + 
                ["-cc1",
                 "-fsyntax-only",
                 "-code-completion-at=" + filename + ":" + str(line) + ":" + str(col),
                 "-code-completion-macros",
                 "-code-completion-patterns"
                 ] +
                self.compile_directives + 
                ["-I" + inc for inc in self.compile_include_dirs] + 
                ["-include" + inc for inc in self.compile_include_headers] + 
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
               self.analyzer_options +
               self.compile_directives + 
               ["-I" + inc for inc in self.compile_include_dirs] + 
               ["-include" + inc for inc in self.compile_include_headers])


    RE_COMPLETION = re.compile("^COMPLETION: (.+?) : (.+?)$")
    def clang_completions(self, req, filename, line, col, prefix, call_id):
        cmd = self.clang_completions_base_cmd(filename, line, col)
        print cmd
        sys.stdout.flush()
        clang_output = util.run_process(" ".join(cmd))
        candidates = []
        for line in clang_output:
            m = self.RE_COMPLETION.match(line)
            if m:
                name = m.group(1)
                tpe = m.group(2)
                if name.find(prefix) == 0:
                    candidates.append(
                        [key(":name"),m.group(1),
                         key(":type-sig"),m.group(2),
                         key(":is-callable"),False,
                         ])
        util.send_sexp(req, util.return_ok(candidates, call_id))


    def clang(self, req, cc_file, call_id):
        cmd = self.clang_base_cmd() + [cc_file]
        print cmd
        sys.stdout.flush()
        clang_output = util.run_process(" ".join(cmd))
        util.send_sexp(
            req,
            [key(":clear-all-notes"), True])
        self.receive_syntax_checker_output(req, clang_output)
        util.send_sexp(req, util.return_ok(True, call_id))


    def clang_all(self, req, call_id):
        cmd = self.clang_base_cmd()
        for root in self.source_roots:
            cmd.append(root + "/*.cc")

        clang_output = util.run_process(" ".join(cmd))

        util.send_sexp(
            req,
            [key(":clear-all-notes"), True])

        self.receive_syntax_checker_output(req, clang_output)

        util.send_sexp(req, util.return_ok(True, call_id))

        util.send_sexp(
            req,
            [key(":full-check-finished"), True])


    def clang_analyze_all(self, req, call_id):
        cmd = self.analyzer_base_cmd()

        for root in self.source_roots:
            cmd.append(root + "/*.cc")

        clang_output = util.run_process(" ".join(cmd))

        util.send_sexp(
            req,
            [key(":clear-all-notes"), True])

        self.receive_syntax_checker_output(req, clang_output)

        util.send_sexp(req, util.return_ok(True, call_id))

        util.send_sexp(
            req,
            [key(":full-check-finished"), True])


    def clang_analyze_file(self, req, filename, call_id):
        cmd = self.analyzer_base_cmd() + [filename]

        clang_output = util.run_process(" ".join(cmd))

        util.send_sexp(
            req,
            [key(":clear-all-notes"), True])

        self.receive_syntax_checker_output(req, clang_output)

        util.send_sexp(req, util.return_ok(True, call_id))



    SEVERITY_MAP = {'error':'error','warning':'warn','note':'info'}
    RE_ITEM = re.compile("^(.+?):([0-9]+):([0-9]+): (error|warning|note): (.+)$")
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
        self.source_roots = [os.path.join(self.root_dir,r) for r in conf[':source-roots']]
        self.project_name = "Unnamed Project"

        self.compile_options = conf[':compile-options']
        self.compile_directives = conf[':compile-directives']
        self.compile_include_dirs = conf[':compile-include-dirs']
        self.compile_include_headers = conf[':compile-include-headers']
        self.analyzer_options = conf[':analyzer-options']

        util.send_sexp(req,
                       util.return_ok([
                    key(":project-name"), self.project_name,
                    key(":source-roots"), self.source_roots,
                    key(":root-dir"), self.root_dir,
                    ], call_id))


    def handle_rpc_check_file(self, rpc, req, call_id):
        filename = rpc[1]
        self.clang(req, filename, call_id)

    def handle_rpc_check_all(self, rpc, req, call_id):
        self.clang_all(req, call_id)

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
        self.clang_completions(req, filename, 
                               line, col, prefix, call_id)

