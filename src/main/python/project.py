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
                self.compile_options + 
                self.compile_directives + 
                ["-I" + inc for inc in self.compile_include_dirs] + 
                ["-include" + inc for inc in self.compile_include_headers])


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
            cmd.append(root + "/*")

        clang_output = util.run_process(" ".join(cmd))

        util.send_sexp(
            req,
            [key(":clear-all-notes"), True])

        self.receive_syntax_checker_output(req, clang_output)

        util.send_sexp(req, util.return_ok(True, call_id))

        util.send_sexp(
            req,
            [key(":full-check-finished"), True])



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

