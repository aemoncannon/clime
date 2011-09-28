import sys
import os
import sexp
from sexp import key
from sexp import sym
import util
import subprocess



OPTIONS=[
    "-fdiagnostics-print-source-range-info",
    "-fdiagnostics-fixit-info",
    "-fdiagnostics-parseable-fixits",
    "-fno-caret-diagnostics",
    "-fsyntax-only",
    "-fvisibility=default",
    "-fPIC",
    "-frtti",
    "-fno-exceptions",
    "-Wall",
    "-Wpointer-arith",
    "-Woverloaded-virtual",
    "-Wsynth",
    "-Wno-ctor-dtor-privacy",
    "-Wno-non-virtual-dtor",
    "-Wcast-align",
    "-Wno-invalid-offsetof",
    "-Wno-long-long",
    "-isysroot /Developer/SDKs/MacOSX10.4u.sdk",
    "-fno-strict-aliasing",
    "-fpascal-strings",
    "-fno-common",
    "-fshort-wchar",
    "-pthread",
    "-frtti",
    "-Wno-variadic-macros",
    "-Wno-write-strings",
    "-Wno-deprecated-writable-strings"
    ]

DIRECTIVES=["-DOSTYPE=\"Darwin\"",
            "-DOSARCH=Darwin",
            "-DGTEST_NOT_MAC_FRAMEWORK_MODE",
            "-DWITH_OPENSSL",
            "-DNOMINMAX",
            "-DOPENSSL_DISABLE_OLD_DES_SUPPORT",
            "-DNO_X11",
            "-DMOZILLA_CLIENT",
            "-DDEBUG"
            ]

MOZILLA_HOME="/Users/aemon/projects/awe/mozilla-1.9.2"
OBJ_BUILD_DIR="/Users/aemon/projects/awe/build"
GIT_ROOT="/Users/aemon/projects/awe/git"
SDK_ROOT="/Developer/SDKs/MacOSX10.4u.sdk"

INCLUDE_DIRS=[
#    MOZILLA_HOME + "/db/sqlite3/src",
    OBJ_BUILD_DIR + "/dist/system_wrappers",
    OBJ_BUILD_DIR + "/dist/include",
    OBJ_BUILD_DIR + "/dist/include/nsprpub",
    OBJ_BUILD_DIR + "/dist/include/nspr",
    OBJ_BUILD_DIR + "/dist/include/nss",
    MOZILLA_HOME + "/xulrunner/aw_power_editor/components/src",
    MOZILLA_HOME + "/xulrunner",
    MOZILLA_HOME + "/xulrunner/aw_power_editor/components/src/autoupdate",
    MOZILLA_HOME + "/xulrunner/aw_power_editor/protobuf/googlecode_protobuf/src",
    MOZILLA_HOME + "/xulrunner/aw_power_editor/third_party/win32/curl/include",
    MOZILLA_HOME + "/xulrunner/aw_power_editor/third_party/win32/sqlite3",
    MOZILLA_HOME + "/xulrunner/aw_power_editor/gunit/google3_gunit",
    MOZILLA_HOME + "/xulrunner/aw_power_editor/components/src",
    GIT_ROOT + "/googleclient/aw_power_editor/../../google3",
]

INCLUDE_HEADERS=[
    MOZILLA_HOME + "/config/gcc_hidden.h"
    ]

#print " ".join(CMD)

def run_process(exe):
    print str(exe)
    p = subprocess.Popen(exe, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
    while(True):
      retcode = p.poll() #returns None while subprocess is running
      line = p.stdout.readline()
      yield line
      if(retcode is not None):
        break

def clang(cc_file):
    cmd = " ".join((["clang++"] + 
           OPTIONS + 
           DIRECTIVES + 
           ["-I" + inc for inc in INCLUDE_DIRS] + 
           ["-include" + inc for inc in INCLUDE_HEADERS] + 
           [cc_file]))
    for line in run_process(cmd):
        print line

clang(os.path.abspath(sys.argv[1]))

