import re, os, sys, tempfile, collections, copy, time, util

# cppdepends v1.0.  (C) 2009, 2010 Nathaniel Flath.
# http://github.com/nflath/cppdepends

class DepGraph:

    def __init__(self, source_roots, include_dirs):
        t = time.clock()
        self.include_dirs = include_dirs
        self.nodes = collections.defaultdict(lambda : [])
        self.nodes_reverse = collections.defaultdict(lambda : [])
        self.num_node_included = collections.defaultdict(lambda : 0)
        for root in source_roots:
            self.__generate_edges_for_dir(root)
        print "Time to build dependency graph: " + str(time.clock() - t) + " secs."
        sys.stdout.flush()

    def __expand_include(self, frag):
        for d in self.include_dirs:
            if os.path.exists(os.path.join(d,frag)):
                return os.path.realpath(os.path.join(d,frag))
        return None


    def __generate_edges_for_dir(self, dir):
        """Iterates through all files in dir, creating a dictionary representing the graph.
        Will call itself recursively on directories, so that all files are
        processed."""
        include_pattern = re.compile( """#include *"([^"]*)""" )
        ignore_pattern = re.compile( "~|\\.svn")
        try:
            for f in os.listdir(dir):
                full_file = os.path.realpath(os.path.join(dir,f))
                if os.path.isfile(full_file) and not ignore_pattern.search(f):
                    for line in file(full_file):
                        m = include_pattern.match(line)
                        if m is not None:
                            include_fragment = m.group(1)
                            include_path = self.__expand_include(include_fragment)
                            if include_path is not None:
                                self.nodes[include_path] += [full_file]
                                self.nodes_reverse[full_file] += [include_path]
                                self.num_node_included[include_path] += 1
                            else:
                                print "Oops. Could not resolve include: " + include_fragment
                elif not ignore_pattern.search(f):
                    self.__generate_edges_for_dir(full_file)
        except Exception as e: 
            sys.stderr.write(str(e))


    def files_including(self, header_filename):
        """Return a list of all files that transitively include on header_filename"""
        t = time.clock()
        result = []
        closed = {}
        q = [header_filename]
        while len(q) > 0:
            cur = q[0]
            q = q[1:]
            result.append(cur)
            closed[cur] = True
            for f in self.nodes[cur]:
                if not f in closed:
                    q.append(f)
        print "Time to get files including on...: " + str(time.clock() - t) + " secs."
        return list(set(result))


    def headers_included_by(self, filename):
        """Return a list of all headers that transitively included by filename"""
        t = time.clock()
        result = []
        closed = {}
        q = [filename]
        while len(q) > 0:
            cur = q[0]
            q = q[1:]
            result.append(cur)
            closed[cur] = True
            for f in self.nodes_reverse[cur]:
                if not f in closed:
                    q.append(f)
        print "Time to get headers included by...: " + str(time.clock() - t) + " secs."
        return list(set(result))

