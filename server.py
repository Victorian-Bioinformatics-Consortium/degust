#!/usr/bin/env python


import sys, os
import CGIHTTPServer, BaseHTTPServer


os.system("./deploy-dev.sh")
os.chdir("dist-dev")



class CGIExtHTTPRequestHandler(CGIHTTPServer.CGIHTTPRequestHandler):
    def is_cgi(self):
        base = self.path
        query = ''
        i = base.find('?')
        if i != -1:
            query = base[i:]
            base = base[:i]
        if not base.lower().endswith('.cgi'):
            return False
        [parentDirs, script] = base.rsplit('/', 1)
        self.cgi_info = (parentDirs, script+query)
        return True
   

def run_server(port):
    dirName = os.getcwd()
    blanks = dirName.count(' ')
    if 0 < blanks:  # server cannot handle blanks in path names
        print("The path to this directory contains spaces")
        return
        
    server_addr = ('localhost', port)
    cgiServer = BaseHTTPServer.HTTPServer(server_addr, CGIExtHTTPRequestHandler)
    sys.stderr.write('Localhost CGI server started : %s\n.'%port)
    cgiServer.serve_forever()

run_server(8000)
