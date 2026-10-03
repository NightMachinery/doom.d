#!/usr/bin/env python3
"""Run batch ERT against a fabricated HTTP server, never the live Emacs."""
import http.server, json, os, pathlib, subprocess, tempfile, threading, time
requests = []
class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args): pass
    def do_POST(self):
        body=json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        requests.append(body)
        if self.path=='/slow': time.sleep(1)
        self.send_response(401 if self.path=='/broken' else 200); self.end_headers()
        try: self.wfile.write(b'{"detail":"fabricated failure"}' if self.path=='/broken' else b'{"choices":[{"message":{"content":" 0"}}]}')
        except BrokenPipeError: pass
server=http.server.ThreadingHTTPServer(('127.0.0.1',0),Handler)
threading.Thread(target=server.serve_forever,daemon=True).start()
with tempfile.TemporaryDirectory(prefix='emacs-fim-',dir='/private/tmp') as d:
    config=pathlib.Path(d)/'providers.json'
    config.write_text(json.dumps({'providers':{name:{'endpoint':f'http://127.0.0.1:{server.server_port}/{name}','model':'stub','key_env':'stub_key','extract':'chat'} for name in ['stub','broken','slow']}}))
    test=pathlib.Path(__file__).with_name('night-llm-fim-test.el')
    env=dict(os.environ,LLM_COMPLETE_CONFIG=str(config))
    subprocess.run(['emacs','-Q','--batch','-l',str(test)],env=env,check=True,timeout=30)
    body=next(r for r in requests if r['prompt']=='count =')
    assert body['max_tokens']==9 and body['stop']==['END'] and body['suffix']=='tail'
server.shutdown()
