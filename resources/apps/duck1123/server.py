import http.server
import os

PORT = int(os.environ.get("PORT", "8080"))


class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory="/nix/var/result", **kwargs)

    def end_headers(self):
        # Required by NIP-05 for /.well-known/nostr.json; harmless on the
        # rest of the (public, read-only) site.
        self.send_header("Access-Control-Allow-Origin", "*")
        super().end_headers()


print(f"duck1123 static server listening on :{PORT}", flush=True)
http.server.HTTPServer(("", PORT), Handler).serve_forever()
