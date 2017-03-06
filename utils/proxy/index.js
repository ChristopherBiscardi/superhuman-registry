var http = require('http'),
    httpProxy = require('http-proxy');

httpProxy.createProxyServer({
  target:'http://api:9000'
}).listen(8000);
