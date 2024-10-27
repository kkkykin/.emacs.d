var hostRulesMap = {}; var ipRulesMap = {};

// 定义 IP 地址检测正则，检查 host 是否是 IP 地址
var ipRegex = /^[0-9a-f:.]+$/i;

function FindProxyForURL(url, host) {
    if (isDirectHost(host)) return 'DIRECT';

    // 检查 host 是否在 hostRulesMap 中
    var action = findHostAction(host);
    if (action) return action;

    // IP 地址规则检查
    if (ipRegex.test(host)) {
        for (var action in ipRulesMap) {
            for (var i = 0; i < ipRulesMap[action].length; i++) {
                if (isInNetEx(host, ipRulesMap[action][i])) return action;
            }
        }
    }

    return 'DIRECT';
}

// 检查 host 是否在 hostRulesMap 中
function findHostAction(host) {
    if (hostRulesMap[host]) return hostRulesMap[host];
    var parts = host.split('.');
    while (parts.length > 1) {
        parts.shift();
        var subdomain = parts.join('.');
        if (hostRulesMap[subdomain]) return hostRulesMap[subdomain];
    }
    return null;
}

// 检查 host 是否为局域网域名
function isDirectHost(host) {
    return isPlainHostName(host) || dnsDomainIs(host, '.local') || dnsDomainIs(host, '.localhost');
}

