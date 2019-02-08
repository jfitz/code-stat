//<![CDATA[
var logMetaError = function(n) {
        (new Image).src = _G.lsUrl + '&Type=Event.ClientInst&DATA=[{"T":"CI.MetaError","FID":"CI","Name":"MetaJSError","Text":"' + escape(n) + '"}]'
    },
    getHref = function() {
        return location.href
    },
    regexEscape;
try {
    regexEscape = function(n) {
        return n.replace(/([.?*+^$&[\]\\(){}|<>-])/g, "\\$1")
    };

    function jsErrorHandler(n) {
        var s, r, y, p, u, f, w, e, h, c, o;
        try {
            if (s = "ERC", r = window[s], r = r ? r + 1 : 1, r === 16 && (n = new Error("max errors reached")), r > 16) return;
            window[s] = r;
            var l = n.error || n,
                b = '"noMessage"',
                k = n.filename,
                d = n.lineno,
                g = n.colno,
                nt = n.extra,
                a = l.severity || "Error",
                tt = l.message || b,
                i = l.stack,
                t = '"' + escape(tt.replace(/"/g, "")) + '"',
                it = new RegExp(regexEscape(getHref()), "g");
            if (i) {
                for (y = /\(([^\)]+):[0-9]+:[0-9]+\)/g, u = {};
                    (p = y.exec(i)) !== null;) f = p[1], u[f] ? u[f]++ : u[f] = 1;
                e = 0;
                for (h in u) u[h] > 1 && (c = regexEscape(h), w = new RegExp(c, "g"), i = i.replace(w, e), i += "#" + e + "=" + c, e++);
                i = i.replace(it, "self").replace(/"/g, "");
                t += ',"Stack":"' + (escape(i) + '"')
            }
            if (k && (t += ',"Meta":"' + escape(k.replace(it, "self")) + '"'), d && (t += ',"Line":"' + d + '"'), g && (t += ',"Char":"' + g + '"'), nt && (t += ',"ExtraInfo":"' + nt + '"'), tt === b)
                if (a = "Warning", t += ',"ObjectToString":"' + n.toString() + '"', JSON && JSON.stringify) t += ',"JSON":"' + escape(JSON.stringify(n)) + '"';
                else
                    for (o in n) n.hasOwnProperty(o) && (t += ',"' + o + '":"' + n[o] + '"');
            var rt = (new Date).getTime(),
                ut = '"T":"CI.' + a + '","FID":"CI","Name":"JS' + a + '","Text":' + t + "",
                ft = "<E><T>Event.ClientInst<\/T><IG>" + _G.IG + "<\/IG><TS>" + rt + "<\/TS><D><![CDATA[[{" + ut + "}]]\]><\/D><\/E>",
                et = "<ClientInstRequest><Events>" + ft + "<\/Events><STS>" + rt + "<\/STS><\/ClientInstRequest>",
                v = new XMLHttpRequest;
            v.open("POST", "/fd/ls/lsp.aspx?", !0);
            v.setRequestHeader("Content-Type", "text/xml");
            v.send(et);
            typeof sj_evt != "undefined" && sj_evt.fire("ErrorInstrumentation", t)
        } catch (ot) {
            logMetaError("Failed to execute error handler. " + ot.message)
        }
    }
    window.addEventListener && window.addEventListener("error", jsErrorHandler, !1);
    window.addEventListener || window.onerror || (window.onerror = function(n, t, i, r, u) {
        var f = "",
            e;
        typeof n == "object" && n.srcElement && n.srcElement.src ? f = "\"ScriptSrc = '" + escape(n.srcElement.src.replace(/'/g, "")) + "'\"" : (n = "" + n, f = '"' + escape(n.replace(/"/g, "")) + '","Meta":"' + escape(t) + '","Line":' + i + ',"Char": ' + r, u && u.stack && (e = new RegExp(regexEscape(getHref()), "g"), f += ',"Stack":"' + escape(u.stack.replace(e, "self").replace(/"/g, "") + '"')));
        (new Image).src = _G.lsUrl + '&Type=Event.ClientInst&DATA=[{"T":"CI.GetError","FID":"CI","Name":"JSGetError","Text":' + f + "}]";
        typeof sj_evt != "undefined" && sj_evt.fire("ErrorInstrumentation", f)
    })
} catch (e) {
    logMetaError("Failed to bind error handler " + e.message)
};
var amd, define, require;
(function(n) {
    function e(n, i, u) {
        t[n] || (t[n] = {
            dependencies: i,
            callback: u
        }, r(n))
    }

    function r(n) {
        if (n) {
            if (n) return u(n)
        } else {
            if (!f) {
                for (var r in t) u(r);
                f = !0
            }
            return i
        }
    }

    function u(n) {
        var s, e;
        if (i[n]) return i[n];
        if (t.hasOwnProperty(n)) {
            var h = t[n],
                f = h.dependencies,
                l = h.callback,
                a = r,
                o = {},
                c = [a, o];
            if (f.length < 2) throw "invalid usage";
            else if (f.length > 2)
                for (s = f.slice(2, f.length), e = 0; e < s.length; e++) c.push(u(s[e]));
            return l.apply(this, c), i[n] = o, o
        }
    }
    var t = {},
        i = {},
        f = !1;
    n.define = e;
    n.require = r
})(amd || (amd = {}));
define = amd.define;
require = amd.require;
var _w = window,
    _d = document,
    sb_ie = window.ActiveXObject !== undefined,
    sb_i6 = sb_ie && !_w.XMLHttpRequest,
    _ge = function(n) {
        return _d.getElementById(n)
    },
    _qs = function(n, t) {
        return t = typeof t == "undefined" ? _d : t, t.querySelector ? t.querySelector(n) : null
    },
    sb_st = function(n, t) {
        return setTimeout(n, t)
    },
    sb_rst = sb_st,
    sb_ct = function(n) {
        clearTimeout(n)
    },
    sb_gt = function() {
        return (new Date).getTime()
    },
    sj_gx = function() {
        return sb_i6 ? new ActiveXObject("MSXML2.XMLHTTP") : new XMLHttpRequest
    };
_w.sj_ce = function(n, t, i) {
    var r = _d.createElement(n);
    return t && (r.id = t), i && (r.className = i), r
};
_w.sj_cook = {
    get: function(n, t) {
        var i = _d.cookie.match(new RegExp("\\b" + n + "=[^;]+")),
            r;
        return t && i ? (r = i[0].match(new RegExp("\\b" + t + "=([^&]*)")), r ? r[1] : null) : i ? i[0] : null
    }
};
_w.sk_merge || (_w.sk_merge = function(n) {
    _d.cookie = n
});
define("fallback", ["require", "exports"], function(n, t) {
    function f() {
        return function() {
            for (var r, h, c, t = [], n = 0; n < arguments.length; n++) t[n] = arguments[n];
            if (r = s(arguments.callee), u && (h = e(r), h.toString() != f().toString())) return h.apply(null, arguments);
            c = i[r].q;
            t[0] === "onPP" && o();
            c.push(t)
        }
    }

    function s(n) {
        for (var t in i)
            if (i[t].h === n) return t
    }

    function e(n, t) {
        for (var u, e = n.split("."), i = _w, r = 0; r < e.length; r++) u = e[r], typeof i[u] == "undefined" && t && (i[u] = r === e.length - 1 ? f() : {}), i = i[u];
        return i
    }

    function o() {
        var e = i["rms.js"].q,
            o, f, t, n, r, u;
        if (e.length > 0)
            for (o = !1, f = 0; f < e.length; f++) {
                for (t = e[f], n = 0; n < t.length; n++)
                    if (r = t[n]["A:rms:answers:Shared:BingCore.Bundle"], r || (r = t[n]["A:rmsBu0"]), r) {
                        u = _d.createElement("script");
                        u.setAttribute("data-rms", "1");
                        u.src = r;
                        u.type = "text/javascript";
                        setTimeout(function() {
                            _d.body.appendChild(u)
                        }, 0);
                        t.splice(n, 1);
                        o = !0;
                        break
                    } if (o) break
            }
    }

    function h() {
        var n, t, f;
        for (u = !1, n = 0; n < r.length; n++) t = r[n], f = e(t, !0), i[t] = {
            h: f,
            q: []
        }
    }

    function c() {
        for (var t, n = 0; n < r.length; n++) {
            var o = r[n],
                s = i[o].q,
                h = e(o);
            for (t = 0; t < s.length; t++) h.toString() !== f().toString() && h.apply(null, s[t])
        }
        u = !0
    }

    function l(n, t, i, r) {
        n && ((n === _w || n === _d || n === _d.body) && t == "load" ? _w.sj_evt.bind("onP1", i, !0) : n.addEventListener ? n.addEventListener(t, i, r) : n.attachEvent ? n.attachEvent("on" + t, i) : n["on" + t] = i)
    }
    t.__esModule = !0;
    var r = ["rms.js", "sj_evt.bind", "sj_evt.fire", "sj_jb", "sj_wf", "sj_cook.get", "sj_cook.set", "sj_pd", "sj_sp", "sj_be", "sj_go", "sj_ev", "sj_ue", "sj_evt.unbind", "sj_et", "Log.Log", "sj_mo", "sj_so"],
        i = {},
        u = !1;
    _w.fb_is = o;
    t.replay = c;
    h();
    _w.sj_be = l
});

function lb() {
    _w.si_sendCReq && sb_st(_w.si_sendCReq, 800);
    _w.lbc && _w.lbc()
};
(function() {
    function n(n) {
        n = sb_ie ? _w.event : n;
        (!n.altKey || n.ctrlKey || n.shiftKey) && (n.key && n.key === "Enter" || n.keyCode && n.keyCode === 13) && _w.si_ct(sb_ie ? n.srcElement : n.target, !1, n, "enter")
    }
    sj_be(document, "keydown", n, !1)
})();
(function() {
    function n(n) {
        _w.si_ct(sb_ie ? _w.event.srcElement : n.target, !1, _w.event || n)
    }
    sj_be(document, "mousedown", n, !1)
})(); /*!DisableJavascriptProfiler*/
0; /*!DisableJavascriptProfiler*/
0; /*!DisableJavascriptProfiler*/
0;
ClTrCo = {
    furl: !0
};
var ctcc = 0,
    clc = _w.ClTrCo || {};
_w.si_ct = function(n, t, i, r) {
    var u, e, f, o, s, h, c;
    if (clc.SharedClickSuppressed) return !0;
    u = "getAttribute";
    try {
        for (; n !== document.body; n = n.parentNode) {
            if (!n || n === document || n[u]("data-noct")) break;
            if (e = (n.tagName === "A" || n[u]("data-clicks")) && (n[u]("h") || n[u]("data-h")) || n[u]("_ct"), e) {
                f = n[u]("_ctf");
                o = -1;
                i && (i.type === "keydown" ? o = -2 : i.button != null && (o = i.button));
                f && _w[f] || (f = "si_T");
                f === "si_T" && (s = encodeURIComponent(n[u]("href")), clc.furl && !n[u]("data-private") ? e += "&url=" + s : clc.mfurl && (e += "&abc=" + s));
                r && (e += "&source=" + r);
                h = "";
                clc.mc && (h = "&c=" + ctcc++);
                c = "&" + e + h;
                _w.si_sbwu(c) || _w[f] && _w[f](c, n, i, o);
                break
            }
            if (t) break
        }
    } catch (l) {
        _w.SharedLogHelper ? SharedLogHelper.LogWarning("clickEX", null, l) : (new Image).src = _G.lsUrl + '&Type=Event.ClientInst&DATA=[{"T":"CI.Warning","FID":"CI","Name":"JSWarning","Text":' + l.message + "}]"
    }
    return !0
};
_w.si_sbwu || (_w.si_sbwu = function() {
        return !1
    }),
    function() {
        _w._G && (_G.si_ct_e = "click")
    }();
var wlc_d = 1500,
    wlc_t = 63685183923;;
var sj_log = function(n, t, i) {
    var r = new RegExp('"', "g");
    (new Image).src = _G.lsUrl + '&Type=Event.ClientInst&DATA=[{"T":"' + n + '","FID":"CI","Name":"' + t + '","Text":"' + escape(i.replace(r, "")) + '"}]'
};
(function(n, t, i) {
    function r(n) {
        var t = n.indexOf("#");
        return t === -1 ? n : n.substring(0, t)
    }

    function u() {
        var n = location.href;
        r(location.href) !== r(i) && location.replace(n + "&ajf=100")
    }
    sj_be(n, t, u, !1);
    sj_evt.bind("ajaxReady", function() {
        sj_ue(n, t, u, !1)
    })
})(_w, "popstate", location.href);
var perf;
(function(n) {
    function f(n) {
        return i.hasOwnProperty(n) ? i[n] : n
    }

    function e(n) {
        var t = "S";
        return n == 0 ? t = "P" : n == 2 && (t = "M"), t
    }

    function o(n) {
        for (var c, i = [], t = {}, r, l = 0; l < n.length; l++) {
            var a = n[l],
                o = a.v,
                s = a.t,
                h = a.k;
            s === 0 && (h = f(h), o = o.toString(36));
            s === 3 ? i.push(h + ":" + o) : (r = t[s] = t[s] || [], r.push(h + ":" + o))
        }
        for (c in t) t.hasOwnProperty(c) && (r = t[c], i.push(e(+c) + ':"' + r.join(",") + '"'));
        return i.push(u), i
    }
    for (var r = ["redirectStart", "redirectEnd", "fetchStart", "domainLookupStart", "domainLookupEnd", "connectStart", "secureConnectionStart", "connectEnd", "requestStart", "responseStart", "responseEnd", "domLoading", "domInteractive", "domContentLoadedEventStart", "domContentLoadedEventEnd", "domComplete", "loadEventStart", "loadEventEnd", "unloadEventStart", "unloadEventEnd", "firstChunkEnd", "secondChunkStart", "htmlEnd", "pageEnd", "msFirstPaint"], u = "v:1.1", i = {}, t = 0; t < r.length; t++) i[r[t]] = t;
    n.compress = o
})(perf || (perf = {}));
window.perf = window.perf || {},
    function(n) {
        n.log = function(t, i) {
            var f = n.compress(t),
                r;
            f.push('T:"CI.Perf",FID:"CI",Name:"PerfV2"');
            var e = "/fd/ls/lsp.aspx?",
                o = "sendBeacon",
                h = "<E><T>Event.ClientInst<\/T><IG>" + _G.IG + "<\/IG><TS>" + i + "<\/TS><D><![CDATA[{" + f.join(",") + "}]\]><\/D><\/E>",
                s = "<ClientInstRequest><Events>" + h + "<\/Events><STS>" + i + "<\/STS><\/ClientInstRequest>",
                u = !_w.navigator || !navigator[o];
            if (!u) try {
                navigator[o](e, s)
            } catch (c) {
                u = !0
            }
            u && (r = sj_gx(), r.open("POST", e, !0), r.setRequestHeader("Content-Type", "text/xml"), r.send(s))
        }
    }(window.perf);
var perf;
(function(n) {
    function a() {
        return c(Math.random() * 1e4)
    }

    function o() {
        return y ? c(f.now()) + l : +new Date
    }

    function v(n, r, f) {
        t.length === 0 && i && sb_st(u, 1e3);
        t.push({
            k: n,
            v: r,
            t: f
        })
    }

    function p(n) {
        return i || (r = n), !i
    }

    function w(n, t) {
        t || (t = o());
        v(n, t, 0)
    }

    function b(n, t) {
        v(n, t, 1)
    }

    function u() {
        var u, f;
        if (t.length) {
            for (u = 0; u < t.length; u++) f = t[u], f.t === 0 && (f.v -= r);
            t.push({
                k: "id",
                v: e,
                t: 3
            });
            n.log(t, o());
            t = [];
            i = !0
        }
    }

    function k() {
        r = o();
        e = a();
        i = !1;
        sj_evt.bind("onP1", u)
    }
    var s = "performance",
        h = !!_w[s],
        f = _w[s],
        y = h && !!f.now,
        c = Math.round,
        t = [],
        i = !1,
        l, r, e;
    h ? l = r = f.timing.navigationStart : r = _w.si_ST ? _w.si_ST : +new Date;
    e = a();
    n.setStartTime = p;
    n.mark = w;
    n.record = b;
    n.flush = u;
    n.reset = k;
    sj_be(window, "load", u, !1);
    sj_be(window, "beforeunload", u, !1)
})(perf || (perf = {}));
_w.si_PP = function(n, t, i) {
    var r, o, l, h, e, c;
    if (!_G.PPS) {
        for (o = ["FC", "BC", "SE", "TC", "H", "BP", null]; r = o.shift();) o.push('"' + r + '":' + (_G[r + "T"] ? _G[r + "T"] - _G.ST : -1));
        var u = _w.perf,
            s = "navigation",
            r, f = i || _w.performance && _w.performance.timing;
        if (f && u) {
            if (l = f.navigationStart, u.setStartTime(l), l >= 0)
                for (r in f) h = f[r], typeof h == "number" && h > 0 && r !== "navigationStart" && r !== s && u.mark(r, h);
            u.record("nav", s in f ? f[s] : performance[s].type)
        }
        e = "connection";
        c = "";
        _w.navigator && navigator[e] && (c = ',"net":"' + navigator[e].type + '"', navigator[e].downlinkMax && (c += ',"dlMax":"' + navigator[e].downlinkMax + '"'));
        _G.PPImg = new Image;
        _G.PPImg.src = _G.lsUrl + '&Type=Event.CPT&DATA={"pp":{"S":"' + (t || "L") + '",' + o.join(",") + ',"CT":' + (n - _G.ST) + ',"IL":' + _d.images.length + "}" + (_G.C1 ? "," + _G.C1 : "") + c + "}" + (_G.P ? "&P=" + _G.P : "") + (_G.DA ? "&DA=" + _G.DA : "") + (_G.MN ? "&MN=" + _G.MN : "");
        _G.PPS = 1;
        sb_st(function() {
            u && u.flush();
            sj_evt.fire("onPP");
            sj_evt.fire(_w.p1)
        }, 1)
    }
};
_w.onbeforeunload = function() {
    si_PP(new Date, "A")
};
sj_evt.bind("ajax.requestSent", function() {
    window.perf && perf.reset()
});
(function(n) {
    var i, r, t;
    if (document.querySelector) {
        i = [];
        r = "ad";

        function u() {
            var w = sb_gt(),
                c = document.documentElement,
                e = document.body,
                u = -1,
                r = -1,
                l = c.clientHeight,
                a = ["#b_results ." + _G.adc, ".sb_adsWv2", ".ads"],
                n, o, s, f, v, t;
            if (e) {
                n = 0;
                o = document.querySelector("#b_pole .b_adSlug");
                o && (s = document.querySelector("#b_pole"), n = s.offsetHeight, r = s.offsetTop);
                var y = document.querySelector("#b_results #productAdCarousel"),
                    h = document.querySelector("#b_results .pa_b_supertop"),
                    p = document.querySelector("#b_results .bn_wide");
                for (h ? (r = h.offsetTop, n = h.offsetHeight) : p ? n += p.offsetHeight : y && (n += y.offsetHeight), f = 0; f < a.length; f++)
                    if (v = a[f], t = document.querySelector(v), t && t.className.indexOf("b_adBottom") == -1 && t.offsetTop < l) {
                        u = t.offsetHeight + n;
                        r === -1 && (r = t.offsetTop);
                        break
                    } o && u == -1 && (u = n);
                i = [r, u, c.clientWidth, l, e.offsetWidth, e.offsetHeight, sb_gt() - w]
            }
        }
        n ? (t = n.onbeforefire, n.onbeforefire = function() {
            t && t();
            u();
            n.mark(r, i)
        }) : (t = si_PP, si_PP = function() {
            u();
            var n = '"' + r + '":[' + i.join() + "]";
            _G.C1 = _G.C1 ? _G.C1 + "," + n : n;
            t.apply(null, [].slice.apply(arguments))
        })
    }
})(_w.pp);
_w.AM = ["live.com", "virtualearth.net", "windows.net", "onenote", "hexun.com", "dict.bing.com.cn", "msn.com", "variflight.com", "bing.net", "msftoffers.com", "chinacloudapp.cn", "cbsnews.com", "swx.cdn.skype.com", "swc.cdn.skype.com", "latest-swx.cdn.skype.com", "a.config.skype.com", "b.config.skype.com", "platform.bing.com", "microsofttranslator.com", "bing.com", "facebook.net", ".delve.office.com"];
_w.APD = ["live.com", "virtualearth.net", "windows.net", "onenote", "hexun.com", "dict.bing.com.cn", "msn.com", "variflight.com", "bing.net", "msftoffers.com", "chinacloudapp.cn", "facebook.com", "skype.com", "msecnd.net", "bingsandbox.com", "bfb", "bfb-int", "platform.bing.com", "youtube.com", "msecnd.net", "photosynth.net", "microsofttranslator.com", "microsoft.com", "vimeo.com", "bing.com", "bing-int.com", "microsoftonline.com", "swc.cdn.skype.com", "latest-swx.cdn.skype.com", "a.config.skype.com", "b.config.skype.com", "cbsnews.com", "swx.cdn.skype.com", "videoplayercdn.osi.office.net", "channel9.msdn.com", "cnn.com", "edition.cnn.com", "wsj.com", "euronews.com", "huffingtonpost.com", "mashable.com", "mtv.com", "zdnet.com", "widgets.ign.com", "dailymotion.com", "embed.vevo.com", "video.disney.com", "downvids.net", "imdb.com", "widget.uservoice.com", "web.powerapps.com", ".delve.office.com", "fave.api.cnn.io"];
_w.APC = ["bm_", "panelWrapper", "df_topAlAs", "Light", "Dark", "taskbar", "ssSIV", "square_", "tall_", "item", "sw_", "sb_", "sml", "ftrd", "sa_", "id_", "sc_", "flt_", "fc_", "cca", "tab-", "emb", "ctx", "dc_", "cipa", "dict", "btm", "wtr", "wpc", "fin", "sp-", "carousel", "vp_", "vid", "nav_", "vt", "va_", "avc", "cic", "sports", "lc_", "bing", "dmap_", "pvc_", "ans_", "mcd", "composite", "mt_", "irp", "iap", "tv", "aggtv", "irhc", "vrh", "det", "tit", "sub", "col", "card", "hlsel", "ovl", "ctpt", "bubble", "memodal", "meoverlay", "c_", "spl-", "microsoft", "skp", "saa", "unlockButton", "overlay", "MapPushpinBase", "pa_", "skype_", "ftrSbR", "quizContainer", "alrt_", "st_", "expan", "word", "rpt_", "o_", "e_", "searchbar", "row", "Traffic", "tl", "gray", "bep", "wk_", "crs_", "w10", "personal", "fs3_", "ezp_", "hp", "post", "mc_", "fb", "lgb", "el_", "perf", "stb", "PP", "bw", "infobubble", "l_", "ms-", "NavBar_", "cmt_", "bottom", "Copyright", "upsell", "ab_", "w_", "hlig", "eachStep", "close_", "cGifIcon", "cThIcon", "autosuggest", "showtimesMovie", "sel", "dish", "formatShowtimes", "wp_", "hasExpandText", "forecast", "as_", "ecmp", "cmp", "comp", "userChat", "bot", "bTyp", "team", "serp", "preG", "option", "azBxInsert", "ec_", "cs_", "spin", "skype-conversation", "conversation", "fs_", "grammarly", "filterBar", "withFilters", "textanno", "mv_lm", "usagTpVsDosage", "trans_button_group", "algo_action_template", "meg_item", "ev_msg_wrapper", "ol_", "offer", "embed", "videoplayercdn", "searchNearby", "directionsPanel", "dragOverlay", "infobox", "mss", "noneG", "usage", "drImp", "sf_", "dfindOverModal", "circuit", "swc", "CodeMirror", "cm-s-default", "msg msg-warning", "LogoContainer", "quadrantOverride", "ac-", "gc-", "fsmd-", "fsg-", "fsmf-", "msto_", "rq", "geoItm", "bqaq_quotes", "bqap_padding", "loc", "ent_cnt", "r_mf", "exp_", "btOverlay", "mnot_container", "info_C", "ev_talkbox_notification", "ev_talkbox_wrapper_min", "p_tr_", "slide", "bnc-", "itr_poi", "cg-", "elmlr_", "scrl", "gam-", "htv-", "genel-", "gs_"];
(function() {
    function i(n, t, i) {
        for (var r = 0; r < t.length; r++)
            if (i === "C" && n.toUpperCase().indexOf(t[r].toUpperCase()) === 0 || i === "S" && n.toUpperCase().indexOf(t[r].toUpperCase()) >= 0) return !0;
        return !1
    }

    function e(n, t, i) {
        sj_log("CI.AdPrevention", n, t + ":" + i)
    }

    function o(n) {
        while (n && n.tagName !== "BODY") {
            if (n.className && n.className.indexOf("lpc_ip_root_class") >= 0) return !0;
            n = n.parentNode
        }
        return !1
    }

    function n(n, t, i) {
        n.parentNode && n.parentNode.nodeType === 1 && n.nodeType === 1 && n.offsetWidth && n.offsetWidth > 20 && (t += "D", n.parentNode.removeChild(n), e(t, n.tagName, i))
    }

    function s(r) {
        r.src && (t.href = r.src, window.location.hostname.indexOf(t.hostname) < 0 && !i(t.hostname, c, "S") && n(r, "RS", t.hostname))
    }

    function h(t) {
        var r = t.className,
            u = r && r.trim();
        !u || u.indexOf("b_") === 0 || i(u, l, "C") || o(t) || n(t, "RC", r)
    }

    function r(t) {
        var i = window.location.pathname,
            r;
        (window.MutationObserver || window.WebKitMutationObserver) && i && (i.toUpperCase() === "/SEARCH" || i.toUpperCase() === "/") && typeof MutationObserver != "undefined" && (r = new MutationObserver(function(t) {
            var f, r, e, i;
            if (t)
                for (f = 0; f < t.length; f++)
                    if (r = t[f].addedNodes, r && r.length)
                        for (e = 0; e < r.length; e++) {
                            i = r[e];
                            switch (i.tagName) {
                                case "IFRAME":
                                case "IMG":
                                case "SCRIPT":
                                case "LINK":
                                    s(i);
                                    break;
                                case "DIV":
                                case "LI":
                                    h(i);
                                    break;
                                case "OBJECT":
                                    i.type && i.type.indexOf("flash") >= 0 && n(i, "RN", i.outerHTML.substr(0, u));
                                    break;
                                case "CENTER":
                                    n(i, "RN", i.outerHTML.substr(0, u));
                                    break;
                                default:
                                    return
                            }
                        }
        }), r.observe(t, {
            childList: !0,
            subtree: !0
        }))
    }
    var t = sj_ce("a"),
        u = 100,
        c = _w.APD ? _w.APD.slice() : [],
        l = _w.APC ? _w.APC.slice() : [],
        f;
    r(document.getElementsByTagName("head")[0]);
    f = function() {
        r(document.getElementsByTagName("body")[0])
    };
    window.addEventListener("load", f)
})();
(function() {
    function t(t, i) {
        var u = t.tagName;
        return (u === "SCRIPT" && (n.href = t.src) || u === "OBJECT" && t.type && t.type.indexOf("flash") > 0 && (n.href = t.data)) && n.href.length > 0 && n.hostname.length > 0 && n.hostname !== location.hostname && !e(n.hostname) ? (sj_log("CI.AntiMalware", i, u.substr(0, 1) + ":" + n.href.substr(0, r)), !1) : !0
    }

    function e(n) {
        for (var t = 0; t < i.length; t++)
            if (n.indexOf(i[t]) >= 0) return !0;
        return !1
    }
    var i = _w.AM,
        r = 100,
        n = document.createElement("A"),
        u, f;
    document.write = function(n) {
        n.length > 0 && sj_log("CI.AntiMalware", "DW", n.substr(0, r))
    };
    typeof Element != "undefined" && Element.prototype && (u = Element.prototype.appendChild, Element.prototype.appendChild = function(n) {
        return t(n, "AC") ? u.apply(this, arguments) : null
    }, f = Element.prototype.insertBefore, Element.prototype.insertBefore = function(n) {
        return t(n, "IB") ? f.apply(this, arguments) : null
    })
})();
var BM = BM || {},
    adrule = "." + _G.adc + " > ul";
BM.rules = {
    ".b_scopebar": [0, 80, 0],
    ".b_logo": [-1, -1, 0],
    ".b_searchboxForm": [100, 19, 0],
    "#id_h": [-1, -1, 0],
    "#b_tween": [-1, -1, 1],
    "#b_results": [100, -1, 1],
    "#b_context": [710, -1, 1],
    "#b_navheader": [-1, -1, 0],
    "#bfb-answer": [-1, -1, 1],
    ".tab-menu > ul": [-1, -1, 1],
    ".b_footer": [0, -1, 0],
    "#b_notificationContainer": [-1, -1, 0],
    "#ajaxMaskLayer": [-1, -1, 0],
    "img,div[data-src],.rms_img": [-1, -1, 0],
    iframe: [-1, -1, 0]
};
BM.rules[adrule] = [-1, -1, 1];
var BM = BM || {};
(function(n) {
    function u(n, u) {
        n in t || (t[n] = []);
        !u.compute || n in r || (r[n] = u.compute);
        !u.unload || n in i || (i[n] = u.unload);
        u.load && u.load()
    }

    function f(n, i) {
        t[n].push({
            t: s(),
            i: i
        })
    }

    function e(n) {
        return n in i && i[n](), n in t ? t[n] : void 0
    }

    function o() {
        for (var n in r) r[n]()
    }

    function s() {
        return window.performance && performance.now ? Math.round(performance.now()) : new Date - window.si_ST
    }
    var t = {},
        i = {},
        r = {};
    n.wireup = u;
    n.enqueue = f;
    n.dequeue = e;
    n.trigger = o
})(BM);
(function(n) {
    function i() {
        var i = document.documentElement,
            r = document.body,
            u = "innerWidth" in window ? window.innerWidth : i.clientWidth,
            f = "innerHeight" in window ? window.innerHeight : i.clientHeight,
            e = window.pageXOffset || i.scrollLeft,
            o = window.pageYOffset || i.scrollTop,
            s = document.visibilityState || "default";
        n.enqueue(t, {
            x: e,
            y: o,
            w: u,
            h: f,
            dw: r.clientWidth,
            dh: r.clientHeight,
            v: s
        })
    }
    var t = "V";
    n.wireup(t, {
        load: null,
        compute: i,
        unload: null
    })
})(BM);
(function(n) {
    function i() {
        var e, o, u, s, f, r;
        if (document.querySelector && document.querySelectorAll) {
            e = [];
            o = n.rules;
            for (u in o)
                for (s = o[u], u += !s[2] ? "" : " >*", f = document.querySelectorAll(u), r = 0; r < f.length; r++) {
                    var i = f[r],
                        h = 0,
                        c = 0,
                        l = i.offsetWidth,
                        a = i.offsetHeight;
                    do h += i.offsetLeft, c += i.offsetTop; while (i = i.offsetParent);
                    e.push({
                        _e: f[r],
                        x: h,
                        y: c,
                        w: l,
                        h: a
                    })
                }
            n.enqueue(t, e)
        }
    }
    var t = "L";
    n.wireup(t, {
        load: null,
        compute: i,
        unload: null
    })
})(BM);
(function(n) {
    function f() {
        u(sj_be, r)
    }

    function r(i) {
        return i && n.enqueue(t, i), !0
    }

    function e() {
        u(sj_ue, r)
    }

    function u(n, t) {
        for (var u, r = 0; r < i.length; r++) u = i[r], n(u === "resize" ? window : document, window.navigator.pointerEnabled ? u.replace("mouse", "pointer") : u, t, !1)
    }
    var t = "EVT",
        i = ["click", "mousedown", "mouseup", "touchstart", "touchend", "mousemove", "touchmove", "scroll", "keydown", "resize"];
    n.wireup(t, {
        load: f,
        compute: null,
        unload: e
    })
})(BM);
FallBackToDefaultProfilePic = function(e) {
    var new_element = document.createElement('span');
    new_element.setAttribute('id', 'id_p');
    new_element.setAttribute('class', 'sw_spd id_avatar');
    new_element.setAttribute('aria-label', "Default Profile Picture");
    var p = e.parentNode;
    p.replaceChild(new_element, e);
};
var DynScopes;
(function(n) {
    function r(n, r) {
        i || (t = n, f(r), sj_evt.bind("onP1", u), i = !0)
    }

    function u() {
        if (sj_cook && sj_cook.set && sj_cook.clear) {
            var n = "dsc";
            sj_cook.clear(n, "/");
            t && sj_cook.set(n, "order", t, !1, "/")
        }
    }

    function f(n) {
        var r = e(),
            i, o, s, t;
        if (r) {
            if (n) {
                var h = n.split(","),
                    u = r.children,
                    c = u.length,
                    f = [];
                for (t = 0; t < h.length; t++) i = h[t].split(":"), i && i.length == 2 && (o = parseInt(i[0]), s = parseInt(i[1]), o < c && s < c && (f[s] = u[o].innerHTML));
                for (t = 0; t < f.length; t++) u[t].innerHTML = f[t]
            }
            r.className = ""
        }
    }

    function e() {
        var n = _d.querySelectorAll(".b_scopebar > .b_scopehide");
        return n && n.length > 0 ? n[0] : null
    }
    var i = !1,
        t;
    n.init = r
})(DynScopes || (DynScopes = {}));
//]]>