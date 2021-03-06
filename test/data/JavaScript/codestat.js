function keysInObject(o) {
    var keys = [];
  
    for (var key in o) {
      keys.push(key);
    }
  
    return keys;
  }
  
  function sortedKeysInObject(o) {
    var keys = keysInObject(o);
    var sorted_keys = [];
  
    while (keys.length > 0) {
      var max_value = -1.0;
      var max_index = -1;
      for (var i = 0; i < keys.length; i++) {
        if (o[keys[i]] > max_value) {
          max_value = o[keys[i]];
          max_index = i;
        }
      }
      sorted_keys.push(keys[max_index]);
      keys.splice(max_index, 1);
    }
  
    return sorted_keys;
  }
  
  function longestString(ss) {
    var longest = '';
  
    for (var i = 0; i < ss.length; i++) {
      if (ss[i].length > longest.length) {
        longest = ss[i];
      }
    }
  
    return longest;
  }
  
  function buildConfidenceLine(name, confidence, width, decimals, nameLength) {
      // format the name with trailing spaces
      var numSpaces = nameLength - name.length + 1;
      var spaces = ' '.repeat(numSpaces);
      var nameBar = name + spaces;
      // format the chart (stars) with trailing spaces
      var gridConfidence = Math.round(confidence * width);
      var gridStars = '*'.repeat(gridConfidence);
      var gridSpaces = ' '.repeat(width - gridConfidence);
      var gridBar = '  [' + gridStars + gridSpaces + ']';
      var line = nameBar + confidence.toFixed(decimals) + gridBar;
  
      return line;
  }
  
  function buildConfidenceGrid(confidences) {
    var width = 20;
    var decimals = 5;
    var names = sortedKeysInObject(confidences);
    var nameLength = longestString(names).length;
  
    var confidenceGridHtml = '<code><pre>';
  
      // format the name with trailing spaces
      for (var i = 0; i < names.length; i++) {
        var name = names[i];
        var confidence = confidences[name];
        var line = buildConfidenceLine(name, confidence, width, decimals, nameLength);
        var htmlLine = line + '<br/>';
  
        confidenceGridHtml += htmlLine;
      }
  
    confidenceGridHtml += '</pre></code>';
  
    return confidenceGridHtml;
  }
  
  function keyWithGreatestValue(o) {
    var greatestValueKey = o[0];
    var greatestValue = 0.0;
  
    for (var key in o) {
      if (o[key] > greatestValue) {
        greatestValueKey = key;
        greatestValue = o[key];
      }
    }
  
    return greatestValueKey;
  }
  
  function buildLanguageSelectOptions(languageSelect, names) {
    while (languageSelect.firstChild) {
      languageSelect.removeChild(languageSelect.firstChild);
    }
  
    for (var i = 0; i < names.length; i++) {
      var option = document.createElement('option');
      option.value = names[i];
      option.textContent = names[i];
      languageSelect.appendChild(option);
    }
  }
  
  function loadLanguageNames() {
    var languageSelect = document.getElementById('language');
  
    while (languageSelect.firstChild) {
      languageSelect.removeChild(languageSelect.firstChild);
    }
  
    var code = document.getElementById('code').value;
    var req = new XMLHttpRequest();
    req.open('GET', "/languages", true);
  
    req.onload = function (e) {
      if (req.readyState === 4) {
        if (req.status === 200) {
          var results = req.responseText;
          var names = JSON.parse(results);
  
          names.sort();
  
          buildLanguageSelectOptions(languageSelect, names);
        }
      }
    }
  
    req.send()
  }
  
  function detectLanguage() {
      var results = '';
      var code = document.getElementById('code').value;
      var req = new XMLHttpRequest();
      req.open('POST', "/detect", true);
      req.setRequestHeader("Content-type", "text/plain");
  
      req.onload = function (e) {
        if (req.readyState === 4) {
          if (req.status === 200) {
            results = req.responseText;
            var confidences = JSON.parse(results);
            var names = keysInObject(confidences).sort();
  
            // build list of names
            var languageSelect = document.getElementById('language');
            buildLanguageSelectOptions(languageSelect, names)
  
            // build list of names and confidence values
            var detectDiv = document.getElementById('detect_result');
            var confidenceGridHtml = buildConfidenceGrid(confidences);
            detectDiv.innerHTML = confidenceGridHtml;
  
            if (names.length > 0) {
              // find the most likely language
              languageSelect.value = keyWithGreatestValue(confidences)
            }
          } else {
            console.error(req.statusText);
          }
        }
      };
  
      req.onerror = function (e) {
        console.error(req.statusText);
      };
  
      req.send(code);
  }
  
  function textToSafeHtml(str) {
    var s1 = str.replace(/&/g, '&amp;');
    var s2 = s1.replace(/</g, '&lt;');
    var s3 = s2.replace(/>/g, '&gt;');
    var s4 = s3.replace(/"/g, '&quot;');
    return s4;
  }
  
  function colorize(token, htmlColors) {
    var tokenType = token['type'];
    var tokenValue = token['value'];
    var tokenSafe = textToSafeHtml(tokenValue);
    var tokenHtml = 'empty';
  
    if (tokenType == 'newline') {
      tokenHtml = '<br/>'; 
    } else if (tokenType.startsWith('invalid')) {
      var frontHtml = '<font color=red>';
      var backHtml = '</font>';
      tokenHtml = frontHtml + tokenSafe + backHtml;
    } else if (tokenType in htmlColors) {
      var frontHtml = '<font color=' + htmlColors[tokenType] + '>';
      var backHtml = '</font>';
      tokenHtml = frontHtml + tokenSafe + backHtml;
    } else {
      tokenHtml = tokenSafe;
    }
  
    return tokenHtml;
  }
  
  function formatCounts(counts) {
    var html = ''
  
    var keys = [];
    for (var key in counts) {
      if (counts.hasOwnProperty(key)) {
        keys.push(key);
      }
    }
    var length = keys.length
  
    var total = 0
    for (var i = 0; i < length; i++) {
      var key = keys[i];
      var count = counts[key];
      total += count
      html += key + '|' + count + '<br/>';
    }
    html += 'TOTAL' + '|' + total + '<br/>';
  
    return html;
  }
  
  function formatConfidence(confidences) {
    var html = ''
  
    var keys = [];
    for (var key in confidences) {
      if (confidences.hasOwnProperty(key)) {
        keys.push(key);
      }
    }
    var length = keys.length
  
    for (var i = 0; i < length; i++) {
      var key = keys[i];
      var count = confidences[key];
      html += key + '|' + count + '<br/>';
    }
  
    return html;
  }
  
  function showConfidence() {
    var results = '';
    var code = document.getElementById('code').value;
    var language = document.getElementById('language').value;
    // change 'C++' because the '+' character causes problems for requests
    // TODO: escape the request to allow '+'
    if (language == 'C++') {
      language = 'Cpp';
    }
    if (language == 'C#') {
      language = 'Csharp';
    }
  
    var query = "/confidence?language=" + language;
    var req = new XMLHttpRequest();
    req.open('POST', query, true);
    req.setRequestHeader("Content-type", "text/plain");
    req.onload = function (e) {
      if (req.readyState === 4) {
        if (req.status === 200) {
          var results = req.responseText;
          var confidences = JSON.parse(results);
          var length = confidences.length;
  
          var formattedConfidence = formatConfidence(confidences);
  
          var div_confidence = document.getElementById('confidence_result');
          div_confidence.innerHTML = formattedConfidence;
        } else {
          // TODO: build a response
          console.error(req.statusText);
        }
      }
    };
  
    req.onerror = function (e) {
      // TODO: build a response
      console.error(req.statusText);
    };
  
    req.send(code);
  };
  
  function tokenize() {
    var htmlColors = {
      keyword: 'orchid',
      number: 'blue',
      string: 'salmon',
      variable: 'steelblue',
      symbol: 'purple',
      operator: 'green',
      group: 'green',
      format: 'orange',
      regex: 'orange',
      comment: 'yellowgreen',
      remark: 'yellowgreen',
      picture: 'hotpink',
      invalid: 'red',
      directive: 'lightgreen',
      preprocessor: 'lightgreen',
      attribute: 'lightgreen',
      'line number': 'skyblue',
      'line identification': 'royalblue',
      'line continuation': 'goldenrod',
      'statement terminator': 'goldenrod',
      'statement separator': 'goldenrod',
      'doc marker': 'orchid',
      'here doc': 'hotpink'
    };
  
    var results = '';
    var code = document.getElementById('code').value;
    var language = document.getElementById('language').value;
    // change 'C++' because the '+' character causes problems for requests
    // TODO: escape the request to allow '+'
    if (language == 'C++') {
      language = 'Cpp';
    }
    if (language == 'C#') {
      language = 'Csharp';
    }
  
    var query = "/tokens?language=" + language;
    var req = new XMLHttpRequest();
    req.open('POST', query, true);
    req.setRequestHeader("Content-type", "text/plain");
    req.onload = function (e) {
      if (req.readyState === 4) {
        if (req.status === 200) {
          var results = req.responseText;
          var tokens = JSON.parse(results);
          var length = tokens.length;
  
          var counts = {};
  
          for (var i = 0; i < length; i++) {
            var token = tokens[i];
            var ctype = token['type'];
            if (ctype in counts) {
              counts[ctype] += 1;
            } else {
              counts[ctype] = 1;
            }
          }
  
          var formattedDetails = formatCounts(counts);
  
          var div_details = document.getElementById('tokens_details');
          div_details.innerHTML = formattedDetails;
  
          var formattedResults = '<code><pre>';
  
          for (var i = 0; i < length; i++) {
            var token = tokens[i];
            formattedResults += colorize(token, htmlColors);
          }
  
          formattedResults += '</pre></code>';
  
          var div_tokens = document.getElementById('tokens_result');
          div_tokens.innerHTML = formattedResults;
        } else {
          // TODO: build a response
          console.error(req.statusText);
        }
      }
    };
  
    req.onerror = function (e) {
      // TODO: build a response
      console.error(req.statusText);
    };
  
    req.send(code);
  }
  