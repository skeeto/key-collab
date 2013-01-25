"use strict";

var alphabet = "abcdefghijklmnopqrstuvwxyz";

function fisherYates(array) {
    var j, tempi, tempj;
    for (var i = 0; i < array.length; i++) {
        j = Math.floor(Math.random() * (i + 1));
        tempi = array[i];
        tempj = array[j];
        array[i] = tempj;
        array[j] = tempi;
    }
    return array;
}

function genkey() {
    return fisherYates(alphabet.split('')).join('');
}

function encode(key, word) {
    return word.replace(/./g, function(c) {
        return key[alphabet.indexOf(c)];
    });
}

function sorted(word) {
    for (var i = 1; i < word.length; i++) {
        if (word[i - 1].localeCompare(word[i]) > 0) return false;
    }
    return true;
}

function encodeWith(key) {
    return function(word) {
        return encode(key, word);
    };
};

function run(words) {
    var threshold = 40;
    var counter = 0;
    while (true) {
        counter++;
        var key = genkey();
        var score = words.map(encodeWith(key)).filter(sorted).length;
        if (score > threshold) {
            self.postMessage('{' +
                             '"score":'   + score + ',' +
                             '"key":'     + '"' + key + '",' +
                             '"counter":' + counter + '}');
        }
    }
}

self.addEventListener('message', function(event) {
    run(eval(event.data));
});
