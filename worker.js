"use strict";

importScripts('key.js');

/* Load data */
Key.words = (function() {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', 'words', false);
    xhr.send();
    if (xhr.status === 200) {
        return JSON.parse(xhr.responseText);
    } else {
        return [];
    }
}());

function report(key) {
    self.postMessage({
        key: key.key,
        score: key.score,
        count: key.count,
        counter: Key.counter
    });
}

(function () {
    var key = Key.generate();
    var counter = 0;
    while (true) {
        if (Key.counter % 157 === 0) report(key);
        counter++;
        var mutate = Math.ceil(Math.pow(counter / 325, 3));
        var next = key.derive(mutate);
        if (next.score > key.score) {
            key = next;
            counter = 0;
        } else if (mutate >= key.key.length) {
            key = Key.generate();
            counter = 0;
        }
    }
})();
