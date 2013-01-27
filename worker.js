//"use strict";

Math.randomN = function(n) {
    return Math.floor(Math.random() * n);
};

Array.prototype.swap = function(i, j) {
    var temp = this[i];
    this[i] = this[j];
    this[j] = temp;
};

Array.prototype.shuffle = function() {
    for (var i = 0; i < this.length; i++) {
        this.swap(i, Math.randomN(i + 1));
    }
    return this;
};

String.prototype.isSorted = function() {
    for (var i = 1; i < this.length; i++) {
        if (this[i - 1].localeCompare(this[i]) > 0) return false;
    }
    return true;
};

function Key(derive, n) {
    var source = derive === undefined ? Key.alphabet : derive.key;
    n = n === undefined ? source.length : n;

    var base = source.split('');
    for (var i = 0; i < n; i++) {
        base.swap(Math.randomN(base.length),
                  Math.randomN(base.length));
    }
    this.key = base.join('');
    this.count = Key.counter++;

    /* Create a quick-lookup table. */
    for (var j = 0; j < this.key.length; j++) {
        this[Key.alphabet[j]] = this.key[j];
    }
}

Key.counter = 0;
Key.alphabet = "abcdefghijklmnopqrstuvwxyz";

Key.fromString = function(string) {
    return new Key({key: string}, 0);
};

Key.prototype.toString = function() {
    return this.key;
};

Key.prototype.encode = function(word) {
    var output = new Array(word.length);
    for (var i = 0; i < word.length; i++) {
        output[i] = this[word[i]];
    }
    return output.join('');
};

Key.prototype.score = function() {
    if (!this._score) {
        this._score = 0;
        for (var i = 0; i < Key.words.length; i++) {
            if (this.encode(Key.words[i]).isSorted()) {
                this._score++;
            }
        }
    }
    return this._score;
};

function report(key) {
    self.postMessage(JSON.stringify({
        key: key.key,
        score: key.score(),
        count: key.count,
        counter: Key.counter
    }));
}

function run() {
    var key = new Key();
    var counter = 0;
    var restart = 256;
    while (true) {
        if (Key.counter % 40 === 0) report(key);
        counter++;
        var mutate = Math.ceil(Math.pow(counter / 325, 3));
        var next = new Key(key, mutate);
        if (next.score() > key.score() || mutate >= Key.alphabet.length) {
            key = next;
            counter = 0;
        }
    }
}

self.addEventListener('message', function(event) {
    Key.words = JSON.parse(event.data);
    run();
});
