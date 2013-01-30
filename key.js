"use strict";

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

function Key(keystring) {
    this.key = keystring;
    this.count = Key.counter++;
    for (var j = 0; j < this.key.length; j++) {
        this[Key.ALPHABET[j]] = this.key[j];
    }
    this.score = 0;
    for (var i = 0; i < Key.words.length; i++) {
        if (this.encode(Key.words[i]).isSorted()) {
            this.score++;
        }
    }
}

Key.ALPHABET = "abcdefghijklmnopqrstuvwxyz";
Key.counter = 0;
Key.words = [];

Key.generate = function() {
    return new Key(Key.ALPHABET.split('').shuffle().join(''));
};

Key.prototype.derive = function(n) {
    var base = this.key.split('');
    for (var i = 0; i < n; i++) {
        base.swap(Math.randomN(base.length),
                  Math.randomN(base.length));
    }
    return new Key(base.join(''));
};

Key.prototype.toString = function() {
    return '[Key "' + this.key + '" ' + this.score + ']';
};

Key.prototype.encode = function(word) {
    var output = new Array(word.length);
    for (var i = 0; i < word.length; i++) {
        output[i] = this[word[i]];
    }
    return output.join('');
};
