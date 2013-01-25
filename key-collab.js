var best = {score: -1};
var name = null;

function report(key) {
    $.post('report', JSON.stringify({
        key: key,
        name: name
    }));
}

$(document).ready(function() {
    var worker = new Worker("worker.js");
    $.get('words', function(words) {
        worker.postMessage(words);
    }, 'text');
    worker.addEventListener('message', function(event) {
        var result = JSON.parse(event.data);
        if (result.score > best.score) {
            best = result;
            $('#personal-best-key').text(result.key);
            $('#personal-best-score').text(result.score);
            report(best.key);
        }
    });
});
