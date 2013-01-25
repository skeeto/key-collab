var best = {score: -1};
var overall = {score: -1};
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

    function getUpdate() {
        $.getJSON('/best?score=' + overall.score, function(result) {
            overall.score = result[0];
            overall.key = result[1];
            overall.name = result[2] || "anonymous";
            $('#overall-best-score').text(overall.score);
            $('#overall-best-key').text(overall.key);
            $('#overall-best-name').text(overall.name);
            getUpdate();
        });
    }
    getUpdate();
});
