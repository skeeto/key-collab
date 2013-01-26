var best = {score: -1};
var overall = {score: -1};
var name = localStorage.name || "anonymous";
var id = Math.floor(Math.random() * 0xffffff).toString(16);

/* Report KEY to the server. */
function report(key) {
    $.post('report', JSON.stringify({
        key: key,
        name: name
    }));
}

$(document).ready(function() {
    /* Fire off the web worker. */
    var start = new Date();
    var worker = new Worker("worker.js");
    $.get('words', function(words) {
        worker.postMessage(words);
    }, 'text');

    /* Get updates from the worker. */
    var lastCpuReport = 0;
    worker.addEventListener('message', function(event) {
        var result = JSON.parse(event.data);
        if (result.score > Math.max(overall.score, best.score)) {
            report(result.key);
        }
        if (result.score > best.score) {
            best = result;
            $('#personal-best-key').text(result.key);
            $('#personal-best-score').text(result.score);
        }
        var counter = result.counter;
        result.rate = (counter / ((Date.now() - start) / 1000)).toFixed(1);
        var msg = counter + ' keys tried (' + result.rate + ' / sec)';
        $('#personal-best-count').text(msg);

        /* Report CPU information to the server. */
        if (lastCpuReport < Date.now() - 10000) {
            var report = {
                id: id,
                rate: result.rate,
                counter: result.counter
            };
            $.post('cpu', JSON.stringify(report), function(global) {
                $('#global-rate').text(global.rate.toFixed(1) + ' keys / sec');
                $('#global-clients').text(global.clients + ' clients');
            }, 'json');
            lastCpuReport = Date.now();
        }
    });

    /* Manage the form. */
    $('form').bind('submit', function() {
        $('#name').blur();
        return false;
    });
    $('#name').bind('change', function() {
        name = $('#name').val();
        localStorage.name = name;
    });
    $('#name').val(name);

    /* Get solution updates from the server. */
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
