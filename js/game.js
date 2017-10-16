;(function (ns, undefined) {
    var EXPORTS = [
        'initGame'
    ];

    var SEED = 12345,
        TRIALS = 10000;

    var setup = {
        successChance: 30,
        successMean: 500,
        successRatio: 5,
        successCost: 200,
        chance1: 60,
        cost1: 50,
        chance2: 90,
        cost2: 50
    };

    function encodeParams(context)
    {
        return Object.keys(context).map(function (p) {
            return p + '=' + encodeURIComponent(context[p]);
        }).join('&');
    }

    function connect(context, mbr, input, label)
    {
        input.addEventListener('change', function () {
            context[mbr] = parseFloat(input.value);
            label.innerHTML = context[mbr].toFixed(2);
            // probably have a time delay here to batch multiple changes
            getMC(context, updateResults);
        })
    }

    function getMC(context, callback)
    {
        context.seed = context.seed || SEED;
        context.trials = context.trials || TRIALS;
        context.stages = 2;

        var xhr = new XMLHttpRequest();
        var target ='http://localhost:3000/gameOutcome?' +
          encodeParams(context);
        xhr.open('GET', target);
        xhr.onreadystatechange = function () {
            if (xhr.readyState == XMLHttpRequest.DONE)
                if (xhr.status == 200)
                    callback(JSON.parse(xhr.responseText));
                else
                    console.log("bad result: " + xhr.status);
        }
        xhr.send();
    }

    function mean(data)
    {
        var sum = 0.0;
        for (var i = 0; i < data.length; ++i)
            sum += data[i];
        return sum / data.length;
    }

    function probPredicate(trials, pred)
    {
        var pred_held = 0;
        for (var i = 0; i < trials.length; ++i)
            if (pred(trials[i]))
                ++pred_held;
        return pred_held / trials.length;
    }

    function updateResults(data)
    {
        document.getElementById('resultMean').innerHTML = mean(data).toFixed(2);
        document.getElementById('resultChanceLoss').innerHTML =
            (probPredicate(data, function (v) { return v < 0; }) * 100).toFixed(2);
        document.getElementById('resultChanceBigLoss').innerHTML =
            (probPredicate(data, function (v) { return v < -100.0; }) * 100).toFixed(2);
        document.getElementById('resultChanceBigGain').innerHTML =
            (probPredicate(data, function (v) { return v > 1000.0; }) * 100).toFixed(2);
        makeHistogram('resultHist', data);
    }

    function makeHistogram(svgId, data)
    {
        document.getElementById(svgId).innerHTML = '';

        var svg = d3.select('#' + svgId),
            margin = { top: 10, right: 30, bottom: 30, left: 30 },
            width = svg.attr('width') - margin.left - margin.right,
            height = svg.attr('height') - margin.top - margin.bottom,
            g = svg.append('g').attr('transform',
              'translate(' + margin.left + ',' + margin.top + ')');

        var x = d3.scaleLinear()
                  .domain(d3.extent(data))
                  .nice(20)
                  .rangeRound([0, width]);

        var bins = d3.histogram()
                     .domain(x.domain())
                     .thresholds(x.ticks(20))
                     (data);

        var y = d3.scaleLinear()
                  .domain([0, d3.max(bins, function (d) { return d.length; })])
                  .range([height, 0]);

        var bar = g.selectAll('bar')
                   .data(bins)
                   .enter().append('g')
                           .attr('class', function (d) {
                               if (d.x0 < 0)
                                   return 'bar bar-negative';
                               else
                                   return 'bar bar-positive';
                           })
                           .attr('transform', function (d) {
                               return 'translate(' + x(d.x0) + ',' +
                                   y(d.length) + ')';
                           });

        bar.append('rect')
           .attr('x', 1)  // by allah, you d3.js are dogs
           .attr('width', x(bins[1].x1) - x(bins[1].x0) - 1)
           .attr('height', function (d) {
               return height - y(d.length);
           });

        // something bout text here

        g.append('g')
         .attr('class', 'axis axis-x')
         .attr('transform', 'translate(0,' + height + ')')
         .call(d3.axisBottom(x));
    }

    function initInput(name)
    {
        var input = document.getElementById(name),
            label = document.getElementById(name + 'Label');
        connect(setup, name, input, label);
    }

    function initGame()
    {
        Object.keys(setup).forEach(initInput);
        getMC(setup, updateResults);
    }

    ns.initGame = initGame;
})(window)
