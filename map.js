// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), container="div", dependencies="usa.js"

const BLUE = "#67f";
const LIGHT_BLUE = "#79f";
const DARK_BLUE = "#56c";
const RED = "#f76";
const LIGHT_RED = "#f99";
const DARK_RED = "#c65";
const BLK = "#444";

const nums =   [1,   1,  1,  1,  1,  1,  1,  1, 1, 1, 1, 1, 1, 3, 1, 3, 2, 4, 1];
const denoms = [100, 50, 40, 20, 15, 12, 10, 9, 8, 7, 6, 5, 4, 10, 3, 7, 5, 9, 2];

function probToText(prob) {
    if (prob < 0.005)
        return "<1 in 100";
    else if (prob > 0.995)
        return ">99 in 100";

    let p = Math.min(prob, 1 - prob);
    let dists = nums.map((n, i) => Math.abs(n/denoms[i] - p));
    let idx = dists.indexOf(Math.min.apply(null, dists));
    if (p == prob)
        return `${nums[idx]} in ${denoms[idx]}`;
    else
        return `${denoms[idx] - nums[idx]} in ${denoms[idx]}`;
}

const w = div.node().getBoundingClientRect().width;
const h = w * 0.625;

let ids = us.objects.states.geometries.map(d => d.id);
window.idxs = {};
for (id of ids) {
    idxs[id] = data.findIndex(d => d.state == id);
}

div.attr("id", "map");
let chart = div.append("svg")
    .attr("width", w)
    .attr("height", h);

let projection = d3.geoAlbersUsa()
    .scale(1.35 * w)
    .translate([1.02*w/2, 0.98*h/2]);
let path = d3.geoPath().projection(projection);

let midpt = "#fafffa";
let color_dem = d3.scaleLinear()
    .domain([0.5, 1.0])
    .range([midpt, BLUE]);
let color_gop = d3.scaleLinear()
    .domain([0.0, 0.5])
    .range([RED, midpt]);
window.color = x => x <= 0.5 ? color_gop(x) : color_dem(x);

let st_feat = topojson.feature(us, us.objects.states).features;
let max_area = d3.max(st_feat, d => path.area(d));
let size_area = d3.scaleSqrt()
    .domain([0, max_area])
    .range([8, 30]);

let states = chart.append("g")
    .attr("class", "states")
    .selectAll("path")
    .data(st_feat)
    .enter().append("path")
    .attr("fill", d => color(data[idxs[d.id]].prob))
    .attr("d", path)
    .style("stroke", "#222")
    .style("stroke-width", "1px");

let states_hover = chart.append("g")
    .attr("class", "states")
    .selectAll("path")
    .data(st_feat)
    .enter().append("path")
    .attr("d", path)
    .style("fill", "none")
    .style("stroke", "black")
    .style("stroke-width", "0");


let labels = chart.append("g")
    .attr("class", "labels")
    .selectAll("text")
    .data(st_feat)
    .enter().append("text")
    .style("fill", "#00000088")
    .attr("x", d => path.centroid(d)[0]+0.5
        + (d.id == "Florida")*10
        + (d.id == "Michigan")*8
        + (d.id == "Louisiana")*(-8)
        + (d.id == "Maryland")*(-4))
    .attr("y", d => path.centroid(d)[1]+2
        + (d.id == "Michigan")*15
        + (d.id == "California")*10
        + (d.id == "Idaho")*5
        + (d.id == "Texas")*5
        + (d.id == "New Jersey")*(-6)
        + (d.id == "Maryland")*(-3))
    .attr("text-anchor","middle");

let tt = div.append("div")
    .attr("class", "tooltip")
    .style("visibility", "hidden");

let mout = function() {
    tt.style("visibility", "hidden");
    states_hover.style("stroke-width", "0");
};

chart.on("mouseout", mout);
chart.on("touchend", mout);

r2d3.onRender(function(data, div, width, height, options) {
    let map = div.select("svg");

    states.attr("fill", d => color(data[idxs[d.id]].prob));
    labels
        .style("font-size", d => {
            let base = size_area(path.area(d));
            return (base < 10 || d.id == "Hawaii") ? 0 : base;
        })
        .text(d => Math.abs(data[idxs[d.id]].prob - 0.5) > 0.495 ? "✓" : "");

    let mmv = function(d) {
        let [mx, my] = d3.mouse(chart.node());
        if (mx < 40) mx = 40;
        if (mx > w - 120) mx = w - 120;

        let state = d.id;
        let prob = data[idxs[state]].prob;
        let candidate = prob > 0.5 ? "Biden" : "Trump";
        let color = prob > 0.5 ? DARK_BLUE : DARK_RED;
        let cand_prob = probToText(prob > 0.5 ? prob : 1 - prob);
        let won_text = Math.abs(prob - 0.5) > 0.495 ? "✓" : "";

        d3.select(this).style("stroke-width", "3px");

        let txt = `<h3>${state} ${won_text}</h3>
        ${data[idxs[state]].ev} electoral votes
        <div style="width: 100%; height: 1.5em; margin: 4px 0; display: flex;">
        <div style="background: ${BLUE}; flex-basis: ${100*prob}%"></div>
        <div style="background: ${RED}; flex-basis: ${100 - 100*prob}%"></div>
        </div>
        <b style="color: ${color};">${candidate}</b> has a <b>${cand_prob}</b>
        chance of winning.
        `;

        tt.style("visibility", "visible")
            .html(txt)
            .style("left", (mx - 50) + "px")
            //.style("top", (my - 25) + "px");
            .style("bottom", (h - my + 25) + "px");
    };

    // clear old handlers
    states_hover.on("mousemove", null);
    states_hover.on("touchmove", null);
    // set new handler
    states_hover.on("mousemove", mmv);
    states_hover.on("touchmove", mmv);
});


