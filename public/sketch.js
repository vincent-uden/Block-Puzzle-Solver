var testGrid = ["#TTT#", "#sXX#", "#LLL#", "#sTw#", "#sxX#", "#LxX#", "#sww#", "#vvw#", "#vxx#"];
var boxSize = 100;
var boxes;
var yRot = 0;
var xRot = 0;
var zRot = 0;

function Cube(x, y, z, color) {
    this.x = x;
    this.y = y;
    this.z = z;
    this.color = color;
}

function setup() {
    var cnv = createCanvas(600, 600, WEBGL);
    colorMode(HSB, 255);
    strokeWeight(5);
    boxes = parseGrid(testGrid);
    setFrameRate(30);
}

function draw() {
    if (keyIsDown(65)) {yRot += 0.05;}
    if (keyIsDown(68)) {yRot -= 0.05;}
    if (keyIsDown(83)) {xRot += 0.05;}
    if (keyIsDown(87)) {xRot -= 0.05;}
    if (keyIsDown(81)) {zRot += 0.05;}
    if (keyIsDown(69)) {zRot -= 0.05;}

    translate(0, 0, -150);
    background(51);
    ambientLight(0, 0, 200);
    rotateX(-QUARTER_PI);
    rotateY(QUARTER_PI);
    rotateX(xRot);
    rotateZ(yRot);
    rotateY(zRot);
    translate(-boxSize, -boxSize, -boxSize);
    for (cube of boxes) {
        push();
        // ambientMaterial(cube.color, 200, 200);
        fill(cube.color, 200, 200);
        stroke(cube.color, 255, 255);
        translate(cube.x * boxSize, cube.y * boxSize, cube.z * boxSize);
        box(boxSize, boxSize, boxSize);
        pop();
    }
}

function parseGrid(grid) {
    var cubes = [];
    var uniqueLetters = new Set();
    // Find all colors
    for (let i = 0; i < testGrid.length; i++) {
        for (let x = 1; x < 4; x++) {
            uniqueLetters.add(grid[i][x]);
        }
    }
    var colors = {};
    var h = 0;
    var keys = uniqueLetters.keys();
    var newKeys = [];
    for (let i = 0; i < uniqueLetters.size; i++) {
        newKeys.push(keys.next()["value"]);
    }
    for (let i = 0; i < newKeys.length; i++) {
        h = map(i, 0, newKeys.length, 0, 255);
        colors[newKeys[i]] = h;
    }
    for (let i = 0; i < grid.length; i++) {
        for (let n = 1; n < 4; n++) {
            cubes.push(new Cube(n - 1, i % 3, floor(i / 3.0), colors[grid[i][n]]));           
        }
    }
    return cubes;
}

function keyPressed() {
    if (keyCode === 32) {
        xRot = 0;
        yRot = 0;
        zRot = 0;
    }
}