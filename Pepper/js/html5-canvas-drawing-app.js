// Copyright 2010 William Malone (www.williammalone.com)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/*jslint browser: true */
/*global G_vmlCanvasManager */

//<script src="/libs/qimessaging/2/qimessaging.js"></script>#

var session,
		outlineImage = new Image()

try {
	QiSession( function (s) {

		session = s;
		// now that we are connected, we can use the buttons on the page

		s.service('ALMemory').then(function (memory) {

			//memory.raiseEvent('NextPhrase', 0);
			memory.raiseEvent('NextPhrase', -100);
			memory.raiseEvent('ImageURL', 'None');

			// memory.subscriber('Something').then(function (subscriber) {
			// 	alert("Something");
			// });
		});

	}, function () {
        alert("disconnected");
    });
} catch (err) {
	//alert("error" + err.message);
	console.log("Error when initializing QiSession: " + err.message);
	console.log("Make sure you load this page from the robots server.")
};


$(function () {
	$('#next').click(nextPhrase);
});


function download() {
    var download = document.getElementById("download");
    var image = document.getElementById("canvasDrawing").toDataURL("image/png")
        .replace("image/png", "image/octet-stream");
    console.log(document.getElementById("canvasDrawing").toDataURL("image/png"))
    download.setAttribute("href", image);
    download.setAttribute("download","canvas.png");
    console.log("download");

    try{
        session.service('ALMemory').then(function (memory) {
            memory.raiseEvent('ImageURL', image);
        });
    } catch (err) {
	console.log("Error when initializing QiSession: " + err.message);
	console.log("Make sure you load this page from the robots server.")
	};
    //alert();
    //location.href='feedback.html';
}

function nextPhrase() {
	//browser.downloads.showDefaultFolder();
	//outlineImage.src = "images/watermelon-duck-outline.png";
	console.log('next');
	var can = document.getElementById('canvasDrawing');

	console.log('can');
	var im = can.toDataURL("image/png", 0.5);
	console.log(im);


	try{
        session.service('ALMemory').then(function (memory) {
            memory.raiseEvent('NextPhrase', -200);
        });
    } catch (err) {
	console.log("Error when initializing QiSession: " + err.message);
	console.log("Make sure you load this page from the robots server.")
	};
	//alert("nextPhrase");
	location.href='feedback.html';
}



var drawingApp = (function () {


	try {
		QiSession( function (s) {

			session = s;
			// now that we are connected, we can use the buttons on the page

			s.service('ALMemory').then(function (memory) {

				//memory.raiseEvent('NextPhrase', 0);

				memory.getData('Image').then(function (TextData) {
					//alert(TextData);
					outlineImage.src = TextData;
					redraw();
				});
			});

		}, function () {
        alert("disconnected");
        });
	} catch (err) {
	    outlineImage.src = "images/map11.png";
	    console.log("got into changing the src to: images/map15.png")
		//alert("error" + err.message);
		console.log("Error when initializing QiSession: " + err.message);
		console.log("Make sure you load this page from the robots server.")
	};


	"use strict";

	var
		canvas,
		context,
		canvasWidth = 1200/1.4,
		canvasHeight = 800/1.4,
		//colorPurple = "#cb3594",
		colorGreen = "#659b41",
		//colorYellow = "#ffcf33",
		//colorBrown = "#986928",

		crayonImage = new Image(),
		markerImage = new Image(),
		eraserImage = new Image(),
		crayonBackgroundImage = new Image(),
		markerBackgroundImage = new Image(),
		eraserBackgroundImage = new Image(),
		crayonTextureImage = new Image(),
		clickX = [],
		clickY = [],
		clickColor = [],
		clickTool = [],
		clickSize = [],
		clickDrag = [],
		paint = false,
		curColor = colorGreen,
		curTool = "crayon",
		curSize = "normal",
		mediumStartX = 18,
		mediumStartY = 19,
		mediumImageWidth = 93,
		mediumImageHeight = 46,
		drawingAreaX = 0,
		drawingAreaY = 0,
		drawingAreaWidth = canvasWidth,
		drawingAreaHeight = canvasHeight,
		toolHotspotStartY = 23,
		toolHotspotHeight = 38,
		sizeHotspotStartY = 157,
		sizeHotspotHeight = 36,
		totalLoadResources = 8,
		curLoadResNum = 0,
		sizeHotspotWidthObject = {
			huge: 39,
			large: 25,
			normal: 18,
			small: 16
		},



		// Clears the canvas.
		clearCanvas = function () {

			context.clearRect(0, 0, canvasWidth, canvasHeight);
		},

		// Redraws the canvas.
		redraw = function () {

			var locX,
				locY,
				radius,
				i,
				selected,

				drawCrayon = function (x, y, color, selected) {


				};

			// Make sure required resources are loaded before redrawing
			if (curLoadResNum < totalLoadResources) {
				return;
			}

			clearCanvas();




			locY = 189;
			context.beginPath();
			context.rect(locX, locY, 2, 12);
			context.closePath();
			context.fillStyle = '#333333';
			context.fill();

			// Keep the drawing in the drawing area
			context.save();
			context.beginPath();
			context.rect(drawingAreaX, drawingAreaY, drawingAreaWidth, drawingAreaHeight);
			context.clip();

			// For each point drawn
			for (i = 0; i < clickX.length; i += 1) {



				// Set the drawing path
				context.beginPath();
				// If dragging then draw a line between the two points
				if (clickDrag[i] && i) {
					context.moveTo(clickX[i - 1], clickY[i - 1]);
				} else {
					// The x position is moved over one pixel so a circle even if not dragging
					context.moveTo(clickX[i] - 1, clickY[i]);
				}
				context.lineTo(clickX[i], clickY[i]);

				// Set the drawing color
				context.strokeStyle = clickColor[i];

				context.lineCap = "round";
				context.lineJoin = "round";
				context.lineWidth = 5;
				context.stroke();
			}
			context.closePath();
			//context.globalCompositeOperation = "source-over";// To erase instead of draw over with white
			context.restore();



			context.globalAlpha = 1; // No IE support

			// Draw the outline image
			//alert(outlineImage.src);
			context.drawImage(outlineImage, drawingAreaX, drawingAreaY, drawingAreaWidth, drawingAreaHeight);
			//var can = document.getElementById('canvasDrawing');
            //if(can!=null){
                //console.log('can');
             //   var im = can.toDataURL("image/png");
               // console.log(im)
            //}


		},

		// Adds a point to the drawing array.
		// @param x
		// @param y
		// @param dragging
		addClick = function (x, y, dragging) {

			clickX.push(x);
			clickY.push(y);
			clickTool.push(curTool);
			clickColor.push(curColor);
			clickSize.push(curSize);
			clickDrag.push(dragging);
		},

		// Add mouse and touch event listeners to the canvas
		createUserEvents = function () {

			var press = function (e) {
				// Mouse down location
				var sizeHotspotStartX,
					mouseX = (e.changedTouches ? e.changedTouches[0].pageX : e.pageX) - this.offsetLeft,
mouseY = (e.changedTouches ? e.changedTouches[0].pageY : e.pageY) - this.offsetTop;


				paint = true;
				addClick(mouseX, mouseY, false);
				redraw();
			},

			drag = function (e) {

				var mouseX = (e.changedTouches ? e.changedTouches[0].pageX : e.pageX) - this.offsetLeft,
					mouseY = (e.changedTouches ? e.changedTouches[0].pageY : e.pageY) - this.offsetTop;

				if (paint) {
					addClick(mouseX, mouseY, true);
					redraw();
				}
				// Prevent the whole page from dragging if on mobile
				e.preventDefault();
			},

			release = function () {
				paint = false;
				redraw();
			},

			cancel = function () {
				paint = false;
			};
			canvas.id = 'canvasDrawing';
			// Add mouse event listeners to canvas element
			canvas.addEventListener("mousedown", press, false);
			canvas.addEventListener("mousemove", drag, false);
			canvas.addEventListener("mouseup", release);
			canvas.addEventListener("mouseout", cancel, false);

			// Add touch event listeners to canvas element
			canvas.addEventListener("touchstart", press, false);
			canvas.addEventListener("touchmove", drag, false);
			canvas.addEventListener("touchend", release, false);
			canvas.addEventListener("touchcancel", cancel, false);
		},

		// Calls the redraw function after all neccessary resources are loaded.
		resourceLoaded = function () {

			curLoadResNum += 1;
			if (curLoadResNum === totalLoadResources) {
				redraw();
				createUserEvents();
			}
		},

		// Creates a canvas element, loads images, adds events, and draws the canvas for the first time.
		init = function () {





			// Create the canvas (Neccessary for IE because it doesn't know what a canvas element is)
			canvas = document.createElement('canvas');

			canvas.setAttribute('width', canvasWidth);
			canvas.setAttribute('height', canvasHeight);
			canvas.setAttribute('id', 'canvas');
			document.getElementById('canvasDiv').appendChild(canvas);
			if (typeof G_vmlCanvasManager !== "undefined") {
				canvas = G_vmlCanvasManager.initElement(canvas);
			}
			context = canvas.getContext("2d"); // Grab the 2d canvas context
			// Note: The above code is a workaround for IE 8 and lower. Otherwise we could have used:
			//     context = document.getElementById('canvas').getContext("2d");

			// Load images
			crayonImage.onload = resourceLoaded;
			crayonImage.src = "images/crayon-outline.png";

			markerImage.onload = resourceLoaded;
			markerImage.src = "images/marker-outline.png";

			eraserImage.onload = resourceLoaded;
			eraserImage.src = "images/eraser-outline.png";

			crayonBackgroundImage.onload = resourceLoaded;
			crayonBackgroundImage.src = "images/crayon-background.png";

			markerBackgroundImage.onload = resourceLoaded;
			markerBackgroundImage.src = "images/marker-background.png";

			eraserBackgroundImage.onload = resourceLoaded;
			eraserBackgroundImage.src = "images/eraser-background.png";

			crayonTextureImage.onload = resourceLoaded;
			crayonTextureImage.src = "images/crayon-texture.png";

			//outlineImage.crossOrigin = "anonymous";
			outlineImage.onload = resourceLoaded;
			//outlineImage.src = "images/watermelon-duck-outline.png";
			//outlineImage.src = "images/map15.png";

		};

	return {
		init: init
	};
}());
