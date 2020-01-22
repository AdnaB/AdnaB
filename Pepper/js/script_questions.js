// keeping a pointer to the session is very useful!
var session;
try {
  QiSession( function (s) {
    //alert("sess");
    console.log('connected!');
    session = s;
    // now that we are connected, we can use the buttons on the page

    s.service('ALMemory').then(function (memory) {
      memory.raiseEvent('Feedback', 0);
      memory.raiseEvent('NextPhrase', -50);
    });
  });
} catch (err) {
  console.log("Error when initializing QiSession: " + err.message);
  console.log("Make sure you load this page from the robots server.")
};


function give_feedback (rating, page_number) {
    session.service('ALMemory').then(function (memory) {
         console.log("feedback");
         memory.insertData('Feedback', rating);
         memory.raiseEvent('NextPhrase', page_number);
     });

     document.getElementById("next").style.display="block";

};

function next(newLoc) {
	try{
	    QiSession( function (s) {
           // alert("sess");
            console.log('connected!');
            //session = s;
            console.log("in try1");
            session.service('ALMemory').then(function (memory) {
                console.log("Mem1");
                memory.insertData('NextPhrase', (newLoc-1));
             });

        }, function () {
            alert("disconnected Next1");

        });
     }
     catch (err) {
     alert("Error when initializing QiSession: " + err.message);
     console.log("Make sure you load this page from the robots server.");
     }


	location.href = "feedback"+newLoc+".html";

};

function done (){
    try{
	    QiSession( function (s) {
           // alert("sess");
            console.log('connected!');
            //session = s;
            console.log("in try1");
            session.service('ALMemory').then(function (memory) {
                console.log("Mem1");
                memory.insertData('NextPhrase', (3));
             });

        }, function () {
            alert("disconnected Next1");

        });
     }
     catch (err) {
     alert("Error when initializing QiSession: " + err.message);
     console.log("Make sure you load this page from the robots server.");
     }


}


