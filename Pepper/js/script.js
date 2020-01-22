// keeping a pointer to the session is very useful!
var session;
alert();
try {
  QiSession( function (s) {
    alert("sess");
    console.log('connected!');
    session = s;
    // now that we are connected, we can use the buttons on the page

    s.service('ALMemory').then(function (memory) {
      memory.subscriber('TouchChanged').then(function (subscriber) {
        alert();
      });
      memory.subscriber('Something').then(function (subscriber) {
        subscriber.signal.connect(changeTitle);
      });
    });
  });
} catch (err) {
  console.log("Error when initializing QiSession: " + err.message);
  console.log("Make sure you load this page from the robots server.")
}


