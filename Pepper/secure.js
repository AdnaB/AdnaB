for (var i = 0; i < 1e7; i++) {
	    if ((new Date().getTime() - start) > 1000){


	    //try{
	        //QiSession( function (s) {
                //alert("sess");
                //alert('connected!');
              //  session = s
                console.log("in try2");
                session.service('ALMemory').then(function (memory) {
                    alert("Mem2");
                    alert(memory.getData('NextPhrase'));
                    memory.getData('NextPhrase').then(  function (Next) {
                        alert("in fi loop");
                        if( Next == 0) {
                           alert(newLoc);
                           location.href = "feedback"+newLoc+".html";
                           stop = 1;
                            };
                        });
                });
            //}, function () {
            //    alert("disconnected Next 2");
            //});
        //} catch (err) {
        //    alert("Error when initializing QiSession: " + err.message);
        //    console.log("Make sure you load this page from the robots server.");
        //};
        if(stop == 1){
            break;
        }
        if ((new Date().getTime() - start) > 1000000){
          alert("Next")
          location.href = "feedback"+newLoc+".html";
          break;
        }
        }
    }