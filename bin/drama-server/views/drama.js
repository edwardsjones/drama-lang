$(function () {
    var server = "http://localhost:5000";
    var ticket;
    
    $(".start").on("click", function () {
        var progStr = $(".program").val();
        $.post(server + "/programs", progStr, function (data) {
            ticket = data;
            $(".output-contents").replaceWith( "<div class=\"output-contents\">" + JSON.stringify(data) + "</div>");
        });
    });
    $(".step").on("click", function () {
        if (ticket.state) {
            $.post(server + "/step", JSON.stringify(ticket), function (data) {
                ticket.state = data;
                $(".output-contents").replaceWith( "<div class=\"output-contents\">" + JSON.stringify(ticket.state) + "</div>");
            });
        } else {
            console.log("No state set.");
        }
    });
}); 
