$(function () {
    var server = "http://localhost:5000";
    var ticket;
    
    $(".start").on("click", function () {
        var progStr = $(".program").val();
        $.post(server + "/programs", progStr, function (data) {
            ticket = data;
            pretty_print(ticket.state);
        });
    });

    $(".step").on("click", function () {
        if (ticket.state) {
            $.post(server + "/step", JSON.stringify(ticket), function (data) {
                ticket.state = data;
                console.log(ticket.state);
                pretty_print(ticket.state);
            });
        } else {
            console.log("No state set.");
        }
    });

    $("#display-heading").on("click", "li a", function (event) {
        var selected_string = event.target.outerText;
        var split_string = selected_string.split(" ");
        console.log(split_string[1]);
        populate_display(ticket.state["_isGlobalEnv"]["_geActorInstances"], parseInt(split_string[1]));
    });

    var pretty_print = function (state) {

        var id_info_string = "<div class=\"col-md-6\">Executing Actor ID: " + state["_isCurrentAID"] + "</div>";
        id_info_string = id_info_string + "<div class=\"col-md-6\">Next Available Actor ID: " + state["_isGlobalEnv"]["_geNextAvailableActor"] + "</div>";


        var dropdown_string = "<div id=\"choose-actor\" class=\"dropdown\"> <button class=\"btn btn-default dropdown-toggle\" type=\"button\" id=\"dropdown-actor-menu\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"true\"> Choose actor instance... <span class=\"caret\"></span> </button> <ul class=\"dropdown-menu\" aria-labelledby=\"actor-dropdown\">" + populate_dropdown(state["_isGlobalEnv"]["_geActorInstances"]) + "</ul>";

        $("#actor-id-info").html(id_info_string);
        $("#display-heading").html(dropdown_string);
        
        populate_display(state["_isGlobalEnv"]["_geActorInstances"], state["_isCurrentAID"]);
    };


    var populate_display = function (actors, aid) {
        if (aid === 0) {
            $("#actor-info-id").html("Actor ID: " + aid);
            $("#actor-info-beh").html("Behaviour: undefined");
            $("#actor-info-cr").html("Can Receive: undefined");
            return;
        }
        var actor;
        Object.keys(actors).forEach(function (key, index) {
            var actor_instance = actors[parseInt(key)];
            if (actors[parseInt(key)]["_aiId"] === aid) {
                actor = actors[parseInt(key)];
            } 
        });

        $("#actor-info-id").html("Actor ID: " + aid);
        $("#actor-info-beh").html("Behaviour: " + actor["_aiBehaviour"]["behaviourName"]);
        $("#actor-info-cr").html("Can Receive: " + actor["_aiCanReceive"]);

        get_table(actor);

    };

    var populate_dropdown = function (actors) {
        var dropdown_string = "";

        Object.keys(actors).forEach(function (key, index) {
            var actor_instance = actors[parseInt(key)];
            dropdown_string = dropdown_string + "<li><a href=\"#\">Actor " + actor_instance["_aiId"] + " (" + actor_instance["_aiBehaviour"]["behaviourName"] + ") </a></li>"; 
        });

        return dropdown_string;
    };

    var get_table = function (actor) {
        var inbox = actor["_aiInbox"];

        var inbox_table = "Inbox <table class=\"table table-hover table-condensed\"> <thead> <tr> <th>#</th> <th>Value</th> </tr> </thead> <tbody>";

        if (inbox.length !== 0) {
            for (var i = 0; i < inbox.length; i++) {
                inbox_table = inbox_table + "<tr> <th>" + (i+1) + "</th> <td>" + inbox[i][0]["tag"] + " " + inbox[i][0]["contents"] + "</td> </tr>";
            }
        }
        inbox_table = inbox_table + "</tbody> </table>";

        var bindings_table = "Bindings <table class=\"table table-hover table-condensed\"> <thead> <tr> <th> Name </th> <th> Value </th> </tr> </thead> <tbody>";

        var lenv = actor["_aiEnv"]["_leBindings"];
        Object.keys(lenv).forEach(function (key, index) {
            bindings_table = bindings_table + "<tr> <td>" + key + "</td> <td>" + lenv[key]["tag"] + " " + lenv[key]["contents"] + "</td> </tr>";
        });

        bindings_table = bindings_table + "</tbody> </table>";

        $("#actor-inbox-table").html(inbox_table);
        $("#actor-bindings-table").html(bindings_table);
        
    };
});
