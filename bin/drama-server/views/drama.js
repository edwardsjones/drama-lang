$(function () {
    var server = "http://localhost:5000";
    var ticket;
    var all_states = [];
    var done = false;

    // RegEx's used to validate inputs when editing messages.
    var valid_string_re = /"[a-zA-Z0-9,.!£$%^&*()\-_?><@~ ]*"/;
    var valid_list_re = /("[a-zA-Z0-9,.!£$%^&*()\-_?><@~ ]*?"|[^",\s]+)(?=\s*,|\s*$)/g;
    
    $(".start").on("click", function () {
        var progStr = $(".program").val();
        $.post(server + "/programs", progStr, function (data) {
            $(".step").attr("disabled", false);
            $(".back").attr("disabled", true);
            reset_message_editor();
            ticket = data;
            all_states = [data.state];
            pretty_print(ticket.state);
        });
    });

    $(".step").on("click", function () {
        if (ticket.state) {
            reset_message_editor(); 
            $.post(server + "/step", JSON.stringify(ticket), function (data) {
                if (data === "done") {
                    $(".step").attr("disabled", true);
                    $(".back").attr("disabled", true);
                } else {
                    all_states.push(data.state);
                    ticket.state = data.state;
                    ticket.ready = data.ready;
                    pretty_print(ticket.state);
                    $(".back").attr("disabled", false);
                }
            });
        }
    });

    $(".back").on("click", function () {
        reset_message_editor();
        if (all_states.length <= 2) {
            $(".back").attr("disabled", true);
        } else {
            $(".step").attr("disabled", false);
            all_states.pop();
            ticket.state = all_states[all_states.length - 1];
            $.post(server + "/back", JSON.stringify(ticket), function (data) {
                ticket.state = data.state;
                ticket.ready = data.ready;
                pretty_print(ticket.state);
            });
        }
    });

    // Changes the isCurrentAID when an actor is selected in the dropdown;
    // this allows the user to control which actors execute, and when.
    $("#display-heading").on("click", "li a", function (event) {
        var selected_string = event.target.outerText;
        var split_string = selected_string.split(" ");
        populate_display(ticket.state["_isGlobalEnv"]["_geActorInstances"], parseInt(split_string[1]));
        ticket.state["_isCurrentAID"] = parseInt(split_string[1]);
    });

    // Make tabs function as tabs instead of moving elements on the page. 
    $(document).delegate('#prog-textarea', 'keydown', function(e) {
        var key_code = e.keyCode || e.which;

        if (key_code == 9) {
            e.preventDefault();
            var start = $(this).get(0).selectionStart;
            var end = $(this).get(0).selectionEnd;

            // Set textarea value to: text before caret + four spaces + text after caret
            $(this).val($(this).val().substring(0, start) + "    " + $(this).val().substring(end));

            // Put caret at right position again
            $(this).get(0).selectionStart =
            $(this).get(0).selectionEnd = start + 4;
        }
    });

    var pretty_print = function (state) {

        var id_info_string = "<div class=\"col-md-6\">Executing Actor ID: " + state["_isCurrentAID"] + "</div>";
        id_info_string = id_info_string + "<div class=\"col-md-6\">Next Available Actor ID: " + state["_isGlobalEnv"]["_geNextAvailableActor"] + "</div>";


        var dropdown_string = "<div id=\"choose-actor\" class=\"dropdown\"> <button class=\"btn btn-default dropdown-toggle\" type=\"button\" id=\"dropdown-actor-menu\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"true\"> Choose actor instance... <span class=\"caret\"></span> </button> <ul class=\"dropdown-menu\" aria-labelledby=\"actor-dropdown\">" + populate_dropdown(state["_isGlobalEnv"]["_geActorInstances"]) + "</ul> <span title=\"The actor displayed here is the one that will execute next, assuming it is ready (ready actors are shown in green).\" class=\"glyphicon glyphicon-info-sign\" aria-hidden=\"true\"></span>";

        $("#actor-id-info").html(id_info_string);
        $("#display-heading").html(dropdown_string);
        $(".glyphicon").tooltip({ placement: "left" });
        
        populate_display(state["_isGlobalEnv"]["_geActorInstances"], state["_isCurrentAID"]);
    };


    var populate_display = function (actors, aid) {

        if (aid === 0) {
            $("#actor-info-id").html("Actor ID: " + aid);
            $("#actor-info-beh").html("Behaviour: undefined");
            $("#actor-info-cr").html("Can Receive: undefined");
            reset_table();
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
            if (ticket.ready.indexOf(actor_instance["_aiId"]) === -1) {
                dropdown_string = dropdown_string + "<li><a href=\"#\">Actor " + actor_instance["_aiId"] + " (" + actor_instance["_aiBehaviour"]["behaviourName"] + ") </a></li>"; 
            } else {
                dropdown_string = dropdown_string + "<li class=\"bg-success\"><a href=\"#\">Actor " + actor_instance["_aiId"] + " (" + actor_instance["_aiBehaviour"]["behaviourName"] + ") </a></li>"; 
            }
        });

        return dropdown_string;
    };

    var get_table = function (actor) {

        var inbox = actor["_aiInbox"];
        var lenv = actor["_aiEnv"]["_leBindings"];

        $("#actor-inbox-table").html(get_inbox_table(inbox));
        $("#actor-bindings-table").html(get_bindings_table(lenv));

        // Change values in messages in table
        $("#actor-inbox-table > table > tbody > tr").on("click", function (event) {

            // Retrieve the message from the actor by getting the row they 
            // clicked on. (Indexing in state object starts at 0, hence
            // the "msg_no - 1" access).
            var msg_no = $(this).closest("tr").find("th").html();
            var actor_id = actor["_aiId"];
            var msg = actor["_aiInbox"][msg_no - 1];

            var editor_string = "";

            editor_string = editor_string + get_message_tuples(msg); 

            $("#editor-heading").html("Message Editor: Actor ID " + actor_id + ", msg " + msg_no + "<button type=\"button\" class=\"btn btn-default btn-small pull-right save\">Save</button>");
            $("#editor-body").html(editor_string);

            $(".save").click(function () {
 
                // Iterate over each tuple in the message.
                for (var n = 0; n < msg.length; n++) {

                    // Build the relevant html ID's from the loop counter.
                    var input_id = "#input" + n;
                    var type_id = "#type" + n;
                    var input_val;
                    var valid = false;

                    // Get the type selected by the user.
                    var selected_type = $(type_id).text().trim();

                    // Validate their inputs according to the selected type.
                    // Empty inputs are ignored.
                    if (selected_type === "UnitV") {
                        input_val = [];
                        valid = true;
                    } else if (selected_type === "BoolV") {
                        var selected_option = $("input[type=\"radio\"][name=\"bool"+n+"\"]:checked");
                        if (is_valid(selected_type, selected_option)) {
                            if (selected_option.val() === "True") {
                                input_val = true;
                            } else {
                                input_val = false;
                            }
                            valid = true;
                        }
                    } else if (selected_type === "StringV") {
                        input_val = $(input_id).val();
                        if (is_valid(selected_type, input_val)) {
                            input_val = input_val.match(valid_string_re)[0];
                            input_val = input_val.slice(1, -1);
                            valid = true;
                        }
                    } else if (selected_type === "NumberV" || selected_type === "ActorV") {
                        input_val = $(input_id).val();
                        if (is_valid(selected_type, input_val)) {
                            input_val = parseInt(input_val.match(/[0-9]+/)[0]);
                            valid = true;
                        }
                    } else if (selected_type === "ListV") {
                        var matched_list = $(input_id).val().match(valid_list_re);
                        if (matched_list) {
                            input_val = matched_list;
                            valid = true;
                        }
                    }

                    if (valid) {

                        // Separate updating for lists, as their is no dropdown
                        // for each element to give the type.
                        if (selected_type === "ListV") {
                            var new_list = new Object();
                            new_list["tag"] = "ListV";
                            new_list["contents"] = [];
                            for (var p = 0; p < input_val.length; p++) {
                                new_list["contents"].push(get_list_element(input_val, p));
                            }
                            ticket.state["_isGlobalEnv"]["_geActorInstances"][actor_id]["_aiInbox"][msg_no - 1][n] = new_list;
                        } else {
                            ticket.state["_isGlobalEnv"]["_geActorInstances"][actor_id]["_aiInbox"][msg_no - 1][n]["tag"] = selected_type;
                            ticket.state["_isGlobalEnv"]["_geActorInstances"][actor_id]["_aiInbox"][msg_no - 1][n]["contents"] = input_val;
                        }
                    }
                }

                // Update the actor display with new values
                get_table(actor);
            });

            // Changes selected text in dropdown, and corresponding input field
            $("#msg-editor-type li a").click(function(){
                var selected_text = $(this).text();
                $(this).parents(".btn-group").find(".dropdown-toggle").html(selected_text+"<span class=\"caret\"></span>");

                var tuple_no = $(this).parents(".btn-group").find(".dropdown-toggle").attr("id").charAt(4);
                var element_id = "input" + tuple_no;
    
                update_editor_inputs(selected_text, element_id, tuple_no);
            });

        });

    };

    // Validates message inputs, and returns false on empty inputs. 
    var is_valid = function (type, input) {
        if (type === "UnitV") {
            return true;
        } else if (type === "BoolV") {
            if (input.length > 0) {
                return true;
            } else {
                return false;
            }
        } else if (type === "StringV") {
            var matched_string = input.match(valid_string_re);
            if (matched_string) {
                return true;
            } else {
                return false;
            }
        } else if (type === "NumberV" || type === "ActorV") {
            var matched_string = input.match(/[0-9]+/);
            if (matched_string) {
                return true;
            } else {
                return false;
            }
        }
        return false;
    };

    // Builds the html to display the message editor inputs.
    var get_message_tuples = function (msg) {
        var string = "";
        for (var l = msg.length - 1; l >= 0; l--) {
            string = string + "<div class=\"row\">";
            string = string + get_type_options(msg, l);
            string = string + get_tuple_contents(msg, l);
            string = string + "</div>";
        }
        return string;
    };

    // Builds message editor type dropdowns.
    var get_type_options = function (msg, tuple) {
        options = "<div class=\"col-md-4 dropup\"> <div id=\"msg-editor-type\" class=\"btn-group\"> <a id=\"type"+tuple+"\" class=\"type-option btn dropdown-toggle\" data-toggle=\"dropdown\" href=\"#\">" + msg[tuple]["tag"] + "<span class=\"caret\"></span> </a> <ul class=\"dropdown-menu\"> <li><a href=\"#\">BoolV</a></li> <li><a href=\"#\">NumberV</a></li> <li><a href=\"#\">ActorV</a></li> <li><a href=\"#\">StringV</a></li> <li><a href=\"#\">UnitV</a></li> <li><a href=\"#\">ListV</a></li> </ul> </div> </div>";
        return options;
    };

    // Builds the relevant input type for the tuple, depending on the 
    // type selected in the dropdown.
    var get_tuple_contents = function (msg, tuple) {
        var contents = "<div class=\"col-md-8\">";
        if (msg[tuple]["tag"] === "UnitV") {
            contents = contents + "<div id=\"input"+tuple+"\">()</div>";
        } else if (msg[tuple]["tag"] === "BoolV") {
            //radio buttons; true or false
            contents = contents + "<div id=\"input"+tuple+"\"><input type=\"radio\" id=\"true\" name=\"bool"+tuple+"\" value=\"True\"><label for=\"true\"> True </label><input type=\"radio\" id=\"false\" name=\"bool"+tuple+"\" value=\"False\"><label for=\"false\"> False </label></div>";
        } else {
            contents = contents + "<textarea id=\"input"+tuple+"\" cols=\"5\" rows=\"1\"></textarea>";
        }
        contents = contents + "</div>";
        return contents;
    };

    // Resets the table in the actor display.
    var reset_table = function () {

        var inbox_table = "Inbox <table class=\"table table-hover table-condensed\"> <thead> <tr> <th>#</th> <th>Value</th> </tr> </thead> <tbody> </tbody> </table>";
        var bindings_table = "Bindings <table class=\"table table-hover table-condensed\"> <thead> <tr> <th> Name </th> <th> Value </th> </tr> </thead> <tbody> </tbody> </table>";

        $("#actor-inbox-table").html(inbox_table);
        $("#actor-bindings-table").html(bindings_table);

    };

    // Resets message editor panel.
    var reset_message_editor = function () {
        $("#editor-heading").html("Message Editor");
        $("#editor-body").html("Select a message to edit its contents here.");
    }

    // Function used to create the ListV object to insert into the state upon
    // pressing "Save".
    var get_list_element = function (input_val, p) {
        var new_element = new Object();
        if (input_val[p].charAt(0) === input_val[p].charAt(input_val[p].length - 1) && input_val[p].charAt(0) === '"') {
            new_element["tag"] = "StringV";
            new_element["contents"] = input_val[p].slice(1, -1);
        } else if (input_val[p] === "true" || input_val[p] === "false") {
            new_element["tag"] = "BoolV";
            if (input_val[p] === "true") {
                new_element["contents"] = true;
            } else {
                new_element["contents"] = false;
            } 
        } else if (input_val[p] === "()") {
            new_element["tag"] = "UnitV";
            new_element["contents"] = [];
        } else if (input_val[p].match(/[0-9]+/)) {
            new_element["tag"] = "NumberV";
            new_element["contents"] = parseInt(input_val[p]);
        }
        return new_element;
    }

    // Builds the html to display an actors messages.
    var get_inbox_table = function (inbox) {
        var inbox_table = "Inbox <table class=\"table table-hover table-condensed\"> <thead> <tr> <th>#</th> <th>Value</th> </tr> </thead> <tbody>";

        if (inbox.length !== 0) {

            for (var i = 0; i < inbox.length; i++) {

                inbox_table = inbox_table + "<tr> <th>" + (i+1) + "</th> <td>";

                // Iterate through the messages backwards; due to how the program is parsed, 
                // values that are lists in the Haskell interpreter are reversed. 
                for (var j = inbox[i].length - 1; j >= 0; j--) {

                    // Need to make sure each list element is displayed as well.
                    if (inbox[i][j]["tag"] === "ListV") {
                        inbox_table = inbox_table + inbox[i][j]["tag"] + " [";
                        for (var k = 0; k < inbox[i][j]["contents"].length; k++) {

                            // Put speech marks around it if StringV
                            if (inbox[i][j]["contents"][k]["tag"] === "StringV") {
                                inbox_table = inbox_table + "\"" + inbox[i][j]["contents"][k]["contents"] + "\"";
                            } else if (inbox[i][j]["contents"][k]["tag"] === "UnitV") {
                                inbox_table = inbox_table + "()";
                            } else if (inbox[i][j]["contents"][k]["tag"] === "ListV") {
                                inbox_table = inbox_table + "ListV";
                            } else {
                                inbox_table = inbox_table + inbox[i][j]["contents"][k]["contents"];
                            }

                            // Put a comma between list elements
                            if (k !== (inbox[i][j]["contents"].length - 1)) {
                                inbox_table = inbox_table + ",";
                            }
                        }
                        inbox_table = inbox_table + "]";
                    } else {
                        inbox_table = inbox_table + inbox[i][j]["tag"] + " ";

                        if (inbox[i][j]["tag"] === "StringV") {
                            inbox_table = inbox_table + "\"" + inbox[i][j]["contents"] + "\"";
                        } else if (inbox[i][j]["tag"] === "UnitV") {
                            inbox_table = inbox_table + "()";
                        } else {
                            inbox_table = inbox_table + inbox[i][j]["contents"];
                        }
                            
                    }
                    if (j > 0) {
                        inbox_table = inbox_table + ", ";
                    }
                }
                inbox_table = inbox_table + "</td> </tr>";
            }
        }
        inbox_table = inbox_table + "</tbody> </table>";
        return inbox_table;
    }

    // Function that builds the html string displaying the table of bindings
    // that an actor has. 
    var get_bindings_table = function (lenv) {
        var bindings_table = "Bindings <table class=\"table table-hover table-condensed\"> <thead> <tr> <th> Name </th> <th> Value </th> </tr> </thead> <tbody>";

        Object.keys(lenv).forEach(function (key, index) {
            bindings_table = bindings_table + "<tr> <td>" + key + "</td> <td>" + lenv[key]["tag"] + " " + lenv[key]["contents"] + "</td> </tr>";
        });

        bindings_table = bindings_table + "</tbody> </table>";
        
        return bindings_table;
    }

    // Function that replaces the html string to display the inputs for 
    // each possible type of value in the message editor.
    var update_editor_inputs = function (selected_text, element_id, tuple_no) {
        if (selected_text === "UnitV") {
            $("#" + element_id).replaceWith("<div id=\""+element_id+"\">()</div>");
        } else if (selected_text === "BoolV") {
            $("#" + element_id).replaceWith("<div id=\""+element_id+"\"><input type=\"radio\" id=\"true\" name=\"bool"+tuple_no+"\" value=\"True\"><label for=\"true\"> True </label><input type=\"radio\" id=\"false\" name=\"bool"+tuple_no+"\" value=\"False\"><label for=\"false\"> False </label></div>");
        } else {
            $("#" + element_id).replaceWith("<textarea id=\""+element_id+"\" cols=\"5\" rows=\"1\"></textarea>");
        }
    }
});
