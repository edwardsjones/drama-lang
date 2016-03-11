$(function () {
    var server = "http://localhost:5000";
    var ticket;
    var all_states = [];
    var done = false;
    var option = "Overview";

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
            init_display(option);
        });
    });

    $(".step").on("click", function () {
        if (ticket.state) {
            //reset_message_editor(); 
            $.post(server + "/step", JSON.stringify(ticket), function (data) {
                if (data === "done") {
                    $(".step").attr("disabled", true);
                    $(".back").attr("disabled", true);
                } else {
                    all_states.push(data.state);
                    ticket.state = data.state;
                    ticket.ready = data.ready;
                    update_display(option);
                    $(".back").attr("disabled", false);
                }
            });
        }
    });

    $(".back").on("click", function () {
        //reset_message_editor();
        if (all_states.length <= 2) {
            $(".back").attr("disabled", true);
        } else {
            $(".step").attr("disabled", false);
            all_states.pop();
            ticket.state = all_states[all_states.length - 1];
            $.post(server + "/back", JSON.stringify(ticket), function (data) {
                ticket.state = data.state;
                ticket.ready = data.ready;
                update_display(option);
            });
        }
    });

    // Changes which tab is selected upon click
    $(".singular-tab").on("click", function (event) {
        $(".singular-tab").removeClass("active");
        $(this).toggleClass("active");
        option = $(this)[0].innerText.trim();
        init_display(option);
        if (ticket) {
            update_display(option);
        }
    });

    var init_display = function (option) {
        if (option === "Overview") {
            overview_init();
        } else if (option === "Detailed Actor View") {
            detailed_init();
        } else if (option === "Examples") {
            example_init();
        }
    }

    var update_display = function (option) {
        if (option === "Overview") {
            get_overview();
        } else if (option === "Detailed Actor View") {
            get_detailed_view();
        }
    }

    var example_init = function () {
        var example_string = "<div id=\"output-content\"><div><p>Select an example from the dropdown below to load it's code.</p></div><div id=\"example-dropdown\"><div class=\"btn-group\"><button id=\"dropdown-menu\" class=\"btn btn-default btn-sm dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">Choose an example<span class=\"caret\"></span></button><ul class=\"dropdown-menu\" aria-labelledby=\"dropdown-menu\"><li><a href=\"#\">Send to Self</a></li><li><a href=\"#\">Acknowledge</a></li><li><a href=\"#\">Producer-Consumer</a></li><li role=\"separator\" class=\"divider\"></li><li><a href=\"#\">Needham-Schroeder</a></li><li><a href=\"#\">Needham-Schroeder (fixed)</a></li></ul></div></div></div>";
        
        $("#output-content").replaceWith(example_string);
        $("#example-dropdown li a").click(function(){
            var selected_text = $(this).text().trim();
            load_program(selected_text);
        });
    }

    var load_program = function (name) {
        if (name === "Send to Self") {
            $("#prog-textarea").val("behaviour sender () {\n  let me = self in\n    send me (1)\n  receive\n    (NumberV x) ->\n      ()\n  done\n}\n\ncreate sender ()");
        } else if (name === "Acknowledge") {
            $("#prog-textarea").val("behaviour chatterbox (mate) {\n  send mate (self)\n  receive\n    (NumberV x) ->\n      ()\n  done\n}\n\nbehaviour acknowledge () {\n  ()\n  receive\n    (ActorV adr) ->\n      send adr (1)\n  done\n}\n\nbehaviour starter () {\n  let ack = create acknowledge () in\n    create chatterbox (ack)\n  receive\n    () -> ()\n  done\n}\n\ncreate starter ()");
        } else if (name === "Producer-Consumer") {
            $("#prog-textarea").val("behaviour producer () {\n  ()\n  receive\n    (ActorV id, StringV msg) ->\n      if (msg == \"ready\") then {\n        send id (\"job\")\n      } else {\n        ()\n      }\n  done\n}\n\nbehaviour consumer (prod) {\n  send prod (self, \"ready\")\n  receive\n    (StringV job) ->\n      if (job == \"stop\") then {\n        ()\n      } else {\n        send prod (self, \"ready\")\n      }\n  done\n}\n\nbehaviour creater () {\n  let prod = create producer () in\n    create consumer (prod)\n  receive\n    () -> ()\n  done\n}\n\ncreate creater ()");
        } else if (name === "Needham-Schroeder") {
            $("#prog-textarea").val("behaviour alice (kpb ksa) {\n  // bob's address is actually impostor's address\n  // kpb is actually kpi\n  let bob_address = 4 in\n  let my_nonce = \"alice nonce\" in\n    send bob_address (encrypt my_nonce kpb, self)\n  receive\n    // response from impostor (bob's msg unchanged)\n    // both nonces encrypted with kpa\n    (EncryptedV na_enc, EncryptedV nb_enc) ->\n      let na = decrypt na_enc ksa in\n      let nb = decrypt nb_enc ksa in\n          send bob_address (encrypt nb kpb)\n  done\n}\n\nbehaviour impostor (bob alice kpb ksi) {\n  ()\n  receive\n    // relay msg from alice to bob (change encryption)\n    // impostor learns first nonce\n    (EncryptedV na_enc, ActorV alice_address) ->\n      let na = decrypt na_enc ksi in\n      send bob (encrypt na kpb, encrypt self kpb)\n\n    // response from bob\n    (EncryptedV na_enc, EncryptedV nb_enc) ->\n      send alice (na_enc, nb_enc)\n\n    // second msg from alice\n    // impostor learns second nonce\n    (EncryptedV nb_enc) ->\n      let nb = decrypt nb_enc ksi in\n        send bob (encrypt nb kpb)\n  done\n}\n\nbehaviour bob (kpa ksb) {\n  let my_nonce = \"bob nonce\" in\n    ()\n  receive\n    // message from impostor\n    // \"alice_address\" is impostor's address\n    (EncryptedV na_enc, EncryptedV alice_address) ->\n      let na = decrypt na_enc ksb in\n      let alice = decrypt alice_address ksb in\n        send alice (encrypt na kpa, encrypt my_nonce kpa)\n  done\n}\n\nbehaviour starter () {\n  let bob = create bob (\"alice key\" \"bob key\") in\n  let alice = create alice (\"impostor key\" \"alice key\") in\n  let impostor = create impostor (bob alice \"bob key\" \"impostor key\") in\n    ()\n  receive\n    () -> ()\n  done\n}\n\ncreate starter ()");
        } else if (name === "Needham-Schroeder (fixed)") {
            $("#prog-textarea").val("behaviour alice (kpb ksa) {\n  // bob's address is actually impostor's address\n  // kpb is actually kpi\n  let bob_address = 4 in\n  let my_nonce = \"alice nonce\" in\n    send bob_address (encrypt my_nonce kpb, self)\n  receive\n    // response from impostor (bob's msg unchanged)\n    // both nonces encrypted with kpa\n    (EncryptedV na_enc, EncryptedV nb_enc, EncryptedV bob_enc) ->\n      let na = decrypt na_enc ksa in\n      let nb = decrypt nb_enc ksa in\n      let bob = decrypt bob_enc ksa in\n        if (bob == bob_address) then {\n          send bob (encrypt nb kpb)\n        } else {\n          ()\n        }\n  done\n}\n\nbehaviour impostor (bob alice kpb ksi) {\n  ()\n  receive\n    // relay msg from alice to bob (change encryption)\n    // impostor learns first nonce\n    (EncryptedV na_enc, ActorV alice_address) ->\n      let na = decrypt na_enc ksi in\n        send bob (encrypt na kpb, encrypt self kpb)\n\n    // response from bob\n    (EncryptedV na_enc, EncryptedV nb_enc, EncryptedV bob_enc) ->\n      send alice (na_enc, nb_enc, bob_enc)\n\n    // second msg from alice\n    // impostor learns second nonce\n    (EncryptedV nb_enc) ->\n      let nb = decrypt nb_enc ksi in\n        send bob (encrypt nb kpb)\n  done\n}\n\nbehaviour bob (kpa ksb) {\n  let my_nonce = \"bob nonce\" in\n    ()\n  receive\n    // message from impostor\n    // \"alice_address\" is impostor's address\n    (EncryptedV na_enc, EncryptedV alice_address) ->\n      let na = decrypt na_enc ksb in\n      let alice = decrypt alice_address ksb in\n        send alice (encrypt na kpa, encrypt my_nonce kpa, encrypt self kpa)\n  done\n}\n\nbehaviour starter () {\n  let bob = create bob (\"alice key\" \"bob key\") in\n  let alice = create alice (\"impostor key\" \"alice key\") in\n  let impostor = create impostor (bob alice \"bob key\" \"impostor key\") in\n    ()\n  receive\n    () -> ()\n  done\n}\n\ncreate starter ()");
        }
    }

    var overview_init = function () {
        var overview_string = "<div id=\"output-content\" class=\"row\">";
        for (var u = 1; u < 4; u++) {
            overview_string = overview_string + "<div class=\"col-md-4\"><div id=\"overview-heading-container-"+u+"\"><div id=\"overview-heading-info-"+u+"\"><div class=\"col-md-8\">--</div><div class=\"col-md-4\"><span class=\"glyphicon glyphicon-user\"></span></div></div><div id=\"overview-heading-dropdown-"+u+"\"><div class=\"btn-group\"><button id=\"dropdown-menu-"+u+"\" class=\"btn btn-default btn-sm dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">Choose actor...<span class=\"caret\"></span></button><ul class=\"dropdown-menu\" aria-labelledby=\"dropdown-menu-"+u+"\"></ul></div></div></div><div class=\"radio\"><label><input type=\"radio\" name=\"execute-options\" id=\"exe-op-"+u+"\" value=\"-1\" disabled>Execute next</label></div><div id=\"overview-table-"+u+"\"></div></div>";
        }
        overview_string = overview_string + "</div>";
        $("#output-content").replaceWith(overview_string);
    }

    var detailed_init = function () {
        var detail_string = "<div id=\"output-content\"><div class=\"output-contents\"><div id=\"actor-id-info\"></div><div class=\"panel panel-default\" id=\"actor-display\"><div id=\"display-heading\" class=\"panel-heading\">Program state will appear here once execution begins.</div><div class=\"panel-body\"><div id=\"actor-info-id\" class=\"col-md-4\"></div><div id=\"actor-info-beh\" class=\"col-md-4\"></div><div id=\"actor-info-cr\" class=\"col-md-4\"></div><div id=\"actor-info-table\" class=\"row\"><div id=\"actor-inbox-table\" class=\"col-md-6\"></div><div id=\"actor-bindings-table\" class=\"col-md-6\"></div></div></div></div><div class=\"panel panel-default\" id=\"message-editor\"><div id=\"editor-heading\" class=\"panel-heading\">Message Editor</div><div id=\"editor-body\" class=\"panel-body\">Select a message to edit its contents here.</div></div></div></div>";
        $("#output-content").replaceWith(detail_string);
    }

    var get_overview = function () {

        var overview_string = "";
        for (var t = 1; t < 4; t++) {

            var selected_item = $("#dropdown-menu-"+t).text().trim();
            var aid = selected_item.charAt(6);
            var number_of_actors = Object.keys(ticket.state["_isGlobalEnv"]["_geActorInstances"]).length;

            if (selected_item === "Choose actor..." || number_of_actors < aid) {
                overview_string = overview_string + "<div class=\"col-md-4\"><div id=\"overview-heading-container-"+t+"\"><div id=\"overview-heading-info-"+t+"\"><div class=\"col-md-8\">--</div><div class=\"col-md-4\"><span class=\"glyphicon glyphicon-user\"></span></div></div><div id=\"overview-heading-dropdown-"+t+"\"><div class=\"btn-group\"><button id=\"dropdown-menu-"+t+"\" class=\"btn btn-default btn-sm dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">Choose actor...<span class=\"caret\"></span></button><ul class=\"dropdown-menu\" aria-labelledby=\"dropdown-menu-"+t+"\">" + populate_dropdown(ticket.state["_isGlobalEnv"]["_geActorInstances"]) + "</ul></div></div></div><div class=\"radio\"><label><input type=\"radio\" name=\"execute-options\" id=\"exe-op-"+t+"\" value=\"-1\" disabled>Execute next</label></div><div id=\"overview-table-"+t+"\"></div></div>";
            } else {
                var glyphicon;
                if (ticket.ready.indexOf(parseInt(aid)) !== -1) {
                    glyphicon = "<span class=\"green glyphicon glyphicon-user\"></span>";
                } else {
                    glyphicon = "<span class=\"glyphicon glyphicon-user\"></span>";
                }
                overview_string = overview_string + "<div class=\"col-md-4\"><div id=\"overview-heading-container-"+t+"\"><div id=\"overview-heading-info-"+t+"\"><div class=\"col-md-8\">" + ticket.state["_isGlobalEnv"]["_geActorInstances"][aid]["_aiBehaviour"]["behaviourName"] + "</div><div class=\"col-md-4\">" + glyphicon + "</div></div><div id=\"overview-heading-dropdown-"+t+"\"><div class=\"btn-group\"><button id=\"dropdown-menu-"+t+"\" class=\"btn btn-default btn-sm dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">" + selected_item + "<span class=\"caret\"></span></button><ul class=\"dropdown-menu\" aria-labelledby=\"dropdown-menu-"+t+"\">" + populate_dropdown(ticket.state["_isGlobalEnv"]["_geActorInstances"]) + "</ul></div></div></div><div class=\"radio\"><label><input type=\"radio\" name=\"execute-options\" id=\"exe-op-"+t+"\" value=\""+aid+"\">Execute next</label></div><div id=\"overview-table-"+t+"\">" + get_inbox_table(ticket.state["_isGlobalEnv"]["_geActorInstances"][aid]["_aiInbox"]).substring(6) + "</div></div>";
            }
        }
        $("#output-content").html(overview_string);
        for (t = 1; t < 4; t++) {
            attach_listeners(t);
        }
    }
    
    var attach_listeners = function (t) {
        $("#overview-heading-dropdown-"+t+" li a").click(function(){
            var selected_text = $(this).text();
            $(this).parents(".btn-group").find(".dropdown-toggle").html(selected_text+"<span class=\"caret\"></span>");
            var aid = selected_text.charAt(6);
            var inbox_string = get_inbox_table(ticket.state["_isGlobalEnv"]["_geActorInstances"][aid]["_aiInbox"]);

            var info_string = "<div class=\"col-md-8\">" + ticket.state["_isGlobalEnv"]["_geActorInstances"][aid]["_aiBehaviour"]["behaviourName"] + "</div>";

            // If actor is ready, produce green glyph, else grey
            if (ticket.ready.indexOf(parseInt(aid)) === -1) {
                info_string = info_string + "<div class=\"col-md-4\"><span class=\"glyphicon glyphicon-user\"></span></div>"; 
            } else {
                info_string = info_string + "<div class=\"col-md-4\"><span class=\"green glyphicon glyphicon-user\"></span></div>";
            }

            $("#exe-op-"+t).prop("disabled", false);
            $("#exe-op-"+t).val(aid);
            $("#overview-table-"+t).html(inbox_string.substring(6));
            $("#overview-heading-info-"+t).html(info_string);
        });
        $("#exe-op-"+t).click(function () {
            ticket.state["_isCurrentAID"] = parseInt($(this).val());
        });
    }

    var blank_overview = function () {
        var overview_string = "";
        for (var t = 1; t < 4; t++) {
            overview_string = overview_string + "<div class=\"col-md-4\"><div id=\"overview-heading-container-"+t+"\"><div id=\"overview-heading-info-"+t+"\"><div class=\"col-md-8\">--</div><div class=\"col-md-4\"><span class=\"glyphicon glyphicon-user\"></span></div></div><div id=\"overview-heading-dropdown-"+t+"\"><div class=\"btn-group\"><button id=\"dropdown-menu-"+t+"\" class=\"btn btn-default btn-sm dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">Choose actor...<span class=\"caret\"></span></button><ul class=\"dropdown-menu\" aria-labelledby=\"dropdown-menu-"+t+"\">" + populate_dropdown(ticket.state["_isGlobalEnv"]["_geActorInstances"]) + "</ul></div></div></div><div class=\"radio\"><label><input type=\"radio\" name=\"execute-options\" id=\"exe-op-"+t+"\" value=\"-1\" disabled>Execute next</label></div><div id=\"overview-table-"+t+"\"></div></div>";
        }
        $("#output-content").html(overview_string);
        for (t = 1; t < 4; t++) {
            attach_listeners(t);
        }
    }
    // Below this point is all the code for Detailed Actor View


    // Make tabs function as tabs instead of moving elements on the page. 
    $(document).delegate('#prog-textarea', 'keydown', function(e) {
        var key_code = e.keyCode || e.which;

        if (key_code === 9) {
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

    var get_detailed_view = function () {

        var id_info_string = "<div class=\"col-md-6\">Executing Actor ID: " + ticket.state["_isCurrentAID"] + "</div>";
        id_info_string = id_info_string + "<div class=\"col-md-6\">Next Available Actor ID: " + ticket.state["_isGlobalEnv"]["_geNextAvailableActor"] + "</div>";

        var dropdown_string = "<div id=\"choose-actor\" class=\"dropdown\"> <button class=\"btn btn-default dropdown-toggle\" type=\"button\" id=\"dropdown-actor-menu\" data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"true\"> Choose actor instance... <span class=\"caret\"></span> </button> <ul class=\"dropdown-menu\" aria-labelledby=\"actor-dropdown\">" + populate_dropdown(ticket.state["_isGlobalEnv"]["_geActorInstances"]) + "</ul> <span title=\"The actor displayed here is the one that will execute next, assuming it is ready (ready actors are shown in green).\" class=\"glyphicon glyphicon-info-sign\" aria-hidden=\"true\"></span>";

        $("#actor-id-info").html(id_info_string);
        $("#display-heading").html(dropdown_string);
        $(".glyphicon").tooltip({ placement: "left" });

        // Changes the isCurrentAID when an actor is selected in the dropdown;
        // this allows the user to control which actors execute, and when.
        $("#display-heading").on("click", "li a", function (event) {
            var selected_string = event.target.outerText;
            var split_string = selected_string.split(" ");
            populate_display(ticket.state["_isGlobalEnv"]["_geActorInstances"], parseInt(split_string[1]));
            ticket.state["_isCurrentAID"] = parseInt(split_string[1]);
        });
        
        populate_display(ticket.state["_isGlobalEnv"]["_geActorInstances"], ticket.state["_isCurrentAID"]);
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
                dropdown_string = dropdown_string + "<li><a href=\"#\">Actor " + actor_instance["_aiId"] + " (" + actor_instance["_aiBehaviour"]["behaviourName"] + ")</a></li>"; 
            } else {
                dropdown_string = dropdown_string + "<li class=\"bg-success\"><a href=\"#\">Actor " + actor_instance["_aiId"] + " (" + actor_instance["_aiBehaviour"]["behaviourName"] + ")</a></li>"; 
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
                    } else if (selected_type === "EncryptedV") {
                        input_val = $("#key"+ n).val();
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

                        // Separate updating for lists, as there is no dropdown
                        // for each element to give the type.
                        if (selected_type === "ListV") {
                            var new_list = new Object();
                            new_list["tag"] = "ListV";
                            new_list["contents"] = [];
                            for (var p = 0; p < input_val.length; p++) {
                                new_list["contents"].push(get_list_element(input_val, p));
                            }
                            ticket.state["_isGlobalEnv"]["_geActorInstances"][actor_id]["_aiInbox"][msg_no - 1][n] = new_list;
                        } else if (selected_type === "EncryptedV") {
                            ticket.state["_isGlobalEnv"]["_geActorInstances"][actor_id]["_aiInbox"][msg_no - 1][n]["contents"][1] = input_val;
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
        } else if (type === "StringV" || type === "EncryptedV") {
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
        } else if (msg[tuple]["tag"] === "EncryptedV") {
            contents = contents + "<div id=\"input"+tuple+"\"><textarea id=\"key"+tuple+"\" cols=\"8\" rows=\"1\">New key</textarea></div>";
        } else {
            contents = contents + "<textarea id=\"input"+tuple+"\" cols=\"15\" rows=\"1\"></textarea>";
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
                    } else if (inbox[i][j]["tag"] === "EncryptedV") {
                        inbox_table = inbox_table + "EncryptedV (Key " + inbox[i][j]["contents"][1] + ")";
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
        } else if (selected_text === "EncryptedV") {
            $("#" + element_id).replaceWith("<div class=\"enckeys\" id=\""+element_id+"\"><textarea id=\"key"+tuple_no+"\" cols=\"15\" rows=\"1\">New key</textarea></label></div>");
        } else {
            $("#" + element_id).replaceWith("<textarea id=\""+element_id+"\" cols=\"8\" rows=\"1\"></textarea>");
        }
    }
});
