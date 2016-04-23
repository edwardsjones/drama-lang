# drama-lang

This repository will house the code I am writing as part of my 3rd year project at the University of Southampton. 

Drama is an actor based language, implemented in Haskell; this repository will provide tools to write a program in this language, and execute it.
Execution can be done normally (i.e. all in one go), or it can be stepped through, allowing the state to be examined at each step. 

In order to use this tool, it is reccomended to install Haskell Stack.
Then, simply clone this repo and run the following commands from the root directory.

`stack init`
`stack install`
`stack exec drama-server`

This will build the project, and start a local server running on port 5000.
Simply open the `default.html` file from /bin/drama-server/views/ folder, and begin using the tool.

This tool was developed in Google Chrome, on an 11 inch Macbook Air; results may vary on other platforms/browsers.
