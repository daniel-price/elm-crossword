# Getting started
Install dependencies:
`npm install`

Install git commit hook:
`run prepare` 

# Debugging
`npx elm-live src/Main.elm`

# Mobile Debugging
Start a live server:
`live-server`

Then you can navigate to your PC's network IP and port from your mobile to see it in action.

Include the commented out scripts in `index.html` for dev tools on your mobile


# Mock backend

Requires [json-server](https://github.com/typicode/json-server)
`npm install -g json-server`

`json-server --watch db.json`


# Example

[See it in action](https://daniel-price.github.io/elm-crossword/)
