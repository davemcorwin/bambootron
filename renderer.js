// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// All of the Node.js APIs are available in this process.
'use strict'
const Elm = require('./build/elm.js')
const container = document.getElementById('app')
const app = Elm.Main.embed(container)
