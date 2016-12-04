'use strict'
const electron = require('electron')
const chokidar = require('chokidar')

const app = electron.app
const BrowserWindow = electron.BrowserWindow

let mainWindow = null

app.on('ready', createWindow)
app.on('window-all-closed', quitIfNotMac)
app.on('activate', createWindow)

// tell chokidar to watch these files for changes and reload the window
chokidar
  .watch(['index.html','build/*'])
  .on('change', () => {
    if (mainWindow) mainWindow.reload()
  })

// This will create our app window, no surprise there
function createWindow () {
  if (mainWindow) return

  mainWindow = new BrowserWindow({
    width: 1024,
    height: 728,
    webPreferences: {
      experimentalFeatures: true
    }
  })

  // display the index.html file
  mainWindow.loadURL(`file://${ __dirname }/index.html`)

  // open dev tools by default so we can see any console errors
  mainWindow.webContents.openDevTools()

  mainWindow.on('closed', () => { mainWindow = null })
}

function quitIfNotMac() {
  if (process.platform !== 'darwin') app.quit()
}
