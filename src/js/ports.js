module.exports = function(app, win) {
  let isEditing = false

  // app.ports.editing.subscribe(function(editing) {
  //   isEditing = editing
  // })

  const keys = [
    'ArrowDown',
    'ArrowLeft',
    'ArrowRight',
    'ArrowUp',
    'Tab',
    'Enter',
    'Escape'
  ]

  win.onkeydown = function(e) {
    if (keys.includes(e.key)) {
      if ( (isEditing && !['ArrowLeft', 'ArrowRight'].includes(e.key)) || !isEditing)
        e.preventDefault()
        app.ports.keys.send([e.key, e.shiftKey])
    }
  }
}
