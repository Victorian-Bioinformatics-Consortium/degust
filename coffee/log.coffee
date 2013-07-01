window.msg_error = (msg,rest...) ->
    $('div.container').text("ERROR : #{msg}")
    if rest.length>0
        console.log("ERROR : #{msg}",rest)
    else
        console.log("ERROR : #{msg}")

window.msg_info = (msg,rest...) ->
    if rest.length>0
        console.log("INFO : #{msg}",rest)
    else
        console.log("INFO : #{msg}")

window.msg_debug = (msg,rest...) ->
    if rest.length>0
        console.log("DEBUG : #{msg}",rest)
    else
        console.log("DEBUG : #{msg}")
