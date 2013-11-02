window.version = '0.6'

if (typeof window.console == "undefined")
    my_log = () ->
else
    my_log = (args...) -> console.log(args...)

window.msg_error = (msg,rest...) ->
    $('div.container').text("ERROR : #{msg}")
    if rest.length>0
        my_log("ERROR : #{msg}",rest)
    else
        my_log("ERROR : #{msg}")

window.msg_info = (msg,rest...) ->
    if rest.length>0
        my_log("INFO : #{msg}",rest)
    else
        my_log("INFO : #{msg}")

window.msg_debug = (msg,rest...) ->
    if rest.length>0
        my_log("DEBUG : #{msg}",rest)
    else
        my_log("DEBUG : #{msg}")


# This "scheduler" is designed to be used for tasks that may take some time, and
# will be called often.  For example, updating the heatmap, or datatable.
# It will schedule a given task after a set timeout, if the same task is re-scheduled before
# it is run, its schedule time will be reset
# Use: "schedule" for tasks that may be overwritten by later tasks
#      "schedule_now" for tasks that must not be skipped (these will run *now*)
class ScheduleTasks
    constructor: () ->
        @tasks = {}
        @interval = 100

    schedule: (lbl, func, interval=@interval) ->
        if @tasks[lbl]
            # Already a pending update.  Cancel it and re-schedule
            #msg_debug("clearing",lbl,@tasks[lbl].id)
            clearTimeout(@tasks[lbl].id)

        # No task pending.  add it, and schedule an update
        @tasks[lbl] =
            func: func
            id: setTimeout((() => @_runNow(lbl)), interval)
        #msg_debug("set",lbl,@tasks[lbl].id)

    # Used when an important task must run and not be overridden by later tasks
    schedule_now: (lbl, func) ->
        @tasks[lbl] =
            func: func
        @_runNow(lbl)

    _runNow: (lbl) ->
        task = @tasks[lbl]
        if task
            # Ensure there is still a task, may have been run by schedule_now()
            delete @tasks[lbl]
            #msg_debug("running",lbl,task.id)
            task.func()


window.scheduler = new ScheduleTasks()

# Convert any parameters in the pages url into a hash+array
window.get_url_vars = () ->
    vars = []
    hash = null
    hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&')
    for i in [0..hashes.length-1]
        hash = hashes[i].split('=')
        vars.push(hash[0])
        vars[hash[0]] = hash[1]
    vars

window.setup_about_modal = () ->
    about = $(require("../templates/about.hbs")(version: version))
    $('#about-modal').replaceWith(about)
