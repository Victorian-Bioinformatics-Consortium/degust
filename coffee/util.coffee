window.dge_vis_version = '0.5'

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

    schedule: (lbl, func) ->
        if @tasks[lbl]
            # Already a pending update.  Cancel it and re-schedule
            #msg_debug("clearing",lbl,@tasks[lbl].id)
            clearTimeout(@tasks[lbl].id)

        # No task pending.  add it, and schedule an update
        @tasks[lbl] =
            func: func
            id: setTimeout((() => @_runNow(lbl)), @interval)
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






window.setup_about_modal = () ->
    html = "<div id='about-modal' class='modal hide fade' tabindex='-1' role='dialog' aria-labelledby='myModalLabel' aria-hidden='true'>
      <div class='modal-header'>
        <button type='button' class='close' data-dismiss='modal' aria-hidden='true'>x</button>
        <h3 id='myModalLabel'>About DGE-Vis</h3>
      </div>
      <div class='modal-body'>
        <p>DGE-Vis : Visualize and explore RNA-seq differential gene-expression data.</p>
        <p>Version : #{dge_vis_version}</p>
        <p>Visit the <a href='http://vicbioinformatics.com/software.dge-vis.shtml'>DGE-Vis project page</a>,
           or the <a href='https://github.com/Victorian-Bioinformatics-Consortium/dge-vis'>source code on GitHub</a>
        </p>
        <div>Written by <a href='http://thunking.drp.id.au/'>David R. Powell</a></div>
        <div class='supported-by'>
          Supported by <a href='http://vicbioinformatics.com/'>Victorian Bioinformatics Consortium, Monash University</a>
          and <a href='http://www.vlsci.org.au/lscc'>VLSCI\'s Life Sciences Computation Centre</a>
        </div>
      </div>
      <div class='modal-footer'>
        <button class='btn btn-primary' data-dismiss='modal' aria-hidden='true'>Close</button>
      </div>
    </div>"

    $('#about-modal').replaceWith(html)
