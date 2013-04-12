script = (params) -> "r-json.cgi?code=#{window.my_code}" + if params then "&#{params}" else ""

mod_settings = null
reset_settings = () ->
    mod_settings = $.extend(true, {}, settings)

data = null
grid = null
asRows = null

init_table = () ->
    options =
      enableCellNavigation: false
      enableColumnReorder: false
      multiColumnSort: false
      forceFitColumns: true

    grid = new Slick.Grid("#grid", [], [], options)

    update_data()

csv_or_tab = () -> $('.fmt:checked').val()

warnings = () ->
    $('div.id-column .help-inline').text('')
    $('div.id-column').removeClass('error')

    if !mod_settings.id_column
        $('div.id-column').addClass('error')
        $('div.id-column .help-inline').text('You must specify a unique ID column')
    else
        dups=false
        ids = {}
        $.each(asRows, (i,x) ->
            v = x[mod_settings.id_column]
            dups = true if ids[v]
            ids[v]=1
        )
        if dups
            $('div.id-column').addClass('error')
            $('div.id-column .help-inline').text('ID column contains duplicates')

save = () ->
    mod_settings.name = $("input.name").val()
    conditions_to_settings()


    $('#saving-modal').modal({'backdrop': 'static', 'keyboard' : false})
    $('#saving-modal .modal-body').html("Saving...")
    $('#saving-modal .modal-footer').hide()

    $.ajax({
        type: "POST",
        url: script("query=save"),
        data: {settings: JSON.stringify(mod_settings)},
        dataType: 'json'
    }).done((x) ->
        $('#saving-modal .modal-body').html("Save successful.")
        $('#saving-modal .view').show()
     ).fail(
        $('#saving-modal .modal-body').html("Failed!")
        $('#saving-modal .view').hide()
     ).always(
        $('#saving-modal').modal({'backdrop': true, 'keyboard' : true})
        $('#saving-modal .modal-footer').show()
        $('#saving-modal #close-modal').click( () -> window.location = window.location)
     )


update_data = () ->
    return if !data

    $("input.name").val(mod_settings.name || "")

    asRows = null
    switch csv_or_tab()
        when 'TAB' then asRows = d3.tsv.parse(data)
        when 'CSV' then asRows = d3.csv.parse(data)
        else asRows = []

    column_keys = d3.keys(asRows[0])

    opts = ""
    $.each(column_keys, (i, col) ->
        opts += "<option value='#{col}'>#{col}</option>"
    )
    $('select.id-column').html("<option value=''>--- Not selected ---</option>" + opts)
    if mod_settings.id_column
        $("select.id-column option[value='#{mod_settings.id_column}']").attr('selected','selected')

    $('select.ec-column').html("<option value=''>--- Optional ---</option>" + opts)
    if mod_settings.ec_column
        $("select.ec-column option[value='#{mod_settings.ec_column}']").attr('selected','selected')

    $('select.info-columns').html(opts)
    info_columns = mod_settings.info_columns || []
    $.each(info_columns, (i,col) -> $("select.info-columns option[value='#{col}']").attr('selected','selected'))
    $("select.info-columns").multiselect('refresh')

    $('select#hide-columns').html(opts)
    to_hide = mod_settings.hide_columns || []
    $.each(to_hide, (i,col) -> $("select#hide-columns option[value='#{col}']").attr('selected','selected'))
    $("select#hide-columns").multiselect('refresh')

    update_table()

    $('.condition:not(.template)').remove()
    for n,lst of mod_settings.replicates
        create_condition_widget(n, lst, n in (mod_settings['init_select'] || []))

update_table = () ->
    mod_settings.hide_columns ||= []
    column_keys = d3.keys(asRows[0])
    columns = column_keys.filter((key,i) -> key not in mod_settings.hide_columns ).map((key,i) ->
        id: key
        name: key
        field: key
        sortable: false
    )

    $('#grid-info').text("Number of columns = #{columns.length}")
    grid.setColumns(columns)
    grid.setData(asRows)
    grid.updateRowCount();
    grid.render();

    warnings()

set_guess_type = () ->
    if data.split("\t").length>20
        $('#fmt-tab').attr('checked','checked')
    else
        $('#fmt-csv').attr('checked','checked')

create_condition_widget = (name, selected, is_init) ->
    cond = $('.condition.template').clone(true)
    cond.removeClass('template')

    $("input.col-name",cond).val(name) if name

    column_keys = d3.keys(asRows[0])
    opts = ""
    $.each(column_keys, (i, col) ->
        sel = if col in selected then 'selected="selected"' else ''
        opts += "<option value='#{col}' #{sel}>#{col}</option>"
    )
    $("select.columns",cond).html(opts)
    $("select.columns",cond).multiselect(
        noneSelectedText: '-- None selected --'
        selectedList: 4
    )
    $('.init-select',cond).prop('checked', is_init)

    $(".condition-group").append(cond)

    # Auto setting of condition name based on columns selected
    $("select",cond).change(() ->
        inp = $("input.col-name",cond)
        if inp.val()=="" || !inp.data('edited')
            lst = []
            $('select.columns option:selected',cond).each( (j,opt) -> lst.push($(opt).val()))
            inp.val(common_prefix(lst))
    )

    # Track editing of the name.  Blanking the name out makes it "un-edited"
    $("input.col-name",cond).data('edited', false)
    $("input.col-name",cond).change(() ->
        inp = $("input.col-name",cond)
        inp.data('edited', inp.val()!="")
    )

# Return the longest common prefix of the list of strings passed in
common_prefix = (lst) ->
    lst = lst.slice(0).sort()
    tem1 = lst[0]
    s = tem1.length;
    tem2 = lst.pop();
    while(s && tem2.indexOf(tem1) == -1)
        tem1 = tem1.substring(0, --s)
    tem1


del_condition_widget = (e) ->
    $(e.target).parents(".condition").remove()

conditions_to_settings = () ->
    c = {}
    init_select = []
    $('.condition:not(.template)').each( (i,e) ->
        lst = []
        $('select.columns option:selected',e).each( (j,opt) -> lst.push($(opt).val()))
        name = $('.col-name',e).val() || "Cond #{i+1}"
        c[name] = lst
        init_select.push(name) if $('.init-select',e).is(':checked')
    )
    mod_settings.replicates = c
    mod_settings.init_select = init_select

init = () ->
    reset_settings()
    d3.text(script("query=csv"), "text/csv", (dat,err) ->
        if err
            $('div.container').text("ERROR : #{err}")
            return
        data = dat
        set_guess_type()
        init_table()
    )

    $('input.fmt').click(update_data)
    $('#save').click(save)
    $('#cancel').click(() -> reset_settings(); update_data())
    $('.view').attr('href', script())

    $('select.id-column').change(() ->
        mod_settings.id_column = $("select.id-column option:selected").val()
        warnings()
    )
    $('select.ec-column').change(() ->
        mod_settings.ec_column = $("select.ec-column option:selected").val()
        warnings()
    )

    $('select.info-columns').change(() ->
        info=[]
        $("select.info-columns option:selected").each (i,e) -> info.push($(e).val())
        mod_settings.info_columns = info
    )
    $("select.info-columns").multiselect(
        noneSelectedText: '-- None selected --'
        selectedList: 4
    )

    $('select#hide-columns').change(() ->
        hide=[]
        $("select#hide-columns option:selected").each (i,e) -> hide.push($(e).val())
        mod_settings.hide_columns = hide
        update_table()
    )
    $("select#hide-columns").multiselect(
        noneSelectedText: '-- None selected --'
        selectedList: 4
    )

    $('#add-condition').click(() ->
        create_condition_widget("", [])
    )

    $('.del-condition').click(del_condition_widget)


$(document).ready(() -> init() )
