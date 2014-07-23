script = (params) -> "r-json.cgi?code=#{window.my_code}" + if params then "&#{params}" else ""

init_page = () ->
    $('input[name="code"]').val(my_code)

init = () ->
    code = get_url_vars()["code"]
    if !code?
        log_error "Missing Code"
    else
        window.my_code = code
        $.ajax({
            type: "GET",
            url: script("query=settings"),
            dataType: 'json'
        }).done((json) ->
            window.settings = json
            init_page(true)
         ).fail((x) ->
            log_error "Failed to get settings!",x
        )


$(document).ready(() -> init() )
