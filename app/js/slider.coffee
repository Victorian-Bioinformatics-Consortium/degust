# ------------------------------------------------------------
# Slider routines
class Slider
    # Set the slider to the nearest entry
    set_slider: (v) ->
      set_i=0
      $.each(@stepValues, (i,v2) ->
        if (v2<=v)
          set_i = i
      )
      @slider.slider("value", set_i)

    fmt: (v) ->
      n=Number(v)
      if n==undefined
          v
      else if n>0 && n<0.001
          n.toExponential(0)
      else
          v

    constructor: (@opts) ->
        @stepValues = opts.step_values || [0..10]
        @slider = $(opts.id).slider({
          animate: true,
          min: 0,
          max: @stepValues.length-1,
          value: 1,
          slide: (event, ui) =>
            v = @stepValues[ui.value]
            $(opts.input_id).val(@fmt(v))
            opts.on_change(v)
        })
        if opts.val != undefined
            $(opts.input_id).val(opts.val)
            @set_slider(opts.val)

        self = this
        $(opts.input_id).keyup(() ->
          v = $(this).val()
          if opts.validator(v)
            $(this).removeClass('error')
            self.set_slider(v)
            opts.on_change(v)
          else
            $(this).addClass('error')
        )

window.Slider = Slider