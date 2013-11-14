# Snippet from stackoverflow to invoke a click() d3 will recognise
jQuery.fn.d3Click = () ->
    this.each( (i, e) ->
        evt = document.createEvent("MouseEvents")
        evt.initMouseEvent("click", true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null)

        e.dispatchEvent(evt)
    )

tour_steps =
  [
    title: "<strong>Degust</strong>"
    content: "This web tool can be used to explore overlap between gene lists.  The filters for significance can be dynamically updated; the intersection and difference between gene-lists displayed in a table and downloaded."
    orphan: true
    backdrop: true
  ,
    element: '.fdr-fld'
    placement: 'left'
    title:"<strong>FDR threshold</strong>"
    content: "Modify the threshold for False-Discovery-Rate.  The numbers in the table above, and in the Venn diagram are updated dynamically."
  ,
    element: '.fc-fld'
    placement: 'left'
    title:"<strong>Log fold-change threshold</strong>"
    content: "Modify the filter for significance using log fold-change.  For example, a setting of '1.0' will determine a gene as significant if it has log fold-change greater than 1.0 or less than -1.0 (that is 2x up or down)."
  ,
    element: '#csv-download'
    placement: 'left'
    backdrop: true
    title:"Gene list download"
    content: "Anything displayed in the gene list table can be downloaded as a CSV file."
  ,
  ]

window.setup_tour = (show_tour) ->
    tour = new Tour()    #({debug: true})
    #window.tour = tour
    tour.addSteps(tour_steps)
    $('a#tour').click(() -> tour.restart())
    tour.start() if show_tour
