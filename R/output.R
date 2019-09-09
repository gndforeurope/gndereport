#' @export
my_datatable <- function(my_tbl, paging = FALSE, searching = FALSE, ...) {

  # https://stackoverflow.com/questions/41966732/r-datatable-buttons-export-with-formated-cells
  DT::datatable(my_tbl,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(info = TRUE, paging = paging, searching = searching,
                               fixedHeader = TRUE, fixedHeader.footer = TRUE,
                               columnDefs = list(list(className = 'dt-head-left', targets = '_all')),
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                               ),
                ...
  )

}
