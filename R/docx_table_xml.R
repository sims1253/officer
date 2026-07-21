##==============================================================================
##  docx_table_xml.R  --  fork-local helpers for native-styled Word tables
##
##  Purpose
##  -------
##  Word tables that must BOTH
##    (a) inherit the document template's named table/paragraph styles, AND
##    (b) use cell merges the table primitives cannot express:
##        - grouped (multi-row, merged) headers   -> <w:gridSpan>
##        - vertical merges of consecutive values  -> <w:vMerge>
##        - full-width sub-header divider rows     -> <w:gridSpan> = ncol
##
##  Delivery
##  --------
##  Built on body_add_xml(), which upstream officer's maintainer identified as
##  the intended escape hatch for this exact combination (davidgohel/officer,
##  issue #731, closed as out-of-scope for the table primitives). These helpers
##  do NOT touch block_table() / body_add_table() / table_docx(); the fork's
##  table core stays byte-identical to upstream, so rebasing is trivial and the
##  Word/PowerPoint rendering paths stay consistent.
##
##  How style inheritance works
##  ---------------------------
##  Like officer's built-in table blocks, these helpers emit placeholder
##  attributes (w:tstlname= / w:pstlname=) that convert_custom_styles_in_wml()
##  resolves to real style ids when the document is saved (print.rdocx ->
##  write_docx). That function walks the WHOLE document body, so markup injected
##  via body_add_xml() is resolved and validated identically to block_table().
##==============================================================================


# table_header ----

#' @export
#' @title Grouped (multi-row) table header
#' @description Define a header made of one or more rows, where a top row groups
#' several columns under a single label (rendered with `<w:gridSpan>`). Pass the
#' result to [table_xml()] / [body_add_table_xml()] via their `header` argument
#' (instead of `TRUE`).
#'
#' `groups` is a named list. Each name is a group label displayed in the top
#' header row, spanning the columns listed in its value. The values are column
#' names of the data.frame, in column order; if an element is named, that name
#' is used as the displayed leaf (second-row) label instead of the column name.
#' @param groups a named list. Names = group labels; values = character vectors
#' of column names belonging to the group, in column order.
#' @return An object of class `table_header`.
#' @examples
#' table_header(list(
#'   Treatment = c("Agent" = "treatment", "Dose" = "dose"),
#'   Efficacy  = c("n" = "n", "Responder" = "resp", "ORR" = "orr")
#' ))
#' @family docx native-style table helpers
table_header <- function(groups = list()) {
  if (!is.list(groups)) {
    stop("groups must be a named list", call. = FALSE)
  }
  if (length(groups) > 0L && (is.null(names(groups)) || any(names(groups) == ""))) {
    stop("each group must be named; the name is the displayed group label",
         call. = FALSE)
  }
  if (length(groups) > 0L) {
    flat <- unlist(groups, use.names = FALSE)
    if (anyDuplicated(flat)) {
      stop("a column appears in more than one group: ",
           paste(flat[duplicated(flat)], collapse = ", "), call. = FALSE)
    }
  }
  structure(list(groups = groups), class = "table_header")
}

#' @export
print.table_header <- function(x, ...) {
  cat("<table_header> grouped header\n", sep = "")
  for (g in names(x$groups)) {
    cat("  ", g, " -> ", paste(x$groups[[g]], collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

# internal: expand a table_header against the data.frame columns.
# returns list(flat = ordered column names, leaf = leaf labels, groups = groups).
# Validates that the flattened columns match the data.frame columns, in order.
flatten_table_header <- function(th, cols) {
  groups <- th$groups
  flat <- as.character(unlist(groups, use.names = FALSE))
  if (!identical(flat, as.character(cols))) {
    stop("table_header() columns (", paste(flat, collapse = ", "),
         ") must match the data.frame columns, in the same order (",
         paste(cols, collapse = ", "), ").", call. = FALSE)
  }
  leaf <- character(length(flat))
  k <- 0L
  for (g in names(groups)) {
    v <- groups[[g]]
    labs <- if (!is.null(names(v))) names(v) else v
    leaf[k + seq_along(v)] <- as.character(labs)
    k <- k + length(v)
  }
  list(flat = flat, leaf = leaf, groups = groups)
}


# table_subheader ----

#' @export
#' @title Full-width sub-header row
#' @description Mark a sub-header divider row inside a table body: a single row
#' spanning all columns, containing only a title, that groups the rows that
#' follow it (e.g. an "endpoint category" banner). Use it as an element of the
#' `value` list passed to [table_xml()] / [body_add_table_xml()].
#'
#' A sub-header row also *resets* any active vertical merge: consecutive-value
#' merging restarts after the divider.
#' @param label single character string displayed in the divider row.
#' @param style paragraph style name applied to the divider cell. If `NULL`, the
#' table's `header_style` is used (or `"Normal"` if that is also `NULL`).
#' @return An object of class `table_subheader`.
#' @examples
#' table_subheader("Endpoint category: Mortality")
#' table_subheader("Endpoint category: Morbidity", style = "strong")
#' @family docx native-style table helpers
table_subheader <- function(label, style = NULL) {
  label <- as.character(label)
  if (length(label) != 1L || is.na(label)) {
    stop("label must be a single (non-NA) character string", call. = FALSE)
  }
  if (!is.null(style)) {
    style <- as.character(style)
    if (length(style) != 1L || is.na(style)) {
      stop("style must be a single character string or NULL", call. = FALSE)
    }
  }
  structure(list(label = label, style = style), class = "table_subheader")
}

#' @export
print.table_subheader <- function(x, ...) {
  sty <- if (!is.null(x$style)) paste0(" [style: ", x$style, "]") else ""
  cat("<table_subheader> '", x$label, "'", sty, "\n", sep = "")
  invisible(x)
}


# internal: normalise column paragraph styles to a named character vector
# (column name -> style name). Accepts a table_stylenames() object, a named
# list, or a named character vector. Columns not listed default to "Normal".
normalize_table_stylenames <- function(stylenames, cols) {
  if (inherits(stylenames, "table_stylenames")) {
    sl <- stylenames$stylenames
  } else if (is.list(stylenames)) {
    sl <- stylenames
  } else {
    sl <- as.list(stylenames)
  }
  style_of <- rep("Normal", length(cols))
  names(style_of) <- cols
  if (length(sl) > 0L) {
    nms <- names(sl)
    if (is.null(nms) || any(!nzchar(nms))) {
      stop("stylenames must be named by column name", call. = FALSE)
    }
    for (i in seq_along(sl)) {
      cn <- nms[i]
      if (cn %in% cols) {
        style_of[[cn]] <- as.character(sl[[i]])
      }
    }
  }
  style_of
}

# internal: one <w:tc> cell.
#   pstyle   : paragraph-style NAME (resolved later via w:pstlname)
#   gridspan : columns this cell spans (horizontal merge)
#   vmerge   : "none" | "restart" | "continue" (vertical merge)
table_xml_cell <- function(text = "", pstyle = "Normal", gridspan = 1L,
                           vmerge = c("none", "restart", "continue")) {
  vmerge <- match.arg(vmerge)
  tcpr <- ""
  if (gridspan > 1L) {
    tcpr <- paste0(tcpr, sprintf('<w:gridSpan w:val="%d"/>', gridspan))
  }
  if (vmerge == "restart") {
    tcpr <- paste0(tcpr, '<w:vMerge w:val="restart"/>')
  } else if (vmerge == "continue") {
    tcpr <- paste0(tcpr, "<w:vMerge/>")
  }
  tcpr <- if (nzchar(tcpr)) paste0("<w:tcPr>", tcpr, "</w:tcPr>") else ""
  if (vmerge == "continue") {
    # a merged-continuation cell must contain a paragraph; content is hidden
    return(paste0("<w:tc>", tcpr, "<w:p/></w:tc>"))
  }
  paste0(
    "<w:tc>", tcpr,
    '<w:p><w:pPr><w:pStyle w:pstlname="', pstyle, '"/></w:pPr>',
    "<w:r><w:t xml:space=\"preserve\">",
    htmlEscapeCopy(enc2utf8(text)),
    "</w:t></w:r></w:p></w:tc>"
  )
}

# internal: one <w:tr> row.
table_xml_row <- function(cells, header = FALSE) {
  trpr <- if (header) "<w:trPr><w:tblHeader/></w:trPr>" else ""
  paste0("<w:tr>", trpr, paste0(cells, collapse = ""), "</w:tr>")
}

# internal: header row(s).
table_xml_header_rows <- function(cols, header, style_of, header_style,
                                  repeat_header) {
  if (isFALSE(header)) {
    return(character(0))
  }
  hstyle <- function(col) {
    if (!is.null(header_style)) header_style else style_of[[col]]
  }
  if (isTRUE(header)) {
    # single header row from column names
    cells <- vapply(cols, function(cn) {
      table_xml_cell(cn, pstyle = hstyle(cn))
    }, character(1))
    return(table_xml_row(cells, header = repeat_header))
  }
  if (!inherits(header, "table_header")) {
    stop("'header' must be TRUE, FALSE, or a table_header() object",
         call. = FALSE)
  }
  spec <- flatten_table_header(header, cols)
  # group row: one cell per group, spanning its columns
  gcells <- vapply(seq_along(spec$groups), function(k) {
    gcols <- spec$groups[[k]]
    table_xml_cell(names(spec$groups)[k],
      pstyle = hstyle(gcols[1]), gridspan = length(gcols))
  }, character(1))
  grow <- table_xml_row(gcells, header = repeat_header)
  # leaf row: one cell per column
  lcells <- vapply(seq_along(spec$flat), function(j) {
    table_xml_cell(spec$leaf[j], pstyle = hstyle(spec$flat[j]))
  }, character(1))
  lrow <- table_xml_row(lcells, header = repeat_header)
  c(grow, lrow)
}

# internal: body rows, with optional vertical merges and sub-header dividers.
table_xml_body_rows <- function(body, cols, style_of, merge_cols, sub_default) {
  rows <- character(0)
  prev <- vector("list", length(cols))   # last value per merge column
  names(prev) <- cols
  for (item in body) {
    if (inherits(item, "table_subheader")) {
      pstyle <- if (!is.null(item$style) && nzchar(item$style)) item$style else sub_default
      cell <- table_xml_cell(item$label, pstyle = pstyle, gridspan = length(cols))
      rows <- c(rows, table_xml_row(cell, header = FALSE))
      prev[] <- list(NULL)   # a divider resets vertical merges
      next
    }
    if (!is.data.frame(item)) {
      stop("every element of 'value' must be a data.frame or a table_subheader()",
           call. = FALSE)
    }
    # characterise values like officer does, but keep original column names
    d <- characterise_df(item)
    names(d) <- cols
    for (i in seq_len(nrow(d))) {
      cells <- vapply(seq_along(cols), function(j) {
        cn <- cols[j]
        vm <- "none"
        if (cn %in% merge_cols) {
          cur <- d[[cn]][i]
          pv <- prev[[cn]]
          if (is.null(pv) || is.na(cur) || is.na(pv) ||
              !identical(as.character(cur), as.character(pv))) {
            vm <- "restart"
          } else {
            vm <- "continue"
          }
        }
        text <- if (vm == "continue") "" else as.character(d[[cn]][i])
        table_xml_cell(text, pstyle = style_of[[cn]], vmerge = vm)
      }, character(1))
      rows <- c(rows, table_xml_row(cells, header = FALSE))
      for (cn in merge_cols) {
        prev[[cn]] <- d[[cn]][i]
      }
    }
  }
  rows
}


# table_xml ----

#' @export
#' @title Word table markup (native styles + merges)
#' @description Build the WordprocessingML (`<w:tbl>`) for a table that inherits
#' the document template's named table/paragraph styles **and** can express
#' grouped headers, vertical merges and full-width sub-header rows - the
#' combination [block_table()] / [body_add_table()] deliberately cannot express.
#'
#' This is a fork-local helper built on [body_add_xml()] (see upstream issue
#' #731). The returned string uses the same `w:tstlname` / `w:pstlname`
#' placeholder mechanism as officer's built-in table blocks, so the styles are
#' resolved to real style ids when the document is saved.
#' @param value a data.frame, or a list whose elements are data.frames and
#' [table_subheader()] objects. All data.frames must share identical column
#' names in the same order. A list lets you intersperse full-width sub-header
#' divider rows between groups of rows.
#' @param style table style name defined in the document (e.g. `"table_template"`
#' or a corporate template style).
#' @param header `TRUE` (default: one header row from the column names), `FALSE`
#' (no header), or a [table_header()] object (grouped / merged header).
#' @param stylenames paragraph styles for body cells, per column. A named list
#' or named character vector (column name -> style name); columns not listed
#' default to `"Normal"`. A [table_stylenames()] object is also accepted.
#' @param header_style optional single paragraph style applied to every header
#' cell. If `NULL` (default), header cells inherit the per-column `stylenames`,
#' matching [body_add_table()].
#' @param col_widths optional column widths in inches, length equal to the
#' number of columns. When provided, a fixed `<w:tblGrid>` is emitted.
#' @param align table alignment: one of `"left"`, `"center"` (default) or
#' `"right"`.
#' @param merge_consecutive optional character vector of column names whose
#' consecutive identical values are vertically merged (`<w:vMerge>`) in the
#' body. A sub-header row resets the run, so merging restarts after each
#' divider.
#' @param tcf conditional formatting (table look), see
#' [table_conditional_formatting()].
#' @param repeat_header logical, default `TRUE`; mark header rows as repeating
#' on every page (`<w:tblHeader/>`).
#' @return A length-1 character string of WordprocessingML for the table. Inject
#' it with [body_add_xml()] or use [body_add_table_xml()].
#' @examples
#' library(officer)
#'
#' df <- data.frame(g = c("A", "A", "B"), x = c(1, 2, 3), y = c(4, 5, 6))
#'
#' # grouped (merged) header + vertically merged body column
#' wml <- table_xml(
#'   df,
#'   style = "table_template",
#'   header = table_header(list(
#'     Group  = c("g" = "g"),
#'     Values = c("x" = "x", "y" = "y")
#'   )),
#'   merge_consecutive = "g"
#' )
#'
#' # sub-header dividers between groups of rows
#' wml2 <- table_xml(
#'   list(
#'     table_subheader("Category 1"),
#'     df,
#'     table_subheader("Category 2"),
#'     df
#'   ),
#'   style = "table_template",
#'   merge_consecutive = "g"
#' )
#' @family docx native-style table helpers
#' @seealso [body_add_table_xml()], [body_add_xml()], [table_header()],
#'   [table_subheader()]
table_xml <- function(value, style = "table_template", header = TRUE,
                      stylenames = list(), header_style = NULL,
                      col_widths = NULL, align = "center",
                      merge_consecutive = NULL,
                      tcf = table_conditional_formatting(),
                      repeat_header = TRUE) {
  align <- match.arg(align, c("left", "center", "right"))

  # normalise the body to a list and determine the columns from a data.frame
  if (is.data.frame(value)) {
    body <- list(value)
  } else if (is.list(value)) {
    body <- value
  } else {
    stop("'value' must be a data.frame or a list of data.frames / table_subheader()",
         call. = FALSE)
  }
  df_items <- body[vapply(body, is.data.frame, logical(1))]
  if (length(df_items) < 1L) {
    stop("'value' must contain at least one data.frame", call. = FALSE)
  }
  cols <- colnames(df_items[[1]])
  if (length(cols) < 1L) {
    stop("data.frame must have at least one column", call. = FALSE)
  }
  for (d in df_items) {
    if (!identical(colnames(d), cols)) {
      stop("every data.frame in 'value' must have identical column names ",
           "in the same order", call. = FALSE)
    }
  }

  style_of <- normalize_table_stylenames(stylenames, cols)

  header_rows <- table_xml_header_rows(
    cols = cols, header = header, style_of = style_of,
    header_style = header_style, repeat_header = repeat_header
  )

  merge_cols <- intersect(as.character(merge_consecutive), cols)
  sub_default <- if (is.null(header_style)) "Normal" else header_style
  body_rows <- table_xml_body_rows(
    body = body, cols = cols, style_of = style_of,
    merge_cols = merge_cols, sub_default = sub_default
  )

  tblpr <- paste0(
    "<w:tblPr>",
    sprintf('<w:tblStyle w:tstlname="%s"/>', style),
    '<w:tblW w:w="0" w:type="auto"/>',
    sprintf('<w:jc w:val="%s"/>', align),
    to_wml(tcf),
    "</w:tblPr>"
  )

  tblgrid <- ""
  if (length(col_widths) > 0L) {
    if (length(col_widths) != length(cols)) {
      stop("col_widths length must equal the number of columns", call. = FALSE)
    }
    gc <- paste0(
      sprintf('<w:gridCol w:w="%d"/>', as.integer(col_widths * 1440)),
      collapse = ""
    )
    tblgrid <- paste0("<w:tblGrid>", gc, "</w:tblGrid>")
  }

  paste0(
    tbl_ns_yes,
    tblpr, tblgrid,
    paste0(c(header_rows, body_rows), collapse = ""),
    "</w:tbl>"
  )
}


# body_add_table_xml ----

#' @export
#' @title Add a native-style Word table (with merges)
#' @description Convenience wrapper: build the table markup with [table_xml()]
#' and inject it into the document with [body_add_xml()].
#' @param x an `rdocx` object.
#' @param value a data.frame, or a list of data.frames and [table_subheader()]
#' objects (see [table_xml()]).
#' @param ... arguments passed on to [table_xml()] (`style`, `header`,
#' `stylenames`, `header_style`, `col_widths`, `align`, `merge_consecutive`,
#' `tcf`, `repeat_header`).
#' @param pos where to add the table relative to the cursor: one of `"after"`
#' (default), `"before"` or `"on"`.
#' @return The `rdocx` object `x`, invisibly.
#' @examples
#' library(officer)
#'
#' df1 <- data.frame(endpoint = c("A", "A"), n = c(10, 20))
#' df2 <- data.frame(endpoint = c("B", "B"), n = c(5, 7))
#'
#' doc <- read_docx()
#' doc <- body_add_table_xml(
#'   doc,
#'   value = list(
#'     table_subheader("Category 1"),
#'     df1,
#'     table_subheader("Category 2"),
#'     df2
#'   ),
#'   style = "table_template",
#'   merge_consecutive = "endpoint"
#' )
#' print(doc, target = tempfile(fileext = ".docx"))
#' @family docx native-style table helpers
#' @seealso [table_xml()], [body_add_xml()], [table_header()],
#'   [table_subheader()]
body_add_table_xml <- function(x, value, ..., pos = "after") {
  str <- table_xml(value = value, ...)
  body_add_xml(x = x, str = str, pos = pos)
}
