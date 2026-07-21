# Tests for the fork-local native-style table helpers (see R/docx_table_xml.R).
# These are built on body_add_xml() and the w:tstlname / w:pstlname placeholder
# mechanism that officer resolves on save (see convert_custom_styles_in_wml()).

# -- table_xml() returns a <w:tbl> WML string with the expected markup --------

test_that("table_xml() simple header renders one header row and body cells", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  wml <- table_xml(df, style = "table_template")

  expect_true(grepl("^<w:tbl ", wml))
  expect_true(grepl("</w:tbl>$", wml))
  # table style is referenced via the placeholder (resolved on save)
  expect_match(wml, '<w:tblStyle w:tstlname="table_template"/>', fixed = TRUE)
  # body cells carry per-column paragraph-style placeholders
  expect_match(wml, '<w:pStyle w:pstlname="Normal"/>', fixed = TRUE)
  # exactly one repeating header row, plus one body row per data row
  expect_length(gregexpr("<w:tblHeader/>", wml, fixed = TRUE)[[1]], 1L)
  expect_length(regmatches(wml, gregexpr("<w:tr>", wml))[[1]], 1L + nrow(df))
})

test_that("table_xml() grouped header emits gridSpan group cells", {
  df <- data.frame(
    treatment = c("A"), dose = c(1), resp = c(2), orr = c(3)
  )
  hdr <- table_header(list(
    Treatment = c("Agent" = "treatment", "Dose" = "dose"),
    Efficacy  = c("n" = "resp", "ORR" = "orr")
  ))
  wml <- table_xml(df, style = "table_template", header = hdr)

  # two header rows (group row + leaf row), both repeating
  expect_length(
    regmatches(wml, gregexpr("<w:tblHeader/>", wml, fixed = TRUE))[[1]],
    2L
  )
  # group cells span their columns
  expect_match(wml, '<w:gridSpan w:val="2"/>', fixed = TRUE)
  # group labels and leaf labels are present
  expect_match(wml, ">Treatment<", fixed = TRUE)
  expect_match(wml, ">Efficacy<", fixed = TRUE)
  expect_match(wml, ">Agent<", fixed = TRUE)   # overridden leaf label
})

test_that("table_xml() header=FALSE renders no header row", {
  df <- data.frame(a = c(1, 2))
  wml <- table_xml(df, style = "table_template", header = FALSE)
  expect_length(
    regmatches(wml, gregexpr("<w:tblHeader/>", wml, fixed = TRUE))[[1]],
    0L
  )
})

test_that("table_xml() merge_consecutive vertically merges runs", {
  df <- data.frame(g = c("A", "A", "B"), x = c(1, 2, 3))
  wml <- table_xml(df, style = "table_template", merge_consecutive = "g")

  # restart at first A and at B; continue on second A
  expect_length(
    regmatches(wml, gregexpr('<w:vMerge w:val="restart"/>', wml, fixed = TRUE))[[1]],
    2L
  )
  expect_length(
    regmatches(wml, gregexpr("<w:vMerge/>", wml, fixed = TRUE))[[1]],
    1L
  )
})

test_that("table_xml() accepts a table_stylenames() object", {
  df <- data.frame(x = 1, y = 2)
  sn <- table_stylenames(list(centered = c("x", "y")))
  wml <- table_xml(df, style = "table_template", stylenames = sn)
  expect_match(wml, '<w:pStyle w:pstlname="centered"/>', fixed = TRUE)
})

test_that("table_xml() rejects mismatched table_header columns", {
  df <- data.frame(a = 1, b = 2)
  hdr <- table_header(list(G = c("a" = "a", "b" = "b", "c" = "c")))
  expect_error(
    table_xml(df, style = "table_template", header = hdr),
    "must match the data.frame columns"
  )
})

# -- sub-header dividers ------------------------------------------------------

test_that("table_subheader() rows span all columns and reset vertical merges", {
  df1 <- data.frame(g = c("A", "A"), x = c(1, 2))
  df2 <- data.frame(g = c("A", "A"), x = c(3, 4)) # same 'g' value, across divider
  wml <- table_xml(
    list(table_subheader("Cat 1"), df1,
         table_subheader("Cat 2"), df2),
    style = "table_template",
    merge_consecutive = "g"
  )

  # two divider rows, each a single cell spanning all (2) columns
  expect_match(wml, '<w:gridSpan w:val="2"/>', fixed = TRUE)
  expect_match(wml, ">Cat 1<", fixed = TRUE)
  expect_match(wml, ">Cat 2<", fixed = TRUE)

  # the divider resets merges: each group restarts once (2 restarts, 2 continues),
  # NOT one long merged run (which would be 1 restart, 3 continues)
  expect_length(
    regmatches(wml, gregexpr('<w:vMerge w:val="restart"/>', wml, fixed = TRUE))[[1]],
    2L
  )
  expect_length(
    regmatches(wml, gregexpr("<w:vMerge/>", wml, fixed = TRUE))[[1]],
    2L
  )
})

test_that("table_subheader() style falls back to header_style", {
  df <- data.frame(a = 1)
  wml <- table_xml(
    list(table_subheader("Banner"), df),
    style = "table_template",
    header_style = "MyHeaderStyle"
  )
  expect_match(wml, '<w:pStyle w:pstlname="MyHeaderStyle"/>', fixed = TRUE)
})

test_that("table_xml() rejects data.frames with differing columns in a list", {
  expect_error(
    table_xml(
      list(data.frame(a = 1), data.frame(b = 1)),
      style = "table_template"
    ),
    "identical column names"
  )
})

# -- body_add_table_xml() integration + save-time style resolution -----------

test_that("body_add_table_xml() adds a tbl node with the expected structure", {
  x <- read_docx()
  x <- body_add_table_xml(x, iris[1:3, ], style = "table_template")
  node <- docx_current_block_xml(x)

  expect_equal(xml2::xml_name(node), "tbl")
  # one repeating header row, three body rows
  expect_length(xml2::xml_find_all(node, "w:tr[w:trPr/w:tblHeader]"), 1L)
  expect_length(
    xml2::xml_find_all(node, "w:tr[not(w:trPr/w:tblHeader)]"),
    3L
  )
})

test_that("placeholders are resolved and the file round-trips on save", {
  df <- data.frame(g = c("A", "A", "B"), x = c(1, 2, 3), y = c(4, 5, 6))
  x <- read_docx()
  x <- body_add_table_xml(
    x, df, style = "table_template",
    header = table_header(list(
      Group = c("g" = "g"), Values = c("x" = "x", "y" = "y")
    )),
    merge_consecutive = "g"
  )

  tmp <- tempfile(fileext = ".docx")
  print(x, target = tmp)

  # unzip and read the saved document.xml
  td <- tempfile(); dir.create(td)
  utils::unzip(tmp, exdir = td)
  doc_xml <- paste0(
    readLines(file.path(td, "word/document.xml"), warn = FALSE),
    collapse = ""
  )

  # style placeholders have all been resolved to real style ids
  expect_false(grepl("tstlname|pstlname", doc_xml))
  expect_match(doc_xml, '<w:tblStyle w:val="[^"]+"/>')
  # merges and grouped header survived save
  expect_match(doc_xml, '<w:gridSpan w:val="2"/>', fixed = TRUE)
  expect_match(doc_xml, '<w:vMerge w:val="restart"/>', fixed = TRUE)

  # the produced file can be read back by officer
  expect_no_error(read_docx(tmp))
})
