test_that("body_add_table", {
  x <- read_docx()
  x <- body_add_table(x, value = iris, style = "table_template")
  node <- docx_current_block_xml(x)
  expect_equal(xml_name(node), "tbl")
})

test_that("wml structure", {
  x <- read_docx()
  x <- body_add_table(x, value = iris, style = "table_template")
  node <- docx_current_block_xml(x)
  expect_length(xml_find_all(node, xpath = "w:tr[w:trPr/w:tblHeader]"), 1)
  expect_length(
    xml_find_all(node, xpath = "w:tr[not(w:trPr/w:tblHeader)]"),
    nrow(iris)
  )
  expect_length(
    xml_find_all(
      node,
      xpath = "w:tr[not(w:trPr/w:tblHeader)]/w:tc/w:p/w:r/w:t"
    ),
    ncol(iris) * nrow(iris)
  )
})

test_that("block_caption", {
  run_num <- run_autonum(
    seq_id = "tab",
    pre_label = "tab. ",
    bkm = "mtcars_table"
  )

  expect_output(
    print(block_caption("mtcars table", style = "Normal")),
    "caption \\[autonum off\\]: mtcars table"
  )
  expect_output(
    print(block_caption("mtcars table", style = "Normal", autonum = run_num)),
    "caption \\[autonum on\\]: mtcars table"
  )

  caption <- block_caption("mtcars table", autonum = run_num)
  expect_equal(caption$style, "Normal")

  expect_match(
    to_wml(caption, knitting = TRUE),
    "::: \\{custom-style=\"Normal\"\\}"
  )
  expect_match(to_wml(caption, knitting = TRUE), "mtcars table\\n:::")
  expect_match(to_wml(caption, knitting = FALSE), "^<w:p>")
  expect_match(to_wml(caption, knitting = FALSE), "</w:p>$")
  expect_match(to_wml(caption, knitting = FALSE), "mtcars table</w:t>")
})

test_that("names stay as is", {
  df <- data.frame(
    "hello coco" = c(1, 2),
    value = c("одно значение", "еще значение"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  x <- read_docx()
  x <- body_add_table(x, value = df, style = "table_template")
  node <- docx_current_block_xml(x)
  first_text <- xml_find_first(
    node,
    xpath = "w:tr[w:trPr/w:tblHeader]/w:tc/w:p/w:r/w:t"
  )
  first_text <- xml_text(first_text)
  expect_equal(first_text, "hello coco")
})

test_that("table_header validation", {
  expect_error(table_header(list(c("a", "b"))), "named")
  expect_error(
    table_header(list(G = c("a" = "a"), H = c("a" = "a"))),
    "more than one group"
  )
})

test_that("grouped header renders gridSpan and two header rows", {
  df <- data.frame(g = c("A", "B"), x = c(1, 2), y = c(3, 4))
  hdr <- table_header(list(
    Group = c("g" = "g"),
    Values = c("x" = "x", "y" = "y")
  ))
  x <- body_add_table(read_docx(), df, style = "table_template", header = hdr)
  node <- docx_current_block_xml(x)
  # two repeating header rows
  expect_length(xml_find_all(node, "w:tr[w:trPr/w:tblHeader]"), 2)
  # the second group spans two columns -> exactly one gridSpan
  expect_length(xml_find_all(node, ".//w:gridSpan"), 1)
  spans <- xml_attr(xml_find_all(node, ".//w:gridSpan"), "val")
  expect_equal(spans, "2")
  # group labels, then leaf labels
  expect_equal(
    xml_text(xml_find_all(node, "w:tr[1]/w:tc/w:p/w:r/w:t")),
    c("Group", "Values")
  )
  expect_equal(
    xml_text(xml_find_all(node, "w:tr[2]/w:tc/w:p/w:r/w:t")),
    c("g", "x", "y")
  )
})

test_that("merge_consecutive vertically merges identical runs", {
  df <- data.frame(
    g = c("A", "A", "B", "B"),
    x = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  x <- body_add_table(read_docx(), df, style = "table_template",
                      header = FALSE, merge_consecutive = "g")
  node <- docx_current_block_xml(x)
  expect_length(xml_find_all(node, ".//w:vMerge[@w:val=\"restart\"]"), 2)
  expect_length(xml_find_all(node, ".//w:vMerge[not(@w:val)]"), 2)
  # non-merged column is unaffected (no vMerge on x cells)
  expect_length(
    xml_find_all(node, "w:tr[1]/w:tc[2]//w:vMerge"),
    0
  )
})

test_that("grouped header + merge_consecutive together", {
  df <- data.frame(
    g = c("A", "A", "B"), x = c(1, 2, 3), y = c(4, 5, 6),
    stringsAsFactors = FALSE
  )
  hdr <- table_header(list(Group = c("g" = "g"), Values = c("x" = "x", "y" = "y")))
  x <- body_add_table(read_docx(), df, style = "table_template",
                      header = hdr, merge_consecutive = "g")
  node <- docx_current_block_xml(x)
  expect_length(xml_find_all(node, "w:tr[w:trPr/w:tblHeader]"), 2)
  expect_length(xml_find_all(node, ".//w:gridSpan"), 1)
  expect_length(xml_find_all(node, ".//w:vMerge[@w:val=\"restart\"]"), 2)
  expect_length(xml_find_all(node, ".//w:vMerge[not(@w:val)]"), 1)
})
