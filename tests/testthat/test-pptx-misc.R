test_that("defaul template", {
  x <- read_pptx()
  expect_equal(length(x), 0)
  expect_true(file.exists(x$package_dir))
})


test_that("console printing", {
  opts <- options(cli.num_colors = 1) # suppress colors for error message check
  on.exit(options(opts))

  x <- read_pptx()
  layouts <- layout_summary(x)$layout

  # 1 slide
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world", location = ph_location_type(type = "body"))
  output_print <- capture.output({
    # capture layout dataframe
    output_cli <- cli::cli_fmt(print(x)) # capture cli info lines
  })
  expect_true(any(grepl("pptx document with 1 slide", output_cli)))
  output_shows_layout <- vapply(
    layouts,
    function(l) any(grepl(l, output_print)),
    logical(1)
  )
  expect_true(all(output_shows_layout))

  # 2 slides
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world", location = ph_location_type(type = "body"))
  output_print <- capture.output({
    output_cli <- cli::cli_fmt(print(x))
  })
  expect_true(any(grepl("pptx document with 2 slides", output_cli)))

  # 2 slides + default layout
  x <- layout_default(x, "Title and Content")
  output_print <- capture.output({
    output_cli <- cli::cli_fmt(print(x))
  })
  expect_true(any(grepl("(*) = Default layout", output_cli)))
  output_shows_layout <- vapply(
    layouts,
    function(l) any(grepl(l, output_print)),
    logical(1)
  )
  expect_true(all(output_shows_layout))

  # default layout removed again
  x <- layout_default(x, NULL)
  output_print <- capture.output({
    output_cli <- cli::cli_fmt(print(x))
  })
  expect_false(any(grepl("(*) = Default layout", output_cli)))
})


test_that("check extention and print document", {
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world", location = ph_location_type(type = "body"))
  print(x, target = "print.pptx")
  expect_true(file.exists("print.pptx"))

  expect_error(print(x, target = "print.pptxxx"))
})


test_that("check template", {
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world", location = ph_location_type(type = "body"))
  print(x, target = "template.pptx")

  expect_silent(x <- read_pptx(path = "template.pptx"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(
    x,
    "Hello world again",
    location = ph_location_type(type = "body")
  )
  print(x, target = "example.pptx")

  expect_equal(length(x), 2)
})


test_that("slide remove", {
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world 1", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world 2", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world 3", location = ph_location_type(type = "body"))
  print(x, target = "template.pptx")

  x <- read_pptx(path = "template.pptx")
  x <- remove_slide(x = x)
  expect_equal(length(x), 2)
  x <- remove_slide(x = x, index = 1)
  expect_equal(length(x), 1)

  sm <- slide_summary(x)
  expect_equal(sm[1, ]$text, "Hello world 2")
})

test_that("remove multiple slides at once", {
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 1", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 2", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 3", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 4", location = ph_location_type(type = "body"))

  # Remove slides 2 and 4
  x <- remove_slide(x, index = c(2, 4))
  expect_equal(length(x), 2)

  # Check remaining slides are 1 and 3
  sm1 <- slide_summary(x, index = 1)
  sm2 <- slide_summary(x, index = 2)
  expect_equal(sm1[1, ]$text, "Slide 1")
  expect_equal(sm2[1, ]$text, "Slide 3")
})

test_that("remove multiple slides - edge cases", {
  # Test removing all slides
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- remove_slide(x, index = c(1, 2))
  expect_equal(length(x), 0)

  # Test with duplicate indices (should only remove once)
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 1", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 2", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 3", location = ph_location_type(type = "body"))
  x <- remove_slide(x, index = c(2, 2, 2))
  expect_equal(length(x), 2)
  sm <- slide_summary(x, index = 2)
  expect_equal(sm[1, ]$text, "Slide 3")

  # Test with empty index vector (should return unchanged)
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- remove_slide(x, index = integer(0))
  expect_equal(length(x), 2)

  # Test removing slides in non-sequential order
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 1", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 2", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 3", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Slide 4", location = ph_location_type(type = "body"))
  x <- remove_slide(x, index = c(4, 1, 3))
  expect_equal(length(x), 1)
  sm <- slide_summary(x, index = 1)
  expect_equal(sm[1, ]$text, "Slide 2")
})

test_that("remove multiple slides - invalid indices", {
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- add_slide(x, "Title and Content", "Office Theme")

  # Test with out of range indices
  expect_error(remove_slide(x, index = c(1, 5)), "out of range")
  expect_error(remove_slide(x, index = c(0, 1)), "out of range")
  expect_error(remove_slide(x, index = c(-1, 2)), "out of range")

  # Test with all invalid indices
  expect_error(remove_slide(x, index = c(5, 6)), "out of range")
})

test_that("ph remove", {
  x <- read_pptx()
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world 1", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world 2", location = ph_location_type(type = "body"))
  x <- add_slide(x, "Title and Content", "Office Theme")
  x <- ph_with(x, "Hello world 3", location = ph_location_type(type = "body"))
  print(x, target = "template.pptx")

  x <- read_pptx(path = "template.pptx")
  x <- ph_remove(x = x)

  expect_equal(nrow(slide_summary(x)), 0)
})


test_that("cursor is incremented as expected", {
  x <- read_pptx()
  for (i in 1:11) {
    x <- add_slide(x, "Title Slide", "Office Theme")
    x <- ph_with(x, i, location = ph_location_type(type = "ctrTitle"))
  }
  expect_equal(nrow(slide_summary(x, 11)), 1)
  expect_equal(x$slide$get_slide(11)$name(), "slide11.xml")
})

test_that("annotate base template", {
  expect_s3_class(try(annotate_base(), silent = TRUE), "rpptx")
})

test_that("no master do not generate an error", {
  x <- read_pptx("docs_dir/no_master.pptx")
  x <- add_slide(x, layout = "Page One", master = "Office Theme")
  x <- try(
    ph_with(
      x,
      i,
      value = "graphic title",
      location = ph_location_type(type = "body")
    ),
    silent = TRUE
  )
  expect_s3_class(x, "rpptx")
})


unlink("*.pptx")
unlink("*.emf")


test_that("slide_visible", {
  opts <- options(cli.num_colors = 1) # suppress colors for error message check
  on.exit(options(opts))

  x <- read_pptx()
  expect_equal(slide_visible(x), logical(0)) # works with 0 slides

  path <- testthat::test_path("docs_dir", "test-slides-visible.pptx")
  x <- read_pptx(path)

  expect_equal(slide_visible(x), c(FALSE, TRUE, FALSE))
  x <- slide_visible(x, hide = 1:2)
  expect_s3_class(x, "rpptx")
  expect_equal(slide_visible(x), c(FALSE, FALSE, FALSE))
  x <- slide_visible(x, show = 1:2)
  expect_s3_class(x, "rpptx")
  expect_equal(slide_visible(x), c(TRUE, TRUE, FALSE))
  x <- slide_visible(x, hide = 1:2, show = 3)
  expect_s3_class(x, "rpptx")
  expect_equal(slide_visible(x), c(FALSE, FALSE, TRUE))

  expect_error(
    regex = "Overlap between indexes in `hide` and `show`",
    slide_visible(x, hide = 1:2, show = 1:2)
  )
  expect_error(
    regex = "2 indexes of `hide` outside slide range",
    slide_visible(x, hide = 1:5)
  )
  expect_error(
    regex = "1 index of `show` outside slide range",
    slide_visible(x, show = -1)
  )

  slide_visible(x) <- FALSE # hide all slides
  expect_false(any(slide_visible(x)))
  slide_visible(x) <- c(TRUE, FALSE, TRUE)
  expect_equal(slide_visible(x), c(TRUE, FALSE, TRUE))
  expect_warning(
    regexp = "Value is not length 1 or same length as number of slides",
    slide_visible(x) <- c(TRUE, FALSE) # warns that rhs values are recycled
  )
  expect_equal(slide_visible(x), c(TRUE, FALSE, TRUE))

  slide_visible(x)[2] <- TRUE
  expect_equal(slide_visible(x), c(TRUE, TRUE, TRUE))
  slide_visible(x)[c(1, 3)] <- FALSE
  expect_equal(slide_visible(x), c(FALSE, TRUE, FALSE))

  slide_visible(x) <- TRUE
  expect_warning(
    regexp = "number of items to replace is not a multiple of replacement length",
    slide_visible(x)[c(1, 2)] <- rep(FALSE, 4)
  )
  expect_equal(slide_visible(x), c(FALSE, FALSE, TRUE))

  expect_error(
    {
      slide_visible(x) <- rep(FALSE, 4)
    },
    regexp = "More values \\(4\\) than slides \\(3\\)"
  )

  # test that changes are written to file
  path <- testthat::test_path("docs_dir", "test-slides-visible.pptx")
  x <- read_pptx(path)

  slide_visible(x) <- FALSE
  path <- tempfile(fileext = ".pptx")
  print(x, path)
  x <- read_pptx(path)
  expect_equal(slide_visible(x), c(FALSE, FALSE, FALSE))

  slide_visible(x)[c(1, 3)] <- TRUE
  path <- tempfile(fileext = ".pptx")
  print(x, path)
  x <- read_pptx(path)
  expect_equal(slide_visible(x), c(TRUE, FALSE, TRUE))
})

# sanitize_images (pptx branch): keep media referenced by any .rels, drop
# orphan media (issue #730, round 5) ---------------------------------------
test_that("pptx sanitize_images keeps referenced media and drops orphans", {
  # build a minimal pptx that has slide1, then unzip it
  base <- tempfile(fileext = ".pptx")
  x0 <- read_pptx()
  x0 <- add_slide(x0, "Title and Content", "Office Theme")
  print(x0, target = base)
  dir <- tempfile()
  unzip(base, exdir = dir)

  media_dir <- file.path(dir, "ppt", "media")
  dir.create(media_dir, recursive = TRUE, showWarnings = FALSE)

  # used.gif: referenced from slide1.xml.rels -> must survive
  writeBin(charToRaw("GIF89a"), file.path(media_dir, "used.gif"))
  # used 2.gif: referenced from slide1.xml.rels via an encoded space
  # ("../media/used%202.gif") -> must survive.
  writeBin(charToRaw("GIF89a"), file.path(media_dir, "used 2.gif"))
  # used3.gif: referenced from presentation.xml.rels (a part directly under
  # ppt/) as "media/used3.gif" -> must survive.
  writeBin(charToRaw("GIF89a"), file.path(media_dir, "used3.gif"))
  # orphan.bmp: not referenced by any .rels -> must be deleted. Content is
  # not validated, a small binary blob is enough (a plausible BMP header).
  writeBin(
    as.raw(c(0x42, 0x4d, 0x00, 0x00)),
    file.path(media_dir, "orphan.bmp")
  )

  # register both image content types if not already present
  ct_f <- file.path(dir, "[Content_Types].xml")
  ct <- paste(readLines(ct_f, warn = FALSE), collapse = "\n")
  if (!grepl('Extension="gif"', ct, fixed = TRUE)) {
    ct <- sub("<Override",
      '<Default Extension="gif" ContentType="image/gif"/><Override', ct)
  }
  if (!grepl('Extension="bmp"', ct, fixed = TRUE)) {
    ct <- sub("<Override",
      '<Default Extension="bmp" ContentType="image/bmp"/><Override', ct)
  }
  writeLines(ct, ct_f, useBytes = TRUE)

  # reference used.gif and used%202.gif from slide1's rels (the slide XML
  # need not display them; the pptx branch of sanitize_images keeps anything
  # referenced by any .rels). Use fresh rIds to avoid colliding with the
  # slideLayout rel.
  s1rels_f <- file.path(dir, "ppt", "slides", "_rels", "slide1.xml.rels")
  s1rels <- paste(readLines(s1rels_f, warn = FALSE), collapse = "\n")
  existing <- regmatches(s1rels, gregexpr('Id="rId[0-9]+"', s1rels))[[1L]]
  existing_nums <- as.integer(sub('Id="rId([0-9]+)"', '\\1', existing))
  img_type <- "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
  new_rid <- sprintf("rId%d", max(existing_nums, 0L) + 1L)
  new_rid2 <- sprintf("rId%d", max(existing_nums, 0L) + 2L)
  s1rels <- sub("</Relationships>",
    paste0(
      '<Relationship Id="', new_rid,
      '" Type="', img_type, '" Target="../media/used.gif"/>',
      '<Relationship Id="', new_rid2,
      '" Type="', img_type, '" Target="../media/used%202.gif"/>',
      '</Relationships>'
    ),
    s1rels)
  writeLines(s1rels, s1rels_f, useBytes = TRUE)

  # reference used3.gif from presentation.xml.rels. presentation.xml lives
  # directly under ppt/, so its rels file is ppt/_rels/presentation.xml.rels
  # and its image target is relative to ppt/ ("media/used3.gif").
  pres_rels_f <- file.path(dir, "ppt", "_rels", "presentation.xml.rels")
  pres_rels <- paste(readLines(pres_rels_f, warn = FALSE), collapse = "\n")
  existing_pres <- regmatches(pres_rels, gregexpr('Id="rId[0-9]+"', pres_rels))[[1L]]
  existing_pres_nums <- as.integer(sub('Id="rId([0-9]+)"', '\\1', existing_pres))
  new_rid3 <- sprintf("rId%d", max(existing_pres_nums, 0L) + 1L)
  pres_rels <- sub("</Relationships>",
    paste0(
      '<Relationship Id="', new_rid3,
      '" Type="', img_type, '" Target="media/used3.gif"/>',
      '</Relationships>'
    ),
    pres_rels)
  writeLines(pres_rels, pres_rels_f, useBytes = TRUE)

  # re-zip
  patched <- tempfile(fileext = ".pptx")
  old_wd <- getwd()
  setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  zip::zip(
    zipfile = patched,
    files = list.files(dir, all.files = TRUE, no.. = TRUE),
    root = dir
  )
  setwd(old_wd)

  # read + print runs sanitize_images() (R/read_pptx.R:175) on the OUTPUT
  # package, so assert on the printed file.
  out <- tempfile(fileext = ".pptx")
  print(read_pptx(patched), target = out)

  dir_out <- tempfile()
  unzip(out, exdir = dir_out)

  # referenced media kept, orphan deleted
  expect_true(file.exists(file.path(dir_out, "ppt", "media", "used.gif")))
  expect_true(file.exists(file.path(dir_out, "ppt", "media", "used 2.gif")))
  expect_true(file.exists(file.path(dir_out, "ppt", "media", "used3.gif")))
  expect_false(file.exists(file.path(dir_out, "ppt", "media", "orphan.bmp")))

  # the referenced image relationship survives in slide1's rels
  rels_out <- paste(readLines(
    file.path(dir_out, "ppt", "slides", "_rels", "slide1.xml.rels"),
    warn = FALSE
  ), collapse = "\n")
  expect_true(grepl("used.gif", rels_out, fixed = TRUE))

  # the saved file must still be readable
  expect_no_error(read_pptx(out))
})
