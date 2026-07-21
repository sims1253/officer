test_that("settings works", {
  x <- read_docx()
  x <- docx_set_settings(
    x = x,
    zoom = 1,
    default_tab_stop = .5,
    hyphenation_zone = .25,
    decimal_symbol = ".",
    list_separator = ";",
    compatibility_mode = "15",
    even_and_odd_headers = TRUE,
    auto_hyphenation = FALSE
  )
  file <- print(x, target = tempfile(fileext = ".docx"))
  x <- read_docx(path = file)
  expect_equal(x$settings$zoom, 1)
  expect_equal(x$settings$list_separator, ";")
  expect_true(x$settings$even_and_odd_headers)
})

test_that("settings preserves existing XML elements", {
  template <- system.file(
    "doc_examples",
    "example.docx",
    package = "officer"
  )
  x <- read_docx(template)

  # read original settings.xml
  settings_before <- xml2::read_xml(
    file.path(x$package_dir, "word", "settings.xml")
  )
  tags_before <- xml2::xml_name(xml2::xml_children(settings_before))

  x <- docx_set_settings(x, zoom = 2)
  file <- print(x, target = tempfile(fileext = ".docx"))

  unpack_dir <- tempfile()
  unpack_folder(file, unpack_dir)
  settings_after <- xml2::read_xml(
    file.path(unpack_dir, "word", "settings.xml")
  )
  tags_after <- xml2::xml_name(xml2::xml_children(settings_after))

  # all original tags should still be present
  expect_true(all(tags_before %in% tags_after))

  # zoom should be updated
  zoom_node <- xml2::xml_child(settings_after, "w:zoom")
  expect_equal(xml2::xml_attr(zoom_node, "percent"), "200")
})

test_that("docx_embed_font embeds font files", {
  gdtools::register_liberationsans()
  sysfonts <- gdtools::sys_fonts()
  libsans <- sysfonts[sysfonts$family == "Liberation Sans", ]

  x <- read_docx()
  x <- docx_embed_font(
    x,
    font_family = "Liberation Sans",
    regular = libsans$path[libsans$style == "Regular"],
    bold = libsans$path[libsans$style == "Bold"]
  )
  file <- print(x, target = tempfile(fileext = ".docx"))

  unpack_dir <- tempfile()
  unpack_folder(file, unpack_dir)

  # odttf files created
  odttfs <- list.files(
    file.path(unpack_dir, "word", "fonts"),
    pattern = "[.]odttf$"
  )
  expect_equal(length(odttfs), 2L)

  # fontTable.xml has embed entries
  ft <- xml2::read_xml(
    file.path(unpack_dir, "word", "fontTable.xml")
  )
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  font_node <- xml2::xml_find_first(
    ft,
    "w:font[@w:name='Liberation Sans']",
    ns = ns
  )
  expect_false(inherits(font_node, "xml_missing"))
  embed_reg <- xml2::xml_child(font_node, "w:embedRegular")
  embed_bold <- xml2::xml_child(font_node, "w:embedBold")
  expect_false(inherits(embed_reg, "xml_missing"))
  expect_false(inherits(embed_bold, "xml_missing"))

  # fontTable.xml.rels has relationships
  rels <- xml2::read_xml(
    file.path(unpack_dir, "word", "_rels", "fontTable.xml.rels")
  )
  rel_nodes <- xml2::xml_find_all(
    rels,
    "d1:Relationship",
    ns = xml2::xml_ns(rels)
  )
  expect_equal(length(rel_nodes), 2L)

  # settings.xml has embedTrueTypeFonts
  settings <- xml2::read_xml(
    file.path(unpack_dir, "word", "settings.xml")
  )
  embed_node <- xml2::xml_child(settings, "w:embedTrueTypeFonts")
  expect_false(inherits(embed_node, "xml_missing"))
})

test_that("docx_embed_font auto-detects font files", {
  gdtools::register_liberationsans()
  x <- read_docx()
  expect_message(
    x <- docx_embed_font(x, font_family = "Liberation Sans"),
    "Liberation Sans"
  )
  file <- print(x, target = tempfile(fileext = ".docx"))
  unpack_dir <- tempfile()
  unpack_folder(file, unpack_dir)

  odttfs <- list.files(
    file.path(unpack_dir, "word", "fonts"),
    pattern = "[.]odttf$"
  )
  expect_gte(length(odttfs), 1L)

  # silent mode
  x2 <- read_docx()
  expect_no_message(
    suppressMessages(
      docx_embed_font(x2, font_family = "Liberation Sans")
    )
  )
})

test_that("docx_embed_font auto-detect errors on unknown family", {
  x <- read_docx()
  expect_error(
    docx_embed_font(x, font_family = "NonExistentFont12345"),
    "not found"
  )
})

test_that("docx_embed_font validates inputs", {
  x <- read_docx()
  expect_error(
    docx_embed_font(x, "Test", regular = "nonexistent.ttf"),
    "does not exist"
  )
  expect_error(
    docx_embed_font("not_rdocx", "Test", regular = tempfile()),
    "rdocx"
  )
})

# settings.xml roundtrip fidelity (#730, round 4) ---------------------------
# Helpers for building a docx whose settings.xml has been patched. Mirrors the
# unzip / string-substitute / `zip::zip(..., root = dir)` pattern used by
# `build_unmanaged_media_docx()` in test-images.R.
patch_settings_docx <- function(patch_fun) {
  base <- tempfile(fileext = ".docx")
  print(read_docx(), target = base)
  dir <- tempfile()
  unzip(base, exdir = dir)

  settings_f <- file.path(dir, "word", "settings.xml")
  settings_xml <- readLines(settings_f, warn = FALSE)
  settings_xml <- patch_fun(paste(settings_xml, collapse = "\n"))
  writeLines(settings_xml, settings_f, useBytes = TRUE)

  src <- tempfile(fileext = ".docx")
  old_wd <- getwd()
  setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  zip::zip(
    zipfile = src,
    files = list.files(dir, all.files = TRUE, no.. = TRUE),
    root = dir
  )
  src
}

settings_xml_from <- function(docx_file) {
  unpack_dir <- tempfile()
  unpack_folder(file = docx_file, folder = unpack_dir)
  xml2::read_xml(file.path(unpack_dir, "word", "settings.xml"))
}

test_that("evenAndOddHeaders stays absent across double roundtrip", {
  doc <- read_docx()
  f1 <- tempfile(fileext = ".docx")
  print(doc, target = f1)

  f2 <- tempfile(fileext = ".docx")
  print(read_docx(f1), target = f2)

  # no w:evenAndOddHeaders element in either save
  for (f in c(f1, f2)) {
    settings <- settings_xml_from(f)
    node <- xml2::xml_child(settings, "w:evenAndOddHeaders")
    expect_true(inherits(node, "xml_missing"))
  }

  # the reader reports FALSE after reading each save
  expect_false(read_docx(f1)$settings$even_and_odd_headers)
  expect_false(read_docx(f2)$settings$even_and_odd_headers)
})

test_that("evenAndOddHeaders val=0 reads as FALSE", {
  src <- patch_settings_docx(function(xml) {
    # inject a present element with an explicit w:val="0" right after the
    # opening <w:settings ...> tag.
    sub("(<w:settings[^>]*>)", "\\1<w:evenAndOddHeaders w:val=\"0\"/>", xml)
  })

  # reader reports FALSE despite the element being present
  doc <- read_docx(src)
  expect_false(doc$settings$even_and_odd_headers)

  # a save must not flip it to present-TRUE / val="1"; the writer removes it
  out <- tempfile(fileext = ".docx")
  print(doc, target = out)
  settings <- settings_xml_from(out)
  node <- xml2::xml_child(settings, "w:evenAndOddHeaders")
  expect_true(inherits(node, "xml_missing"))
})

test_that("even_and_odd_headers=TRUE writes element and survives roundtrip", {
  doc <- docx_set_settings(read_docx(), even_and_odd_headers = TRUE)
  f1 <- tempfile(fileext = ".docx")
  print(doc, target = f1)

  # element is present, with no w:val attribute (TRUE by OOXML semantics)
  settings <- settings_xml_from(f1)
  node <- xml2::xml_child(settings, "w:evenAndOddHeaders")
  expect_false(inherits(node, "xml_missing"))
  expect_true(is.na(xml2::xml_attr(node, "val")))

  # re-read reports TRUE and a second save keeps it TRUE
  doc2 <- read_docx(f1)
  expect_true(doc2$settings$even_and_odd_headers)

  f2 <- tempfile(fileext = ".docx")
  print(doc2, target = f2)
  settings2 <- settings_xml_from(f2)
  node2 <- xml2::xml_child(settings2, "w:evenAndOddHeaders")
  expect_false(inherits(node2, "xml_missing"))
  expect_true(read_docx(f2)$settings$even_and_odd_headers)
})

test_that("compat settings are preserved on save", {
  src <- patch_settings_docx(function(xml) {
    # replace any existing <w:compat ...>...</w:compat> block with a controlled
    # block: compatibilityMode plus two extra compatSettings. The perl regex
    # (\b excludes <w:compatSetting>, (?s) lets . cross newlines, [^>]* allows
    # attributes such as an inline xmlns:w on the opening tag).
    block <- paste0(
      "<w:compat>",
      "<w:compatSetting w:name=\"compatibilityMode\"",
      " w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"14\"/>",
      "<w:compatSetting w:name=\"overrideTableStyleFontSizeAndJustification\"",
      " w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"1\"/>",
      "<w:compatSetting w:name=\"enableOpenTypeFeatures\"",
      " w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"1\"/>",
      "</w:compat>"
    )
    block_re <- "(?s)<w:compat\\b.*?</w:compat>"
    if (grepl(block_re, xml, perl = TRUE)) {
      sub(block_re, block, xml, perl = TRUE)
    } else {
      sub("</w:settings>", paste0(block, "</w:settings>"), xml)
    }
  })

  # the reader must report the patched (non-default) value, exercising the
  # reader/writer symmetry for compatibility_mode ("15" is also the default,
  # so the old fixture could pass even if reading were a no-op).
  doc <- read_docx(src)
  expect_equal(doc$settings$compatibility_mode, "14")

  out <- tempfile(fileext = ".docx")
  print(doc, target = out)

  settings <- settings_xml_from(out)
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  names_after <- xml2::xml_attr(
    xml2::xml_find_all(settings, "w:compat/w:compatSetting", ns = ns),
    "name"
  )
  expect_setequal(
    names_after,
    c(
      "compatibilityMode",
      "overrideTableStyleFontSizeAndJustification",
      "enableOpenTypeFeatures"
    )
  )

  # compatibilityMode val is unchanged through the save
  cm <- xml2::xml_find_first(
    settings,
    "w:compat/w:compatSetting[@w:name='compatibilityMode']",
    ns = ns
  )
  expect_equal(xml2::xml_attr(cm, "val"), "14")
})
