#' @export
#' @title transform an xml string with images references
#' @description This function is useless now, as the processing of images
#' is automated when using [print.rdocx()].
#' @param x an rdocx object
#' @param str wml string
#' @family functions for officer extensions
#' @keywords internal
wml_link_images <- function(x, str) {
  str
}

# by capturing the path, we are making 'unique' new image names.
#' @importFrom openssl sha1
fake_newname <- function(filename) {
  which_files <- grepl("\\.[a-zA-Z0-9]+$", filename)
  file_type <- gsub("(.*)(\\.[a-zA-Z0-9]+)$", "\\2", filename[which_files])
  dest_basename <- sapply(filename[which_files], function(z) {
    as.character(sha1(file(z)))
  })
  dest_basename <- paste0(dest_basename, file_type)
  x <- filename
  x[which_files] <- dest_basename
  x
}

process_images <- function(
  doc_obj,
  relationships,
  package_dir,
  media_dir = "word/media",
  media_rel_dir = "media"
) {
  hl_nodes <- xml_find_all(
    doc_obj$get(),
    "//a:blip[@r:embed]|//asvg:svgBlip[@r:embed]",
    ns = c(
      "a" = "http://schemas.openxmlformats.org/drawingml/2006/main",
      "asvg" = "http://schemas.microsoft.com/office/drawing/2016/SVG/main",
      "r" = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    )
  )
  which_to_add <- hl_nodes[!grepl("^rId[0-9]+$", xml_attr(hl_nodes, "embed"))]
  hl_ref <- unique(xml_attr(which_to_add, "embed"))
  for (i in seq_along(hl_ref)) {
    dest_basename <- fake_newname(hl_ref[i])
    img_path <- file.path(package_dir, media_dir)
    if (!file.exists(file.path(img_path, dest_basename))) {
      dir.create(img_path, recursive = TRUE, showWarnings = FALSE)
      file.copy(from = hl_ref[i], to = file.path(img_path, dest_basename))
    }
    if (
      !file.path(media_rel_dir, dest_basename) %in%
        relationships$get_data()$target
    ) {
      rid <- sprintf("rId%.0f", relationships$get_next_id())
      relationships$add(
        id = rid,
        type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
        target = file.path(media_rel_dir, dest_basename)
      )
    } else {
      reldf <- relationships$get_data()
      rid <- reldf$id[basename(reldf$target) %in% dest_basename]
    }
    which_match_id <- grepl(
      dest_basename,
      fake_newname(xml_attr(which_to_add, "embed")),
      fixed = TRUE
    )
    xml_attr(which_to_add[which_match_id], "r:embed") <- rep(
      rid,
      sum(which_match_id)
    )
  }
}

process_docx_poured <- function(
  doc_obj,
  relationships,
  content_type,
  package_dir,
  media_dir = "word"
) {
  hl_nodes <- xml_find_all(
    doc_obj$get(),
    "//w:altChunk[@r:id]",
    ns = c(
      "w" = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
      "r" = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    )
  )

  which_to_add <- hl_nodes[!grepl("^rId[0-9]+$", xml_attr(hl_nodes, "id"))]
  hl_ref <- unique(xml_attr(which_to_add, "id"))
  for (i in seq_along(hl_ref)) {
    rid <- sprintf("rId%.0f", relationships$get_next_id())

    file.copy(
      from = hl_ref[i],
      to = file.path(package_dir, media_dir, basename(hl_ref[i]))
    )

    relationships$add(
      id = rid,
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/aFChunk",
      target = basename(hl_ref[i])
    )
    content_type$add_override(
      setNames(
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml",
        paste0("/", media_dir, "/", basename(hl_ref[i]))
      )
    )

    which_match_id <- grepl(
      hl_ref[i],
      xml_attr(which_to_add, "id"),
      fixed = TRUE
    )
    xml_attr(which_to_add[which_match_id], "r:id") <- rep(
      rid,
      sum(which_match_id)
    )
  }
}

#' @importFrom xml2 xml_remove as_xml_document xml_parent xml_child
process_links <- function(doc_obj, type = "wml") {
  rel <- doc_obj$relationship()
  if ("wml" %in% type) {
    hl_nodes <- xml_find_all(doc_obj$get(), "//w:hyperlink[@r:id]")
  } else {
    hl_nodes <- xml_find_all(doc_obj$get(), "//a:hlinkClick[@r:id]")
  }
  which_to_add <- hl_nodes[!grepl("^rId[0-9]+$", xml_attr(hl_nodes, "id"))]
  hl_ref <- unique(xml_attr(which_to_add, "id"))
  for (i in seq_along(hl_ref)) {
    rid <- sprintf("rId%.0f", rel$get_next_id())

    rel$add(
      id = rid,
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink",
      target = officer_url_decode(hl_ref[i]),
      target_mode = "External"
    )

    which_match_id <- grepl(
      hl_ref[i],
      xml_attr(which_to_add, "id"),
      fixed = TRUE
    )
    xml_attr(which_to_add[which_match_id], "r:id") <- rep(
      rid,
      sum(which_match_id)
    )
  }
}

process_list_markers <- function(xml_str, package_dir) {
  marker_pattern <- "officer-list-(bullet|decimal)-[a-f0-9-]+"
  has_markers <- grepl(marker_pattern, xml_str)
  if (!any(has_markers)) {
    return(xml_str)
  }

  all_markers <- regmatches(
    xml_str[has_markers],
    gregexpr(marker_pattern, xml_str[has_markers])
  )
  unique_markers <- unique(unlist(all_markers))

  numbering_file <- file.path(package_dir, "word", "numbering.xml")
  if (!file.exists(numbering_file)) {
    cli::cli_abort("numbering.xml not found in {.file {package_dir}}")
  }
  numbering_doc <- read_xml(numbering_file)
  ns_w <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  abstract_nums <- xml_find_all(
    numbering_doc,
    "w:abstractNum",
    ns = c(w = ns_w)
  )
  nums <- xml_find_all(numbering_doc, "w:num", ns = c(w = ns_w))
  next_abstract_id <- if (length(abstract_nums) > 0) {
    max(as.integer(xml_attr(abstract_nums, "abstractNumId"))) + 1L
  } else {
    0L
  }
  next_num_id <- if (length(nums) > 0) {
    max(as.integer(xml_attr(nums, "numId"))) + 1L
  } else {
    1L
  }

  for (marker in unique_markers) {
    list_type <- sub("officer-list-(bullet|decimal)-.*", "\\1", marker)
    abstract_xml <- build_abstract_num_xml(
      next_abstract_id,
      list_type,
      ns_w
    )
    num_xml <- sprintf(
      paste0(
        '<w:num xmlns:w="%s" w:numId="%d">',
        '<w:abstractNumId w:val="%d"/></w:num>'
      ),
      ns_w,
      next_num_id,
      next_abstract_id
    )

    xml_add_child(numbering_doc, read_xml(abstract_xml))
    xml_add_child(numbering_doc, read_xml(num_xml))

    xml_str <- gsub(
      marker,
      as.character(next_num_id),
      xml_str,
      fixed = TRUE
    )

    next_abstract_id <- next_abstract_id + 1L
    next_num_id <- next_num_id + 1L
  }

  write_xml(numbering_doc, numbering_file)
  xml_str
}

build_abstract_num_xml <- function(abstract_num_id, list_type, ns_w) {
  levels <- vapply(
    0:8,
    function(ilvl) {
      indent_left <- 360L * (ilvl + 1L)
      if (list_type == "bullet") {
        bullets <- c(
          "\u2022",
          "\u25E6",
          "\u2013",
          "\u2022",
          "\u25E6",
          "\u2013",
          "\u2022",
          "\u25E6",
          "\u2013"
        )
        sprintf(
          paste0(
            '<w:lvl w:ilvl="%d">',
            '<w:numFmt w:val="bullet"/>',
            '<w:lvlText w:val="%s"/>',
            '<w:lvlJc w:val="left"/>',
            '<w:pPr><w:ind w:left="%d" w:hanging="360"/></w:pPr>',
            '</w:lvl>'
          ),
          ilvl,
          bullets[ilvl + 1L],
          indent_left
        )
      } else {
        sprintf(
          paste0(
            '<w:lvl w:ilvl="%d">',
            '<w:start w:val="1"/>',
            '<w:numFmt w:val="decimal"/>',
            '<w:lvlText w:val="%%%d."/>',
            '<w:lvlJc w:val="left"/>',
            '<w:pPr><w:ind w:left="%d" w:hanging="360"/></w:pPr>',
            '</w:lvl>'
          ),
          ilvl,
          ilvl + 1L,
          indent_left
        )
      }
    },
    character(1L)
  )

  sprintf(
    paste0(
      '<w:abstractNum xmlns:w="%s" w:abstractNumId="%d">',
      '<w:multiLevelType w:val="multilevel"/>%s',
      '</w:abstractNum>'
    ),
    ns_w,
    abstract_num_id,
    paste0(levels, collapse = "")
  )
}

update_hf_list <- function(part_list = list(), type = "header", package_dir) {
  files <- list.files(
    path = file.path(package_dir, "word"),
    pattern = sprintf("^%s[0-9]*.xml$", type)
  )
  files <- files[!basename(files) %in% names(part_list)]
  if (type %in% "header") {
    cursor <- "/w:hdr/*[1]"
    body_xpath <- "/w:hdr"
  } else {
    cursor <- "/w:ftr/*[1]"
    body_xpath <- "/w:ftr"
  }

  new_list <- lapply(files, function(x) {
    docx_part$new(
      path = package_dir,
      main_file = x,
      cursor = cursor,
      body_xpath = body_xpath
    )
  })
  names(new_list) <- basename(files)
  append(part_list, new_list)
}

#' @export
#' @title Remove unused media from a document
#' @description The function will scan the media
#' directory and delete images that are not used
#' anymore. This function is to be used when images
#' have been replaced many times.
#' @param x `rdocx` or `rpptx` object
#' @param warn_user TRUE to make sure users are warned when
#' using this function that will be un-exported in a next
#' version
#' @keywords internal
sanitize_images <- function(x, warn_user = TRUE) {
  if (warn_user) {
    cli::cli_warn(
      c(
        "!" = "Function {.fn sanitize_images} will be removed soon. You should remove calls to {.fn sanitize_images}.",
        "i" = "Instead the function is run before saving the document to a file."
      )
    )
  }

  if (inherits(x, "rdocx")) {
    image_files <- c()
    all_docs <- append(x$headers, x$footers)
    all_docs[[length(all_docs) + 1]] <- x$doc_obj
    all_docs[[length(all_docs) + 1]] <- x$footnotes
    all_docs[[length(all_docs) + 1]] <- x$comments

    for (doc_part in all_docs) {
      rid_attrs <- xml_find_all(
        doc_part$get(),
        "//@*[namespace-uri()='http://schemas.openxmlformats.org/officeDocument/2006/relationships']"
      )
      rid_list <- unique(xml_text(rid_attrs))

      embed_data <- filter(
        .data = doc_part$rel_df(),
        basename(.data$type) %in% "image",
        .data$id %in% rid_list
      )
      # OPC relationship targets are URI-encoded (e.g. "my%20image.gif"),
      # so decode them before they are compared against the real on-disk
      # file names gathered in existing_img (which are not encoded).
      embed_data <- .url_percent_decode(embed_data$target)
      image_files[[length(image_files) + 1]] <- embed_data
    }

    image_files <- do.call(c, image_files)
    image_files <- unique(image_files)

    # Union in the image targets of every UNMANAGED .rels file in the
    # package. Officer never edits parts such as endnotes.xml, charts/*,
    # diagrams/* (SmartArt) or glossary/*, so their relationship files are
    # the authoritative list of what they reference (this mirrors what the
    # rpptx branch already does globally). Without this, media referenced
    # only from such a part would be deleted on save while the part's
    # (verbatim, never rewritten) .rels still points at it -> corrupt docx.
    managed_parts <- c(
      "document.xml", names(x$headers), names(x$footers),
      "footnotes.xml", "comments.xml"
    )
    managed_rels <- file.path(
      x$package_dir, "word", "_rels", paste0(managed_parts, ".rels")
    )
    all_rels <- list.files(
      x$package_dir, pattern = "\\.rels$",
      recursive = TRUE, full.names = TRUE, all.files = TRUE
    )
    unmanaged_rels <- setdiff(
      normalizePath(all_rels, winslash = "/", mustWork = FALSE),
      normalizePath(managed_rels, winslash = "/", mustWork = FALSE)
    )

    base_doc <- file.path(x$package_dir, "word")
    base_norm <- normalizePath(base_doc, winslash = "/", mustWork = FALSE)
    for (rf in unmanaged_rels) {
      zz <- read_xml(rf)
      rels <- xml_children(zz)
      rels <- rels[
        xml_attr(rels, "Type") %in%
          "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" &
          !xml_attr(rels, "TargetMode") %in% "External"
      ]
      targets <- xml_attr(rels, "Target")
      targets <- .url_percent_decode(targets)
      if (length(targets) == 0L) {
        next
      }
      part_dir <- dirname(dirname(rf)) # the dir owning this _rels folder
      resolved <- ifelse(
        startsWith(targets, "/"),
        file.path(x$package_dir, sub("^/", "", targets)),
        file.path(part_dir, targets)
      )
      resolved <- normalizePath(resolved, winslash = "/", mustWork = FALSE)
      # keep only paths under word/, expressed relative to word/ like the
      # entries already in image_files (e.g. "media/image1.emf")
      under <- !is.na(resolved) &
        startsWith(resolved, paste0(base_norm, "/"))
      image_files <- c(
        image_files,
        substring(resolved[under], nchar(base_norm) + 2L)
      )
    }
    image_files <- unique(image_files)

    existing_img <- list.files(
      file.path(base_doc, "media"),
      pattern = "\\.(png|jpg|jpeg|gif|bmp|tif|tiff|wmf|eps|emf|svg)$",
      ignore.case = TRUE,
      recursive = TRUE,
      full.names = TRUE
    )

    existing_img <- gsub(paste0(base_doc, "/"), "", existing_img, fixed = TRUE)
    unlink(
      file.path(base_doc, setdiff(existing_img, image_files)),
      force = TRUE
    )

    # these iterations will remove removed images from relationships
    for (doc_part in all_docs) {
      rel <- doc_part$relationship()
      rel_data <- rel$get_data()
      rel_data <- rel_data[basename(rel_data$type) %in% "image", ]
      rel_data <- rel_data[!rel_data$target_mode %in% "External", ]
      # The file-exists test must use the DECODED target (OPC targets are
      # URI-encoded), but rel$remove() below still receives the ORIGINAL
      # stored target strings because removal matches by stored string.
      rel_data <- rel_data[
        !file.exists(file.path(base_doc, .url_percent_decode(rel_data$target))),
      ]
      if (nrow(rel_data) > 0) {
        rel$remove(rel_data$target)
        doc_part$save()
      }
    }
  } else if (inherits(x, "rpptx")) {
    rel_files <- list.files(
      x$package_dir,
      pattern = "\\.xml.rels$",
      recursive = TRUE,
      full.names = TRUE
    )

    # Collect every image relationship target as a CANONICAL ABSOLUTE path,
    # mirroring the docx unmanaged-rels loop above. pptx has no
    # managed/unmanaged split, so anything referenced from ANY .rels stays.
    # Resolving to absolute paths (and decoding percent-escapes first) makes
    # the comparison robust to the target forms actually found in packages:
    # "../media/image.gif", "media/image.gif" (parts directly under ppt/),
    # "../../media/image.gif" (nested subdirs), "/ppt/media/image.gif"
    # (package-absolute) and URI-encoded names ("my%20image.gif").
    image_files <- c()
    for (rf in rel_files) {
      zz <- read_xml(rf)
      rels <- xml_children(zz)
      rels <- rels[
        xml_attr(rels, "Type") %in%
          "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" &
          !xml_attr(rels, "TargetMode") %in% "External"
      ]
      targets <- xml_attr(rels, "Target")
      targets <- .url_percent_decode(targets)
      if (length(targets) == 0L) {
        next
      }
      part_dir <- dirname(dirname(rf)) # the dir owning this _rels folder
      resolved <- ifelse(
        startsWith(targets, "/"),
        file.path(x$package_dir, sub("^/", "", targets)),
        file.path(part_dir, targets)
      )
      resolved <- normalizePath(resolved, winslash = "/", mustWork = FALSE)
      image_files <- c(image_files, resolved)
    }
    image_files <- unique(image_files)

    base_doc <- file.path(x$package_dir, "ppt")
    existing_img <- list.files(
      file.path(base_doc, "media"),
      pattern = "\\.(png|jpg|jpeg|gif|bmp|tif|tiff|wmf|eps|emf|svg)$",
      ignore.case = TRUE,
      recursive = TRUE,
      full.names = TRUE
    )
    existing_img <- normalizePath(existing_img, winslash = "/", mustWork = FALSE)
    # unlink the set difference (absolute paths): media under ppt/media
    # matching the glob that no .rels references.
    unlink(setdiff(existing_img, image_files), force = TRUE)
  }
  x
}
