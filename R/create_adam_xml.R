
#' @title Create XML for ADAM
#' @description Function generates XML for the ADAM define.xml file.
#' @param lst A list of data frames that contain SDTM metadata.
#' @param version The version of the define XML to create.  Currently
#' only 2.0.0 is supported, which is the default.
#' @returns A vector of XML strings.
#' @noRd
create_adam_xml <- function(lst, version) {

  if(!is.list(lst)) {
    stop("Metadata must provided in the form of a list")
  }

  if (version == "2.0.0") {

    ret <- get_adam_xml_20(lst)

  } else {

    stop(paste0("Version ", version, " not supported."))
  }


  return(ret)

}


get_adam_xml_20 <- function(lst) {

  nms <- names(lst)

  hdr <- c()
  if ("DEFINE_HEADER_METADATA" %in% nms)
    hdr <- get_header_adam(lst[["DEFINE_HEADER_METADATA"]])
  else
    stop("Header metadata is required.")

  grps <- c()
  if ("TOC_METADATA" %in% nms)
    grps <- get_item_groups_adam(lst[["TOC_METADATA"]], lst[["VARIABLE_METADATA"]])
  else
    stop("Table of Contents metadata is required.")

  defs <- c()
  if ("VARIABLE_METADATA" %in% nms)
    defs <- get_item_defs_adam(lst[["TOC_METADATA"]],
                               lst[["VARIABLE_METADATA"]],
                               lst[["VALUELEVEL_METADATA"]])
  else
    stop("Variable metadata is required.")

  val <- c()
  if ("VALUELEVEL_METADATA" %in% nms)
    val <- get_value_level(lst[["VALUELEVEL_METADATA"]], lst[["WHERE_CLAUSES"]])
  else
    stop("Table of Contents metadata is required.")

  comp <- c()
  if ("COMPUTATION_METHOD" %in% nms)
    comp <- get_computations(lst[["COMPUTATION_METHOD"]])
  else
    stop("Computation Method metadata is required.")

  cl <- c()
  if ("CODELISTS" %in% nms)
    cl <- get_code_lists_adam(lst[["CODELISTS"]])
  else
    stop("Code List metadata is required.")

  whr <- c()
  if ("WHERE_CLAUSES" %in% nms)
    whr <- get_where(lst[["WHERE_CLAUSES"]],
                     lst[["VALUELEVEL_METADATA"]])


  cmnts <- c()
  if ("COMMENTS" %in% nms)
    cmnts <- get_comments(lst[["COMMENTS"]])


  extl <- c()
  leafdefs <- c()
  if ("EXTERNAL_LINKS" %in% nms)
  {
    extl <- get_external_links(lst[["EXTERNAL_LINKS"]])
    leafdefs <- get_leaf_definitions(lst[["EXTERNAL_LINKS"]])
  }

  analysis <- c()
  if("ANALYSIS_RESULTS" %in% nms)
    analysis <- get_analysis_results(lst[["ANALYSIS_RESULTS"]], lst[["EXTERNAL_LINKS"]])

  ftr <- get_footer()


  ret <- c(hdr, extl, val, whr, grps, defs, cl, comp, cmnts, leafdefs, analysis, ftr)



  return(ret)
}


# Small changes in header relating to ADaM name/version etc.
#' @noRd
get_header_adam <- function(dta) {

  str <- '
  <?xml version="1.0" encoding="ISO-8859-1" ?>
    <?xml-stylesheet type="text/xsl" href="define2-0-0.xsl"?>
      <!-- ************************************************************* -->
      <!-- File: define.xml                                              -->
      <!-- Date: {sdt}                                                   -->
      <!-- Description: Define.xml file for {foid}                       -->
      <!-- Created by the defineR package                                -->
      <!-- ************************************************************* -->
      <ODM
    xmlns="http://www.cdisc.org/ns/odm/v1.3"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:def="http://www.cdisc.org/ns/def/v2.0"
    ODMVersion="1.3.2"
    xmlns:arm="http://www.cdisc.org/ns/arm/v1.0"
    FileOID="{foid}"
    FileType="Snapshot"
    CreationDateTime="{dstmp}">
      <Study OID="{soid}">
      <GlobalVariables>
      <StudyName>{study}</StudyName>
      <StudyDescription>{desc}</StudyDescription>
      <ProtocolName>{protocol}</ProtocolName>
      </GlobalVariables>
      <MetaDataVersion OID="CDISC.ADaM-IG.1.0"
    Name="{study}, ADaM Data Definitions"
    Description="{study}, ADaM Data Definitions"
    def:DefineVersion="2.0.0"
    def:StandardName="{sn}"
    def:StandardVersion="{sv}">'

  ret <- glue(str,
              sdt = format(Sys.Date(), "%d%B%Y"),
              dstmp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
              foid = cleanid(dta[["FILEOID"]][1]),
              soid = cleanid(dta[["STUDYOID"]][1]),
              study = dta[["STUDYNAME"]][1],
              desc = encodeMarkup(dta[["STUDYDESCRIPTION"]][1]),
              protocol = dta[["PROTOCOLNAME"]][1],
              sn = dta[["STANDARD"]][1],
              sv = dta[["VERSION"]][1],
              stylesheet = dta[["STYLESHEET"]][1])


  return(ret)
}

# CommentOID def added, role and rolecodelist removed
#' @noRd
get_item_groups_adam <- function(toc, vardt) {
  blk <-
    '  <!-- ******************************************* -->
    <!-- {name}             ItemGroupDef INFORMATION *** -->
    <!-- ******************************************* -->'

  itemGroup <-
    '  <ItemGroupDef OID="IG.{oid}"
      Domain="{name}"
      Name="{name}"
      Repeating="{reps}"
      Purpose="{purp}"
      IsReferenceData="{isRef}"
      SASDatasetName="{name}"
      def:Structure="{struct}"
      def:Class="{class}"
      def:CommentOID="COM.{commentoid}"
      def:ArchiveLocationID="LF.{name}">
      <Description>
        <TranslatedText xml:lang="en">{label}</TranslatedText>
      </Description>'

  endCom <-
    ' <!-- **************************************************** -->
      <!-- def:leaf details for hypertext linking the dataset   -->
      <!-- **************************************************** -->'

  groupEnd <-
    '<def:leaf ID="LF.{name}" xlink:href="{loc}.xpt">
        <def:title>{loc}.xpt </def:title>
      </def:leaf>
    </ItemGroupDef>'

  itemRefs <-
    '<ItemRef ItemOID="IT.{domain}.{varname}"
    OrderNumber="{varnum}"
    Mandatory="{manda}"
    {keyseq}{methodoid}/>'

  ret<-vector()
  for(rw in 1:nrow(toc)) {
    ret[length(ret) + 1] <- glue(blk, name = cleanid(toc[[rw, "NAME"]]))
    ret[length(ret) + 1] <- glue(itemGroup,
                                 oid = cleanid(toc[[rw, "OID"]]),
                                 name = cleanid(toc[[rw, "NAME"]]),
                                 reps = toc[[rw, "REPEATING"]],
                                 purp = cleanid(toc[[rw, "PURPOSE"]]),
                                 isRef = toc[[rw, "ISREFERENCEDATA"]],
                                 struct = toc[[rw, "STRUCTURE"]],
                                 class = toc[[rw, "CLASS"]],
                                 commentoid = cleanid(toc[[rw, "COMMENTOID"]]),
                                 label = encodeMarkup(toc[[rw, "LABEL"]]))

    for(varrow in 1:nrow(vardt)) {
      keyHolder <- ""
      methodoidHolder <- ""
      # search for variables sharing domain name from toc
      if(toc[[rw, "NAME"]] %eq% vardt[[varrow, "DOMAIN"]]) {
        # second check, existence of keyseq
        if(!is.na(vardt[varrow, "KEYSEQUENCE"])) {
          keyHolder <- paste0('KeySequence="',cleanid(vardt[[varrow, "KEYSEQUENCE"]]),'"\n')
        }
        # third check, nonmutual, existence of methodoid
        if(!is.na(vardt[varrow, "COMPUTATIONMETHODOID"])) {
          methodoidHolder <- paste0('MethodOID="MT.',cleanid(vardt[[varrow, "COMPUTATIONMETHODOID"]]),'"\n')
        }




        # itemref
        ret[length(ret) + 1] <- glue(itemRefs,
                                     domain = cleanid(vardt[[varrow, "DOMAIN"]]),
                                     varname = cleanid(vardt[[varrow, "VARIABLE"]]),
                                     varnum = cleanid(vardt[[varrow, "VARNUM"]]),
                                     manda = vardt[[varrow, "MANDATORY"]],
                                     keyseq = keyHolder,
                                     methodoid = methodoidHolder)
      }
    }

    ret[length(ret) + 1] <- endCom
    ret[length(ret) + 1] <- glue(groupEnd,
                                 name = cleanid(toc[[rw, "NAME"]]),
                                 loc = cleanid(toc[[rw, "ARCHIVELOCATIONID"]]))

  }
  return(ret)
}



#' @import glue
#' @noRd
get_item_defs_adam <- function(toc, vardt, valdt) {

  blk <- '<!-- ************************************************************ -->
  <!-- The details of each variable is here for all domains         -->
  <!-- ************************************************************ -->'
  str <-
    ' <ItemDef OID="IT.{domain}.{variable}"
      Name="{variable}"
      SASFieldName="{variable}"
      DataType="{type}"
      Length="{length}"
      def:DisplayFormat="{display}"
      {commentid}
      >
      <Description>
          <TranslatedText xml:lang="en">{label}</TranslatedText>
      </Description>
      {codelistref}<def:Origin Type="{origin}">
        {internals}
      </def:Origin>
    </ItemDef>'


  vdefstr <-
    '<ItemDef OID="{ValueOID}" Name="{Variable}" SASFieldName="{SASFieldName}"
         DataType="{DataType}" Length="{Length}">
        <Description>
          <TranslatedText xml:lang="en">{Label}</TranslatedText>
        </Description>
        <def:Origin Type="{Origin}"/>
      </ItemDef>'

  valstr <- '<def:ValueListRef ValueListOID="{ValueID}"/>'

  ret <- c(blk)

  for(varrow in 1:nrow(vardt)) {
    newDomain <- vardt[[varrow, "DOMAIN"]]
    flag <- varrow
      strHolder <- ""
      if(!is.na(vardt[varrow, "DISPLAYFORMAT"])) {
        strHolder <- vardt[[varrow, "DISPLAYFORMAT"]]
      }
      else {
        strHolder <- vardt[[varrow, "LENGTH"]]
      }
      codeListHolder <- ""
      if(!is.na(vardt[[varrow, "CODELISTNAME"]])) {

        clstr <- vardt[[varrow, "CODELISTNAME"]]

        spos <- grep("*", clstr, fixed = TRUE)
        ipos <- grep("ISO", clstr, fixed = TRUE)

        if (length(spos) == 0 & length(ipos) == 0) {
          codeListHolder <- '<CodeListRef CodeListOID="CL.{codelist}"/>\n'
          codeListHolder <- glue(codeListHolder, codelist = cleanid(vardt[varrow, "CODELISTNAME"]))
        }
      }



      sbst <- subset(valdt, valdt$DOMAIN==vardt[[varrow, "DOMAIN"]] &
                       valdt$VARIABLE==vardt[[varrow, "VARIABLE"]])

      valLevel <- ""
      vDefs <- ""
      if (nrow(sbst) > 0) {

        #browser()

        vid <- paste0("VL.",
                      cleanid(vardt[[varrow, "DOMAIN"]]),
                      ".",
                      cleanid(vardt[[varrow, "VARIABLE"]]))

        valLevel <- paste0(valLevel,  glue(valstr,
                                           ValueID = vid), "\n")

        for (i in seq_len(nrow(sbst))) {

          vid <- paste0("IT.",
                        cleanid(sbst[[i, "DOMAIN"]]),
                        ".",
                        cleanid(sbst[[i, "VARIABLE"]]),
                        ".",
                        cleanid(sbst[[i, "VALUENAME"]]))

          if (!is.na(sbst[[i, "WHERECLAUSEOID"]])) {

            splt <- strsplit(sbst[[i, "WHERECLAUSEOID"]], ".", fixed = TRUE)[[1]]

            vid <- paste0(vid, ".", cleanid(splt[length(splt)]))
          }

          vDefs <- paste0(vDefs,  glue(vdefstr,
                                       ValueOID = vid,
                                       Variable = cleanid(vardt[[varrow, "VARIABLE"]]),
                                       SASFieldName = vardt[[varrow, "VARIABLE"]],
                                       DataType = cleanid(vardt[[varrow, "TYPE"]]),
                                       Length = vardt[[varrow, "LENGTH"]],
                                       Label = vardt[[varrow, "LABEL"]],
                                       Origin = vardt[[varrow, "ORIGIN"]]), "\n")

        }

      }


      internalHolder <- ""
      originHolder <- ""
      if(vardt[[varrow, "ORIGIN"]] %eq% 'Assigned' ||
         vardt[[varrow, "ORIGIN"]] %eq% 'Derived') {
        originHolder <- encodeMarkup(vardt[[varrow, "ORIGIN"]])
      }
      else {
        internalHolder <- '<Description>
        <TranslatedText xml:lang="en">{origin}</TranslatedText>
      </Description>'
        internalHolder <- glue(internalHolder, origin = vardt[varrow, "ORIGIN"])
        originHolder <- "Predecessor"
      }

      cmtid <- ""
      if (!is.na(vardt[[varrow, "COMMENTOID"]])) {

        cmtid <- paste0('def:CommentOID="COM.', cleanid(vardt[[varrow, "COMMENTOID"]]), '"')
      }
      ret[length(ret) + 1] <- glue(str,
                                   domain = cleanid(vardt[[varrow, "DOMAIN"]]),
                                   variable = cleanid(vardt[[varrow, "VARIABLE"]]),
                                   type = cleanid(vardt[[varrow, "TYPE"]]),
                                   length = vardt[[varrow, "LENGTH"]],
                                   display = strHolder,
                                   internals = internalHolder,
                                   codelistref = codeListHolder,
                                   label = encodeMarkup(vardt[[varrow, "LABEL"]]),
                                   origin = originHolder,
                                   commentid = cmtid)

      ret[length(ret) + 1] <- vDefs
  }

  return(ret)


}



# Changed Rank to OrderNumber, otherwise identical
#' @noRd
get_code_lists_adam <- function(dta) {
  blk <- '  <!-- ************************************************************ -->
  <!-- Codelists are presented below                                -->
  <!-- ************************************************************ -->'
  listHead <- '<CodeList OID="CL.{codelistname}"
  Name="{codelistname}"
  DataType="{dtype}">'
  endCL <- '</CodeList>'
  item <- '  <CodeListItem CodedValue="{codedval}" OrderNumber="{rank}">
          <Decode>
            <TranslatedText>{translated}</TranslatedText>
          </Decode>
        </CodeListItem>'
  dictcl <- '<ExternalCodeList Dictionary="{dict}" Version="{clver}"/>'


  f <- list(as.factor(dta[["CODELISTNAME"]]))
  splts <- split(dta, f)


  ret <- c(blk)

  for(sp in splts) {
    ret[length(ret) + 1] <- glue(listHead,
                                 codelistname = cleanid(sp[[1, "CODELISTNAME"]]),
                                 dtype = cleanid(sp[[1, "TYPE"]]))
    for(rw in seq_len(nrow(sp))) {
      if(!is.na(sp[[rw, "CODELISTDICTIONARY"]])) {
        ret[length(ret) + 1] <- glue(dictcl,
                                     dict = sp[[rw, "CODELISTDICTIONARY"]],
                                     clver = sp[[rw, "CODELISTVERSION"]])
      }
      else {
        ret[length(ret) + 1] <- glue(item,
                                     codedval = encodeMarkup(sp[[rw, "CODEDVALUE"]]),
                                     rank = sp[[rw, "RANK"]],
                                     translated = encodeMarkup(sp[[rw, "TRANSLATED"]]))
      }
    }
    ret[length(ret) + 1] <- endCL
  }
  return(ret)
}



# Stub for leaf definition
get_leaf_definitions <- function(dta) {

  blk <- "    <!-- ******************************************* -->
    <!-- LEAF DEFINITION SECTION                 *** -->
    <!-- ******************************************* -->"

  leafdefs <- '    <def:leaf ID="LF.{leafid}"
      xlink:href="{leafrelpath}">
      <def:title>{title}</def:title>
    </def:leaf>\n'
  ret <- c(blk)
  for(rw in 1:nrow(dta)) {
    ret[length(ret) + 1] <- glue(leafdefs,
                                 leafid = cleanid(dta[[rw, "LeafID"]]),
                                 leafrelpath = dta[[rw, "LeafRelPath"]],
                                 title = encodeMarkup(dta[[rw, "Title"]]))
  }
  return(ret)
}


# Final issue - what do with analysis_results_helper when analysisdataset is NA
# (but the variable is not)
# Func already handles opposite scenario and returns empty
get_analysis_results <- function(dta, lfdta) {
  blk <-
  '<!-- ************************************************************ -->
    <!-- Analysis Results MetaData are Presented Below                -->
    <!-- ************************************************************ -->
    <arm:AnalysisResultDisplays>'

  resultDisplay <-
  '    <arm:ResultDisplay OID="RD.{dispid}" Name="{dispid}">
      <Description>
          <TranslatedText xml:lang="en">{dispname}</TranslatedText>
      </Description>
      <def:DocumentRef leafID="LF.{dispid}">
        <def:PDFPageRef PageRefs="{lfpgid}" Type="{lreftype}"/>
      </def:DocumentRef>'


  analysisResult <- '<arm:AnalysisResult
        OID="AR.{analysisid}"
        {paramcd}
        ResultIdentifier="AR.{analysisid}"
        AnalysisReason="{reason}"
        AnalysisPurpose="{purpose}">
        <Description>
          <TranslatedText xml:lang="en">{resultname}</TranslatedText>
        </Description>
        <arm:AnalysisDatasets>
          <arm:AnalysisDataset ItemGroupOID="IG.{analysisdata}" >

        {WhereClauses}
        {analysisvars}
          </arm:AnalysisDataset>
        </arm:AnalysisDatasets>
        <arm:Documentation>
        <Description>
          <TranslatedText xml:lang="en">{document}</TranslatedText>
        </Description>
        <def:DocumentRef  leafID="{rleafid}">
        </def:DocumentRef>
        </arm:Documentation>

        <arm:ProgrammingCode Context="{context}">
          {pgrmcode}
          {pgrmref}
        </arm:ProgrammingCode>
      </arm:AnalysisResult>'

  ardend <- '</arm:ResultDisplay>'

  end <- '</arm:AnalysisResultDisplays>'

  wcstr <- '      <def:WhereClauseRef WhereClauseOID="WC.{wcoid}"/>\n'

  ret <- c(blk)

  udsp <- unique(dta[["DISPLAYID"]])

  for (sb in seq_len(length(udsp))) {

    sbst <- subset(dta, dta$DISPLAYID == udsp[[sb]])


    for(rw in 1:nrow(sbst)) {


      if (length(udsp) > 1)
        cntr <- rw
      else
        cntr <- 0

      # Output resultDisplay on first row
      if (rw == 1) {
#browser()
        lpg <- ""
        rtype <- ""
        lsbst <- subset(lfdta, lfdta$LeafID == sbst[[rw, "DISPLAYID"]])
        if (nrow(lsbst) > 0) {

          lpg <- lsbst[[1, "LeafPageRef"]]
          rtype <-  lsbst[[1, "LeafPageRefType"]]
        }


        ret[length(ret) + 1] <- glue(resultDisplay,
                                dispid = cleanid(sbst[[rw, "DISPLAYID"]]),
                                dispname = encodeMarkup(sbst[[rw, "DISPLAYNAME"]]),
                                lfpgid = lpg,
                                lreftype = rtype)

      }


      wc <- ""
      if (!is.na(sbst[[rw, "WHERECLAUSEOID"]])) {

           wc <- glue(wcstr, wcoid = cleanid(sbst[[rw, "WHERECLAUSEOID"]]))


      }


      pcd <- ""
      if (!is.na(sbst[[rw, "PARAMCD"]])) {


        pcd <- paste0('ParameterOID="',
                        "IT.", cleanid(trimws(sbst[[rw, "ANALYSISDATASET"]])) , ".PARAMCD",
                      '"')
      }


      cd <- ""
      if (!is.na(sbst[[rw, "PROGRAMMINGCODE"]])) {

        cd <- glue('<arm:Code>{pgrmcode}</arm:Code>\n',
                   pgrmcode = encodeMarkup(sbst[[rw, "PROGRAMMINGCODE"]]))
      }

      cdref <- ""
      if (!is.na(sbst[[rw, "PROGRAMLEAFID"]])) {

        cdref <- glue('<def:DocumentRef leafID="LF.{pglfid}" />\n',
                      pglfid = cleanid(trimws(sbst[[rw, "PROGRAMLEAFID"]])))
      }





        if (cntr == 0)
          arid <- cleanid(sbst[[rw, "DISPLAYID"]])
        else
          arid <- paste0(cleanid(sbst[[rw, "DISPLAYID"]]), ".R", cntr)
#browser()
      # Output analysis result on every row
      ret[length(ret) + 1] <- glue(analysisResult,
                                   analysisid = arid,
                                   analysisdata = cleanid(sbst[[rw, "ANALYSISDATASET"]]),
                                   paramcd = pcd,
                                   reason = encodeMarkup(sbst[[rw, "REASON"]]),
                                   purpose = encodeMarkup(sbst[[rw, "PURPOSE"]]),
                                   resultname = sbst[[rw, "RESULTNAME"]],
                                   WhereClauses = wc,
                                   analysisvars = analysis_results_helper(sbst[[rw,
                                    "ANALYSISVARIABLES"]],
                                    sbst[[rw, "ANALYSISDATASET"]]),
                                   document = encodeMarkup(sbst[[rw, "DOCUMENTATION"]]),
                                   rleafid = ifelse(!is.na(sbst[[rw, "REFLEAFID"]]),
                                                    paste0("LF.", cleanid(sbst[[rw, "REFLEAFID"]])), ""),
                                   context = encodeMarkup(sbst[[rw, "CONTEXT"]]),
                                   pgrmcode = cd,
                                   pgrmref = cdref
                                   )
    }


    ret[length(ret) + 1] <- ardend


  }

  ret[length(ret) + 1] <- end

  return(ret)
}


analysis_results_helper <- function(cellcont, dtaSetName) {

  varTag <- '<arm:AnalysisVariable ItemOID="IT.{analysisdata}.{varname}"/>'
  # Generate analysis variables
  strHolder <- ""
  if(!(cellcont %eq% "")){
    varRes <- strsplit(cellcont, split = ", ", fixed = TRUE)

    for(i in 1:length(varRes[[1]])) {
      temp <- glue(varTag,
                   analysisdata = cleanid(dtaSetName),
                   varname = cleanid(varRes[[1]][[i]]))
      strHolder <- paste0(strHolder, temp,"\n")
    }
  }
  return(strHolder)
}
