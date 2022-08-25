
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
    whr <- get_where(lst[["WHERE_CLAUSES"]], lst[["VALUELEVEL_METADATA"]])


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
    analysis <- get_analysis_results(lst[["ANALYSIS_RESULTS"]], lst[["WHERE_CLAUSES"]])

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
              foid = dta[["FILEOID"]][1],
              soid = dta[["STUDYOID"]][1],
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
      def:ArchiveLocationID="Location.{name}">
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
    ret[length(ret) + 1] <- glue(blk, name = toc[[rw, "NAME"]])
    ret[length(ret) + 1] <- glue(itemGroup,
                                 oid = toc[[rw, "OID"]],
                                 name = toc[[rw, "NAME"]],
                                 reps = toc[[rw, "REPEATING"]],
                                 purp = toc[[rw, "PURPOSE"]],
                                 isRef = toc[[rw, "ISREFERENCEDATA"]],
                                 struct = toc[[rw, "STRUCTURE"]],
                                 class = toc[[rw, "CLASS"]],
                                 commentoid = toc[[rw, "COMMENTOID"]],
                                 label = encodeMarkup(toc[[rw, "LABEL"]]))

    for(varrow in 1:nrow(vardt)) {
      keyHolder <- ""
      methodoidHolder <- ""
      # search for variables sharing domain name from toc
      if(toc[[rw, "NAME"]] %eq% vardt[[varrow, "DOMAIN"]]) {
        # second check, existence of keyseq
        if(!is.na(vardt[varrow, "KEYSEQUENCE"])) {
          keyHolder <- paste0('KeySequence="',vardt[[varrow, "KEYSEQUENCE"]],'"\n')
        }
        # third check, nonmutual, existence of methodoid
        if(!is.na(vardt[varrow, "COMPUTATIONMETHODOID"])) {
          methodoidHolder <- paste0('MethodOID="MT.',vardt[[varrow, "COMPUTATIONMETHODOID"]],'"\n')
        }




        # itemref
        ret[length(ret) + 1] <- glue(itemRefs,
                                     domain = vardt[[varrow, "DOMAIN"]],
                                     varname = vardt[[varrow, "VARIABLE"]],
                                     varnum = vardt[[varrow, "VARNUM"]],
                                     manda = vardt[[varrow, "MANDATORY"]],
                                     keyseq = keyHolder,
                                     methodoid = methodoidHolder)
      }
    }

    ret[length(ret) + 1] <- endCom
    ret[length(ret) + 1] <- glue(groupEnd,
                                 name = toc[[rw, "NAME"]],
                                 loc = toc[[rw, "ARCHIVELOCATIONID"]])

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
      if(!is.na(vardt[varrow, "CODELISTNAME"])) {
        codeListHolder <- '<CodeListRef CodeListOID="CL.{codelist}"/>\n'
        codeListHolder <- glue(codeListHolder, codelist = vardt[varrow, "CODELISTNAME"])
      }



      sbst <- subset(valdt, valdt$DOMAIN==vardt[[varrow, "DOMAIN"]] &
                       valdt$VARIABLE==vardt[[varrow, "VARIABLE"]])

      valLevel <- ""
      vDefs <- ""
      if (nrow(sbst) > 0) {

        #browser()

        vid <- paste0("VL.",
                      vardt[[varrow, "DOMAIN"]],
                      ".",
                      vardt[[varrow, "VARIABLE"]])

        valLevel <- paste0(valLevel,  glue(valstr,
                                           ValueID = vid), "\n")

        for (i in seq_len(nrow(sbst))) {

          vid <- paste0("IT.",
                        sbst[[i, "DOMAIN"]],
                        ".",
                        sbst[[i, "VARIABLE"]],
                        ".",
                        sbst[[i, "VALUENAME"]])

          if (!is.na(sbst[[i, "WHERECLAUSEOID"]])) {

            splt <- strsplit(sbst[[i, "WHERECLAUSEOID"]], ".", fixed = TRUE)[[1]]

            vid <- paste0(vid, ".", splt[length(splt)])
          }

          vDefs <- paste0(vDefs,  glue(vdefstr,
                                       ValueOID = vid,
                                       Variable = vardt[[varrow, "VARIABLE"]],
                                       SASFieldName = vardt[[varrow, "VARIABLE"]],
                                       DataType = vardt[[varrow, "TYPE"]],
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

        cmtid <- paste0('def:CommentOID="COM.', vardt[[varrow, "COMMENTOID"]], '"')
      }
      ret[length(ret) + 1] <- glue(str,
                                   domain = vardt[[varrow, "DOMAIN"]],
                                   variable = vardt[[varrow, "VARIABLE"]],
                                   type = vardt[[varrow, "TYPE"]],
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



# @noRd
# get_value_level_adam <- function(dta) {
#
#
#   blk <- '
#   <!-- ******************************************* -->
#   <!-- VALUE LEVEL LIST DEFINITION INFORMATION  ** -->
#   <!-- ******************************************* -->\n'
#
#   defstart <- ' <def:ValueListDef OID="VL.{domain}.{variable}">\n'
#   defend <- ' </def:ValueListDef>\n'
#   wcstr <- ' <def:WhereClauseRef WhereClauseOID="{wcoid}"/>\n'
#
#   str <- '
#     <ItemRef ItemOID="VL.{domain}.{variable}.{value}"
#       OrderNumber="{varnum}"
#       Mandatory="{mandatory}"
#       {methodoid}>
#       {wc}
#     </ItemRef>'
#
#
#   f <- list(as.factor(dta[["DOMAIN"]]), as.factor(dta[["VARIABLE"]]))
#
#   splts <- split(dta, f)
#
#   ret <- c(blk)
#   last_domain <- ""
#   last_variable <- ""
#
#   for (sp in splts) {
#
#     if (nrow(sp)) {
#       ret[length(ret) + 1] <- glue(defstart,
#                                    domain =  sp[[1, "DOMAIN"]],
#                                    variable = sp[[1, "VARIABLE"]])
#
#       for (rw in seq_len(nrow(sp))) {
#
#
#         whrc <- ""
#         holder <- ""
#         if (!is.na(sp[[rw, "WHERECLAUSEOID"]]))
#           whrc <- glue(wcstr, wcoid = sp[[rw, "WHERECLAUSEOID"]])
#         # Added 312, 315-316
#         if(!is.na(sp[[rw, "COMPUTATIONMETHODOID"]]))
#           holder <- paste0('MethodOID="', sp[[rw, "COMPUTATIONMETHODOID"]], '"')
#
#         ret[length(ret) + 1] <- glue(str,
#                                      domain =  sp[[rw, "DOMAIN"]],
#                                      variable = sp[[rw, "VARIABLE"]],
#                                      value =  sp[[rw, "VALUENAME"]],
#                                      varnum =  sp[[rw, "VARNUM"]],
#                                      mandatory =  sp[[rw, "MANDATORY"]],
#                                      methodoid = holder,
#                                      wc = whrc
#         )
#
#       }
#
#       ret[length(ret) + 1] <- defend
#
#     }
#
#   }
#
#
#   return(ret)
# }

# identical function
# @noRd
# get_computations_adam <- function(dta) {
#
#   blk <-'  <!-- ******************************************* -->
#   <!-- COMPUTATIONAL METHOD INFORMATION        *** -->
#   <!-- ******************************************* -->'
#
#   str <- '<MethodDef OID="{mthdOID}" Name="{label}" Type="{comp}">
#         <Description>
#           <TranslatedText xml:lang="en">{compMthd}</TranslatedText>
#         </Description>
#       </MethodDef>'
#
#   ret <- c(blk)
#   for(rw in seq_len(nrow(dta))) {
#     ret[length(ret) + 1] <- glue(str,
#                                  mthdOID = dta[[rw, "COMPUTATIONMETHODOID"]],
#                                  label = encodeMarkup(dta[[rw, "LABEL"]]),
#                                  comp = dta[[rw, "TYPE"]],
#                                  compMthd = encodeMarkup(dta[[rw, "COMPUTATIONMETHOD"]]))
#   }
#   ret[length(ret) + 1] <- ""
#   return(ret)
#
# }

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
                                 codelistname = sp[[1, "CODELISTNAME"]],
                                 dtype = sp[[1, "TYPE"]])
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


# Identical function
# @noRd
# get_where_adam <- function(dta) {
#
#   blk <- '
#   <!-- ****************************************************************** -->
#   <!-- WhereClause Definitions Used/Referenced in Value List Definitions) -->
#   <!-- ****************************************************************** -->'
#
#   str <- '<def:WhereClauseDef OID="{oid}">
#     <RangeCheck SoftHard="{sh}" def:ItemOID="{iod}" Comparator="{comp}">
#       <CheckValue>{val}</CheckValue>
#       </RangeCheck>
#       </def:WhereClauseDef>'
#
#   ret <- c(blk)
#
#   for (rw in seq_len(nrow(dta))) {
#
#     ret[length(ret) + 1] <- glue(str,
#                                  oid = dta[[rw, "WHERECLAUSEOID"]],
#                                  sh = dta[[rw, "SOFTHARD"]],
#                                  iod = dta[[rw, "ITEMOID"]],
#                                  comp = encodeMarkup(dta[[rw, "COMPARATOR"]]),
#                                  val = encodeMarkup(dta[[rw, "VALUES"]]))
#   }
#
#
#
#   return(ret)
# }



# Identical function
# @noRd
# get_comments_adam <- function(dta) {
#
#   blk <- '
#   <!-- ******************************** -->
#   <!-- COMMENTS DEFINITION SECTION      -->
#   <!-- ******************************** -->'
#
#   str <-'<def:CommentDef OID="{oid}">
#       <Description>
#         <TranslatedText xml:lang="en">{comment}</TranslatedText>
#       </Description>
#     </def:CommentDef>'
#
#
#   ret <- c(blk)
#
#   for (rw in seq_len(nrow(dta))) {
#
#     ret[length(ret) + 1] <- glue(str,
#                                  oid = dta[[rw, "COMMENTOID"]],
#                                  comment = dta[[rw, "COMMENT"]])
#
#   }
#
#   return(ret)
#
#
# }

# Identical Function
# @noRd
# get_external_links_adam <- function(dta) {
#   blk <- '
#   <!-- ******************************************* -->
#   <!-- EXTERNAL DOCUMENT REFERENCE             *** -->
#   <!-- ******************************************* -->'
#
#   str1 <- '<def:AnnotatedCRF>
#       <def:DocumentRef leafID="{leafid}"/>
#     </def:AnnotatedCRF>\n\n'
#
#   str2 <- '<def:SupplementalDoc>
#       <def:DocumentRef leafID="{leafid}"/>
#     </def:SupplementalDoc>\n\n'
#
#   ret <- c(blk)
#
#
#   for(rw in seq_len(nrow(dta))) {
#     # print(dta[[rw, "AnnotatedCRF"]])
#     # print(dta[[rw,"SupplementalDoc"]])
#     if(dta[[rw, "AnnotatedCRF"]] %eq% 'Y') {
#       ret[length(ret) + 1] <- glue(str1, leafid = dta[[rw, "LeafID"]])
#     }
#     else if (dta[[rw, "SupplementalDoc"]] %eq% 'Y') {
#       ret[length(ret) + 1] <- glue(str2, leafid = dta[[rw, "LeafID"]])
#     }
#   }
#   return(ret)
# }

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
                                 leafid = dta[[rw, "LeafID"]],
                                 leafrelpath = dta[[rw, "LeafRelPath"]],
                                 title = encodeMarkup(dta[[rw, "Title"]]))
  }
  return(ret)
}


# Final issue - what do with analysis_results_helper when analysisdataset is NA
# (but the variable is not)
# Func already handles opposite scenario and returns empty
get_analysis_results <- function(dta, wcdta) {
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
        <def:PDFPageRef PageRefs="302" Type="PhysicalRef"/>
      </def:DocumentRef>
      <arm:AnalysisResult
        OID="AR.{dispid}"
        ParameterOID="IT.{analysisdata}.{paramcd}"
        ResultIdentifier="{dispid}"
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
        <arm:Code>
{pgrmcode}
        </arm:Code>
        </arm:ProgrammingCode>
      </arm:AnalysisResult>
    </arm:ResultDisplay>'

  end <- '</arm:AnalysisResultDisplays>'

  wcstr <- '      <def:WhereClauseRef WhereClauseOID="WC.{wcoid}" /> '

  ret <- c(blk)
  for(rw in 1:nrow(dta)) {
#browser()
    sbst <- subset(wcdta, wcdta$WHERECLAUSEOID == dta[[rw, "WHERECLAUSEOID"]])
    wc <- ""
    if (nrow(sbst) > 0) {

      for (i in seq_len(nrow(sbst))) {

         wc <- paste0(wc, glue(wcstr, wcoid = paste0(sbst[[i, "WHERECLAUSEOID"]],
                                                    ".",  sbst[[i, "SEQ"]])), "\n")
      }

    }

    ret[length(ret) + 1] <- glue(resultDisplay,
                                 dispid = dta[[rw, "DISPLAYID"]],
                                 dispname = encodeMarkup(dta[[rw, "DISPLAYNAME"]]),
                                 analysisdata = dta[[rw, "ANALYSISDATASET"]],
                                 paramcd = trimws(dta[[rw, "PARAMCD"]]),
                                 reason = encodeMarkup(dta[[rw, "REASON"]]),
                                 purpose = encodeMarkup(dta[[rw, "PURPOSE"]]),
                                 resultname = dta[[rw, "RESULTNAME"]],
                                 WhereClauses = wc,
                                 analysisvars = analysis_results_helper(dta[[rw,
                                  "ANALYSISVARIABLES"]],
                                  dta[[rw, "ANALYSISDATASET"]]),
                                 document = encodeMarkup(dta[[rw, "DOCUMENTATION"]]),
                                 rleafid = ifelse(!is.na(dta[[rw, "REFLEAFID"]]),
                                                  paste0("LF.", dta[[rw, "REFLEAFID"]]), ""),
                                 context = encodeMarkup(dta[[rw, "CONTEXT"]]),
                                 pgrmcode = encodeMarkup(dta[[rw, "PROGRAMMINGCODE"]]))
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
                   analysisdata = dtaSetName,
                   varname = varRes[[1]][[i]])
      strHolder <- paste0(strHolder, temp,"\n")
    }
  }
  return(strHolder)
}
