
#' @title Create XML for ADAM
#' @description Function generates XML for the ADAM define.xml file.
#' @param lst A list of data frames that contain SDTM metadata.
#' @param version The version of the define XML to create.  Currently
#' only 2.0.0 is supported, which is the default.
#' @returns A vector of XML strings.
#' @export
create_adam_xml <- function(lst, version) {



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
    defs <- get_item_defs_adam(lst[["TOC_METADATA"]], lst[["VARIABLE_METADATA"]])
  else
    stop("Variable metadata is required.")

  val <- c()
  if ("VALUELEVEL_METADATA" %in% nms)
    val <- get_value_level_adam(lst[["VALUELEVEL_METADATA"]])
  else
    stop("Table of Contents metadata is required.")

  comp <- c()
  if ("COMPUTATION_METHOD" %in% nms)
    comp <- get_computations_adam(lst[["COMPUTATION_METHOD"]])
  else
    stop("Computation Method metadata is required.")

  cl <- c()
  if ("CODELISTS" %in% nms)
    cl <- get_code_lists_adam(lst[["CODELISTS"]])
  else
    stop("Code List metadata is required.")

  whr <- c()
  if ("WHERE_CLAUSES" %in% nms)
    whr <- get_where_adam(lst[["WHERE_CLAUSES"]])


  cmnts <- c()
  if ("COMMENTS" %in% nms)
    cmnts <- get_comments_adam(lst[["COMMENTS"]])


  extl <- c()
  leafdefs <- c()
  if ("EXTERNAL_LINKS" %in% nms)
  {
    extl <- get_external_links(lst[["EXTERNAL_LINKS"]])
    leafdefs <- get_leaf_definitions_adam(lst[["EXTERNAL_LINKS"]])
  }

  analysis <- c()
  if("ANALYSIS_RESULTS" %in% nms)
    analysis <- get_analysis_results_adam(lst[["ANALYSIS_RESULTS"]])

  ftr <- get_footer()


  ret <- c(hdr, val, grps, defs,  comp, cl, whr, cmnts, extl, leafdefs, analysis, ftr)



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
              desc = dta[["STUDYDESCRIPTION"]][1],
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
    '  <ItemGroupDef OID="{oid}"
      Domain="{name}"
      Name="{name}"
      Repeating="{reps}"
      Purpose="{purp}"
      IsReferenceData="{isRef}"
      SASDatasetName="{name}"
      def:Structure="{struct}"
      def:Class="{class}"
      def:CommentOID="{commentoid}"
      def:ArchiveLocationID="Location.{name}">
      <Description>
        <TranslatedText xml:lang="en">{label}</TranslatedText>
      </Description>'

  endCom <-
    ' <!-- **************************************************** -->
      <!-- def:leaf details for hypertext linking the dataset   -->
      <!-- **************************************************** -->'

  groupEnd <-
    '<def:leaf ID="Location.{name}" xlink:href="{loc}.xpt">
        <def:title>{loc}.xpt </def:title>
      </def:leaf>
    </ItemGroupDef>'

  itemRefs <-
    '<ItemRef ItemOID="{domain}.{varname}"
    OrderNumber="{varnum}"
    Mandatory="{manda}"
    {keyseq}{methodoid}/>'

  ret<-vector()
  for(rw in 1:nrow(toc)) {
    ret[length(ret) + 1] <- glue(blk, name = toc[rw, "NAME"])
    ret[length(ret) + 1] <- glue(itemGroup,
                                 oid = toc[rw, "OID"],
                                 name = toc[rw, "NAME"],
                                 reps = toc[rw, "REPEATING"],
                                 purp = toc[rw, "PURPOSE"],
                                 isRef = toc[rw, "ISREFERENCEDATA"],
                                 struct = toc[rw, "STRUCTURE"],
                                 class = toc[rw, "CLASS"],
                                 commentoid = toc[rw, "COMMENTOID"],
                                 label = toc[rw, "LABEL"])

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
          methodoidHolder <- paste0('MethodOID="',vardt[[varrow, "COMPUTATIONMETHODOID"]],'"\n')
        }




        # itemref
        ret[length(ret) + 1] <- glue(itemRefs,
                                     domain = vardt[varrow, "DOMAIN"],
                                     varname = vardt[varrow, "VARIABLE"],
                                     varnum = vardt[varrow, "VARNUM"],
                                     manda = vardt[varrow, "MANDATORY"],
                                     keyseq = keyHolder,
                                     methodoid = methodoidHolder)
      }
    }

    ret[length(ret) + 1] <- endCom
    ret[length(ret) + 1] <- glue(groupEnd,
                                 name = toc[rw, "NAME"],
                                 loc = toc[rw, "ARCHIVELOCATIONID"])

  }
  return(ret)
}



#' @import glue
#' @noRd
get_item_defs_adam <- function(toc, vardt) {

  blk <- '<!-- ************************************************************ -->
  <!-- The details of each variable is here for all domains         -->
  <!-- ************************************************************ -->'
  str <-
    ' <ItemDef OID="{domain}.{variable}"
      Name="{variable}"
      SASFieldName="{variable}"
      DataType="{type}"
      Length="{length}"
      def:DisplayFormat="{display}"
      >
      <Description>
          <TranslatedText xml:lang="en">{label}</TranslatedText>
      </Description>
      {codelistref}<def:Origin Type="{origin}">
        {internals}
      </def:Origin>
    </ItemDef>'

  # double check efficiency for outer loop
  ret <- c(blk)
  for(rw in 1:nrow(toc)) {
    for(varrow in 1:nrow(vardt)) {
      if(toc[[rw, "NAME"]] %eq% vardt[[varrow, "DOMAIN"]]) {
        strHolder <- ""
        if(!is.na(vardt[varrow, "DISPLAYFORMAT"])) {
          strHolder <- vardt[[varrow, "DISPLAYFORMAT"]]
        }
        else {
          strHolder <- vardt[[varrow, "LENGTH"]]
        }
        codeListHolder <- ""
        if(!is.na(vardt[varrow, "CODELISTNAME"])) {
          codeListHolder <- '<CodeListRef CodeListOID="CodeList.{codelist}"/>\n'
          codeListHolder <- glue(codeListHolder, codelist = vardt[varrow, "CODELISTNAME"])
        }
        valueListHolder <- ""
        # #
        # if(!is.na(vardt[varrow, "VALUELIST"])) {
        #   valueListHolder <- '<def:ValueListRef ValueListOID="VL.ADTTE.CNSR"/>'
        # }
        internalHolder <- ""
        originHolder <- ""
        if(vardt[[varrow, "ORIGIN"]] %eq% 'Assigned' ||
           vardt[[varrow, "ORIGIN"]] %eq% 'Derived') {
          originHolder <- vardt[[varrow, "ORIGIN"]]
        }
        else {
          internalHolder <- '<Description>
          <TranslatedText xml:lang="en">{origin}</TranslatedText>
        </Description>'
          internalHolder <- glue(internalHolder, origin = vardt[varrow, "ORIGIN"])
          originHolder <- "Predecessor"
        }
        ret[length(ret) + 1] <- glue(str,
                                     domain = vardt[varrow, "DOMAIN"],
                                     variable = vardt[varrow, "VARIABLE"],
                                     type = vardt[varrow, "TYPE"],
                                     length = vardt[varrow, "LENGTH"],
                                     display = strHolder,
                                     internals = internalHolder,
                                     codelistref = codeListHolder,
                                     label = vardt[varrow, "LABEL"],
                                     origin = originHolder)
      }
    }
  }
  return(ret)


}



#' @noRd
get_value_level_adam <- function(dta) {


  blk <- '
  <!-- ******************************************* -->
  <!-- VALUE LEVEL LIST DEFINITION INFORMATION  ** -->
  <!-- ******************************************* -->\n'

  defstart <- ' <def:ValueListDef OID="VL.{domain}.{variable}">\n'
  defend <- ' </def:ValueListDef>\n'
  wcstr <- ' <def:WhereClauseRef WhereClauseOID="{wcoid}"/>\n'

  str <- '
    <ItemRef ItemOID="VL.{domain}.{variable}.{value}"
      OrderNumber="{varnum}"
      Mandatory="{mandatory}">
      {methodoid}{wc}
    </ItemRef>'


  f <- list(as.factor(dta[["DOMAIN"]]), as.factor(dta[["VARIABLE"]]))

  splts <- split(dta, f)

  ret <- c(blk)
  last_domain <- ""
  last_variable <- ""

  for (sp in splts) {

    if (nrow(sp)) {
      ret[length(ret) + 1] <- glue(defstart,
                                   domain =  sp[1, "DOMAIN"],
                                   variable = sp[1, "VARIABLE"])

      for (rw in seq_len(nrow(sp))) {


        whrc <- ""
        holder <- ""
        if (!is.na(sp[rw, "WHERECLAUSEOID"]))
          whrc <- glue(wcstr, wcoid = sp[rw, "WHERECLAUSEOID"])
        # Added 312, 315-316
        if(!is.na(sp[rw, "COMPUTATIONMETHODOID"]))
          holder <- paste0('MethodOID="', sp[rw, "COMPUTATIONMETHODOID"], '">\n')

        ret[length(ret) + 1] <- glue(str,
                                     domain =  sp[rw, "DOMAIN"],
                                     variable = sp[rw, "VARIABLE"],
                                     value =  sp[rw, "VALUENAME"],
                                     varnum =  sp[rw, "VARNUM"],
                                     mandatory =  sp[rw, "MANDATORY"],
                                     methodoid = holder,
                                     wc = whrc
        )

      }

      ret[length(ret) + 1] <- defend

    }

  }


  return(ret)
}

# identical function
#' @noRd
get_computations_adam <- function(dta) {

  blk <-'  <!-- ******************************************* -->
  <!-- COMPUTATIONAL METHOD INFORMATION        *** -->
  <!-- ******************************************* -->'

  str <- '<MethodDef OID="{mthdOID}" Name="{label}" Type="{comp}">
        <Description>
          <TranslatedText xml:lang="en">{compMthd}</TranslatedText>
        </Description>
      </MethodDef>'

  ret <- c(blk)
  for(rw in seq_len(nrow(dta))) {
    ret[length(ret) + 1] <- glue(str,
                                 mthdOID = dta[rw, "COMPUTATIONMETHODOID"],
                                 label = dta[rw, "LABEL"],
                                 comp = dta[rw, "TYPE"],
                                 compMthd = dta[rw, "COMPUTATIONMETHOD"])
  }
  ret[length(ret) + 1] <- ""
  return(ret)

}

# Changed Rank to OrderNumber, otherwise identical
#' @noRd
get_code_lists_adam <- function(dta) {
  blk <- '  <!-- ************************************************************ -->
  <!-- Codelists are presented below                                -->
  <!-- ************************************************************ -->'
  listHead <- '<CodeList OID="CodeList.{codelistname}"
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
                                 codelistname = sp[1, "CODELISTNAME"],
                                 dtype = sp[1, "TYPE"])
    for(rw in seq_len(nrow(sp))) {
      if(!is.na(sp[[rw, "CODELISTDICTIONARY"]])) {
        ret[length(ret) + 1] <- glue(dictcl,
                                     dict = sp[rw, "CODELISTDICTIONARY"],
                                     clver = sp[rw, "CODELISTVERSION"])
      }
      else {
        ret[length(ret) + 1] <- glue(item,
                                     codedval = sp[rw, "CODEDVALUE"],
                                     rank = sp[rw, "RANK"],
                                     translated = sp[rw, "TRANSLATED"])
      }
    }
    ret[length(ret) + 1] <- endCL
  }
  return(ret)
}


# Identical function
#' @noRd
get_where_adam <- function(dta) {

  blk <- '
  <!-- ****************************************************************** -->
  <!-- WhereClause Definitions Used/Referenced in Value List Definitions) -->
  <!-- ****************************************************************** -->'

  str <- '<def:WhereClauseDef OID="{oid}">
    <RangeCheck SoftHard="{sh}" def:ItemOID="{iod}" Comparator="{comp}">
      <CheckValue>{val}</CheckValue>
      </RangeCheck>
      </def:WhereClauseDef>'

  ret <- c(blk)

  for (rw in seq_len(nrow(dta))) {

    ret[length(ret) + 1] <- glue(str,
                                 oid = dta[rw, "WHERECLAUSEOID"],
                                 sh = dta[rw, "SOFTHARD"],
                                 iod = dta[rw, "ITEMOID"],
                                 comp = dta[rw, "COMPARATOR"],
                                 val = dta[rw, "VALUES"])
  }



  return(ret)
}



# Identical function
#' @noRd
get_comments_adam <- function(dta) {

  blk <- '
  <!-- ******************************** -->
  <!-- COMMENTS DEFINITION SECTION      -->
  <!-- ******************************** -->'

  str <-'<def:CommentDef OID="{oid}">
      <Description>
        <TranslatedText xml:lang="en">{comment}</TranslatedText>
      </Description>
    </def:CommentDef>'


  ret <- c(blk)

  for (rw in seq_len(nrow(dta))) {

    ret[length(ret) + 1] <- glue(str,
                                 oid = dta[rw, "COMMENTOID"],
                                 comment = dta[rw, "COMMENT"])

  }

  return(ret)


}

# Identical Function
#' @noRd
get_external_links_adam <- function(dta) {
  blk <- '
  <!-- ******************************************* -->
  <!-- EXTERNAL DOCUMENT REFERENCE             *** -->
  <!-- ******************************************* -->'

  str1 <- '<def:AnnotatedCRF>
      <def:DocumentRef leafID="{leafid}"/>
    </def:AnnotatedCRF>\n\n'

  str2 <- '<def:SupplementalDoc>
      <def:DocumentRef leafID="{leafid}"/>
    </def:SupplementalDoc>\n\n'

  ret <- c(blk)


  for(rw in seq_len(nrow(dta))) {
    # print(dta[[rw, "AnnotatedCRF"]])
    # print(dta[[rw,"SupplementalDoc"]])
    if(dta[[rw, "AnnotatedCRF"]] %eq% 'Y') {
      # print("hi")
      ret[length(ret) + 1] <- glue(str1, leafid = dta[rw, "LeafID"])
    }
    else if (dta[[rw, "SupplementalDoc"]] %eq% 'Y') {
      ret[length(ret) + 1] <- glue(str2, leafid = dta[rw, "LeafID"])
    }
  }
  return(ret)
}

# Stub for leaf definition
get_leaf_definitions_adam <- function(dta) {

  blk <- "    <!-- ******************************************* -->
    <!-- LEAF DEFINITION SECTION                 *** -->
    <!-- ******************************************* -->"

  leafdefs <- '    <def:leaf ID="{leafid}"
      xlink:href="{leafrelpath}">
      <def:title>{title}</def:title>
    </def:leaf>\n'
  ret <- c(blk)
  for(rw in 1:nrow(dta)) {
    ret[length(ret) + 1] <- glue(leafdefs,
                                 leafid = dta[rw, "LeafID"],
                                 leafrelpath = dta[rw, "LeafRelPath"],
                                 title = dta[rw, "Title"])
  }
  return(ret)
}


# Stub for analysis metadata
get_analysis_results_adam <- function(dta) {
  blk <-     '<!-- ************************************************************ -->
    <!-- Analysis Results MetaData are Presented Below                -->
    <!-- ************************************************************ -->
    <arm:AnalysisResultDisplays>'

  resultDisplay <- '    <arm:ResultDisplay OID="RD.Table_14.1.1" Name="Table 14.1.1">
      <Description>
          <TranslatedText xml:lang="en">Summary of Demographics (ITT Population)</TranslatedText>
      </Description>
      <def:DocumentRef leafID="Table_14.1.1">
        <def:PDFPageRef PageRefs="302" Type="PhysicalRef"/>
      </def:DocumentRef>
      <arm:AnalysisResult
        OID="Table_14.1.1"
        ParameterOID="ADSL.PARAMCD"
        ResultIdentifier="Table_14.1.1"
        AnalysisReason="SPECIFIED IN SAP"
        AnalysisPurpose="Comparisons of baseline characteristics by treatment group">
        <Description>
          <TranslatedText xml:lang="en">Summary of Demographics</TranslatedText>
        </Description>
        <arm:AnalysisDatasets>
          <arm:AnalysisDataset ItemGroupOID="ADSL" >
          <def:WhereClauseRef WhereClauseOID="WC.ITTFL" />

          <arm:AnalysisVariable ItemOID="ADSL.AGE"/>
          <arm:AnalysisVariable ItemOID="ADSL.AGEGR1"/>
          <arm:AnalysisVariable ItemOID="ADSL.SEX"/>
          <arm:AnalysisVariable ItemOID="ADSL.RACE"/>
          </arm:AnalysisDataset>
        </arm:AnalysisDatasets>
        <arm:Documentation>
        <Description>
          <TranslatedText xml:lang="en">Rates and chi-squared tests of categorical demographic variables </TranslatedText>
        </Description>
        <def:DocumentRef  leafID="SAP_Section_9.1.1">
        </def:DocumentRef>
        </arm:Documentation>

        <arm:ProgrammingCode Context="SAS Version 9.4">
        <arm:Code>
PROC FREQ DATA=ADSL;
  where ittfl=&quot;Y&quot;;
  tables trt01pn * (agegr1 sex race) / cmh;
  run;
        </arm:Code>
        </arm:ProgrammingCode>
      </arm:AnalysisResult>
    </arm:ResultDisplay>'

  end <- '</arm:AnalysisResultDisplays>'


}