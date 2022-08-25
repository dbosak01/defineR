
# <!-- ******************************************  -->
# <!-- OID conventions used in this example:       -->
# <!--    MetaDataVersion         = MDV.           -->
# <!--    def:leaf, leafID        = LF.            -->
# <!--    def:ValueListDef        = VL.            -->
# <!--    def:WhereClauseDef      = WC.            -->
# <!--    ItemGroupDef            = IG.            -->
# <!--    ItemDef                 = IT.            -->
# <!--    CodeList                = CL.            -->
# <!--    MethodDef               = MT.            -->
# <!--    def:CommentDef          = COM.           -->
# <!--    arm:ResultDisplay       = RD             -->
# <!--    arm:AnalysisResult      = AR             -->
# <!-- ******************************************  -->


# Create XML --------------------------------------------------------------

#' @title Create XML for SDTM
#' @description Function generates XML for the SDTM define.xml file.
#' @param lst A list of data frames that contain SDTM metadata.
#' @param version The version of the define XML to create.  Currently
#' only 2.0.0 is supported, which is the default.
#' @returns A vector of XML strings.
#' @noRd
create_sdtm_xml <- function(lst, version = "2.0.0") {

  if(!is.list(lst)) {
   stop("Metadata must provided in the form of a list")
  }

  ret <- NULL

  if (version == "2.0.0") {

   ret <- get_sdtm_xml_20(lst)
  } else {

   stop(paste0("Version ", version, " not supported."))
  }


  return(ret)

}

get_sdtm_xml_20 <- function(lst) {

  nms <- names(lst)

  hdr <- c()
  if ("DEFINE_HEADER_METADATA" %in% nms)
    hdr <- get_header(lst[["DEFINE_HEADER_METADATA"]])
  else
    stop("Header metadata is required.")

  grps <- c()
  if ("TOC_METADATA" %in% nms)
    grps <- get_item_groups(lst[["TOC_METADATA"]], lst[["VARIABLE_METADATA"]])
  else
    stop("Table of Contents metadata is required.")

  defs <- c()
  if ("VARIABLE_METADATA" %in% nms)
    defs <- get_item_defs(lst[["TOC_METADATA"]], lst[["VARIABLE_METADATA"]],
                          lst[["VALUELEVEL_METADATA"]])
  else
    stop("Variable metadata is required.")

  val <- c()
  if ("VALUELEVEL_METADATA" %in% nms)
    val <- get_value_level(lst[["VALUELEVEL_METADATA"]], lst[["WHERE_CLAUSES"]])
  else
    stop("Table of Contents metadata is required.")

  cl <- c()
  if ("CODELISTS" %in% nms)
    cl <- get_code_lists(lst[["CODELISTS"]])
  else
    stop("Code List metadata is required.")

  comp <- c()
  if ("COMPUTATION_METHOD" %in% nms)
    comp <- get_computations(lst[["COMPUTATION_METHOD"]])
  else
    stop("Computation Method metadata is required.")

  whr <- c()
  if ("WHERE_CLAUSES" %in% nms)
    whr <- get_where(lst[["WHERE_CLAUSES"]], lst[["VALUELEVEL_METADATA"]])


  cmnts <- c()
  if ("COMMENTS" %in% nms)
    cmnts <- get_comments(lst[["COMMENTS"]])


  extl <- c()
  if ("EXTERNAL_LINKS" %in% nms)
    extl <- get_external_links(lst[["EXTERNAL_LINKS"]])

  ftr <- get_footer()


  ret <- c(hdr, extl, val, whr, grps, defs, cl, comp,  cmnts, ftr)

  return(ret)

}


# Subsections -------------------------------------------------------------

#' @noRd
get_header <- function(dta) {
# Comment
  str <- '
  <?xml version="1.0" encoding="ISO-8859-1" ?>
    <?xml-stylesheet type="text/xsl" href="{stylesheet}"?>
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
      <MetaDataVersion OID="CDISC.SDTM-IG.3.2"
    Name="{study}, SDTM Data Definitions"
    Description="{study}, SDTM Data Definitions"
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


#' @noRd
get_item_groups <- function(toc, vardt) {
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
  '<ItemRef ItemOID="IT.{domain}.{varname}"
    OrderNumber="{varnum}"
    Mandatory="{manda}"
    {keyseq}{methodoid}Role="{role}"
    RoleCodeListOID="CodeList.rolecode"/>'

  ret<-vector()
  for(rw in 1:nrow(toc)) {
    ret[length(ret) + 1] <- glue(blk, name = toc[rw, "NAME"])
    ret[length(ret) + 1] <- glue(itemGroup,
                                 oid = toc[[rw, "OID"]],
                                 name = toc[[rw, "NAME"]],
                                 reps = toc[[rw, "REPEATING"]],
                                 purp = toc[[rw, "PURPOSE"]],
                                 isRef = toc[[rw, "ISREFERENCEDATA"]],
                                 struct = toc[[rw, "STRUCTURE"]],
                                 class = toc[[rw, "CLASS"]],
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
                                     methodoid = methodoidHolder,
                                     role = vardt[[varrow, "ROLE"]])
      }
    }

    ret[length(ret) + 1] <- endCom
    ret[length(ret) + 1] <- glue(groupEnd,
                                 name = toc[[rw, "NAME"]],
                                 loc = toc[[rw, "ARCHIVELOCATIONID"]])

  }
  return(ret)
}

#' @noRd
get_item_defs <- function(toc, vardt, valdt) {

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
      >
      <Description>
          <TranslatedText xml:lang="en">{label}</TranslatedText>
      </Description>
      <def:Origin Type="{origin}">
      </def:Origin>
      {vlevel}
    </ItemDef>'

  vstr <-
    '<ItemDef OID="IT.ADQSADAS.AVAL.ACITM01-ACITM14" Name="AVAL" SASFieldName="AVAL" DataType="integer" Length="8">
        <Description>
          <TranslatedText xml:lang="en">Analysis Value</TranslatedText>
        </Description>
        <def:Origin Type="Derived"/>
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

        strHolder <- ""


        if(!is.na(vardt[[varrow, "DISPLAYFORMAT"]])) {
          strHolder <- encodeMarkup(vardt[[varrow, "DISPLAYFORMAT"]])
        }
        else {
          strHolder <- ifelse(is.na(vardt[[varrow, "LENGTH"]]),
                              "", vardt[[varrow, "LENGTH"]])
        }

        sbst <- subset(valdt, "DOMAIN"==vardt[[varrow, "DOMAIN"]] &
                              "VARIABLE"==vardt[[varrow, "VARIABLE"]])

        valLevel <- ""
        vDefs <- ""
        if (nrow(sbst) > 0) {

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
                                         SASFieldName = vardt[[varrow, "SASFIELDNAME"]],
                                         DataType = vardt[[varrow, "TYPE"]],
                                         Length = vardt[[varrow, "LENGTH"]],
                                         Label = vardt[[varrow, "LABEL"]],
                                         Origin = vardt[[varrow, "ORIGIN"]]), "\n")

          }

        }

        ret[length(ret) + 1] <- glue(str,
                                   domain = vardt[[varrow, "DOMAIN"]],
                                   variable = vardt[[varrow, "VARIABLE"]],
                                   type = vardt[[varrow, "TYPE"]],
                                   length = ifelse(is.na(vardt[[varrow, "LENGTH"]]),
                                                   "", vardt[[varrow, "LENGTH"]]),
                                   display = strHolder,
                                   label = encodeMarkup(vardt[[varrow, "LABEL"]]),
                                   origin = encodeMarkup(vardt[[varrow, "ORIGIN"]]),
                                   vlevel = valLevel)

        ret[length(ret) + 1] <- vDefs
    }



  return(ret)


}

#' @noRd
get_value_level <- function(dta, wcdt) {


  blk <- '
  <!-- ******************************************* -->
  <!-- VALUE LEVEL LIST DEFINITION INFORMATION  ** -->
  <!-- ******************************************* -->\n'

  defstart <- ' <def:ValueListDef OID="VL.{domain}.{variable}">\n'
  defend <- ' </def:ValueListDef>\n'
  wcstr <- ' <def:WhereClauseRef WhereClauseOID="{wcoid}"/>\n'

  str <- '
    <ItemRef ItemOID="IT.{domain}.{variable}.{value}{where}"
      OrderNumber="{varnum}"
      Mandatory="{mandatory}"
      {methodoid}>
      {wc}
    </ItemRef>'


  f <- list(as.factor(dta[["DOMAIN"]]), as.factor(dta[["VARIABLE"]]))

  splts <- split(dta, f)

  ret <- c(blk)
  last_domain <- ""
  last_variable <- ""

  for (sp in splts) {

    if (nrow(sp)) {
      ret[length(ret) + 1] <- glue(defstart,
                                   domain =  sp[[1, "DOMAIN"]],
                                   variable = sp[[1, "VARIABLE"]])

      for (rw in seq_len(nrow(sp))) {


          whrc <- ""
          holder <- ""
          whre <- ""
          if (!is.na(sp[[rw, "WHERECLAUSEOID"]])) {

            wcnm <- sp[[rw, "WHERECLAUSEOID"]]
            splt <- strsplit(wcnm, ".", fixed = TRUE)[[1]]

            whre <- paste0(".", splt[length(splt)])

            fltr <- subset(wcdt, "WHERECLAUSEOID" == wcnm)

            for (i in seq_len(nrow(fltr))) {



              whrc <- paste0(whrc, glue(wcstr,
                                        wcoid = paste0("WC.",
                                                       fltr[[i, "WHERECLAUSEOID"]],
                                                       ".",
                                                       fltr[[i, "SEQ"]])), "\n")

            }

          } else {


            whrc <- paste0(whrc, glue(wcstr,
                                      wcoid = paste0("WC.",
                                                     sp[[rw, "DOMAIN"]],
                                                     ".",
                                                     sp[[rw, "VARIABLE"]],
                                                     ".",
                                                     sp[[rw, "VALUENAME"]]
                                                     )), "\n")

          }

          # Added 312, 315-316
          if(!is.na(sp[[rw, "COMPUTATIONMETHODOID"]]))
            holder <- paste0('MethodOID="MT.', sp[[rw, "COMPUTATIONMETHODOID"]], '"')

          ret[length(ret) + 1] <- glue(str,
                                       domain =  sp[[rw, "DOMAIN"]],
                                       variable = sp[[rw, "VARIABLE"]],
                                       value =  sp[[rw, "VALUENAME"]],
                                       varnum =  sp[[rw, "VARNUM"]],
                                       mandatory =  sp[[rw, "MANDATORY"]],
                                       methodoid = holder,
                                       wc = whrc,
                                       where = whre
                                       )

      }

      ret[length(ret) + 1] <- defend

    }

  }


  return(ret)
}

#' @noRd
get_computations <- function(dta) {

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
                                 mthdOID = paste0("MT.", dta[[rw, "COMPUTATIONMETHODOID"]]),
                                 label = encodeMarkup(dta[[rw, "LABEL"]]),
                                 comp = dta[[rw, "TYPE"]],
                                 compMthd = encodeMarkup(dta[[rw, "COMPUTATIONMETHOD"]]))
  }
  ret[length(ret) + 1] <- ""
  return(ret)

}

#' @noRd
get_code_lists <- function(dta) {
  blk <- '  <!-- ************************************************************ -->
  <!-- Codelists are presented below                                -->
  <!-- ************************************************************ -->'
  listHead <- '<CodeList OID="CL.{codelistname}"
  Name="{codelistname}"
  DataType="{dtype}">'
  endCL <- '</CodeList>'
  item <- '  <CodeListItem CodedValue="{codedval}" Rank="{rank}">
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
                                     dict = sp[rw, "CODELISTDICTIONARY"],
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

#' @noRd
get_where <- function(dta, valdta) {

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

    wcoid <- paste0("WC.", dta[[rw, "WHERECLAUSEOID"]], ".", dta[[rw, "SEQ"]])

    ret[length(ret) + 1] <- glue(str,
                                 oid = wcoid,
                                 sh = dta[[rw, "SOFTHARD"]],
                                 iod = paste0("IT.", dta[[rw, "ITEMOID"]]),
                                 comp = encodeMarkup(dta[[rw, "COMPARATOR"]]),
                                 val = encodeMarkup(dta[[rw, "VALUES"]]))
  }


  for (rw in seq_len(nrow(valdta))) {


    if (is.na(valdta[[rw, "WHERECLAUSEOID"]])) {

      wcoid <- paste0("WC.", valdta[[rw, "DOMAIN"]], ".", valdta[[rw, "VARIABLE"]],
                      ".", valdta[[rw, "VALUENAME"]])

      ioid <- paste0("IT.", valdta[[rw, "DOMAIN"]], ".", valdta[[rw, "VALUEVAR"]])

      ret[length(ret) + 1] <- glue(str,
                                   oid = wcoid,
                                   sh = "Soft",
                                   iod = ioid,
                                   comp = "EQ",
                                   val = valdta[[rw, "VALUENAME"]])


    }


  }



  return(ret)
}


#' @noRd
get_comments <- function(dta) {

  blk <- '
  <!-- ******************************** -->
  <!-- COMMENTS DEFINITION SECTION      -->
  <!-- ******************************** -->'

  str <-'<def:CommentDef OID="COM.{oid}">
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



#' @noRd
get_external_links <- function(dta) {
  blk <- '
  <!-- ******************************************* -->
  <!-- EXTERNAL DOCUMENT REFERENCE             *** -->
  <!-- ******************************************* -->'

  str1 <- '<def:AnnotatedCRF>
      <def:DocumentRef leafID="LF.{leafid}"/>
    </def:AnnotatedCRF>\n\n'

  str2 <- '<def:SupplementalDoc>
      <def:DocumentRef leafID="LF.{leafid}"/>
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

#' @noRd
get_footer <- function() {

  ret <- c()

  ret[length(ret) + 1] <- "</MetaDataVersion>"
  ret[length(ret) + 1] <- "</Study>"
  ret[length(ret) + 1] <- "</ODM>"

  return(ret)
}

