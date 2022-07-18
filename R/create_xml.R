
# Create XML --------------------------------------------------------------

# test

library(common)


create_xml <- function(lst) {



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
    defs <- get_item_defs(lst[["VARIABLE_METADATA"]])
  else
    stop("Variable metadata is required.")

  val <- c()
  if ("VALUELEVEL_METADATA" %in% nms)
    val <- get_value_level(lst[["VALUELEVEL_METADATA"]])
  else
    stop("Table of Contents metadata is required.")

  comp <- c()
  if ("COMPUTATION_METHOD" %in% nms)
    comp <- get_computations(lst[["COMPUTATION_METHOD"]])
  else
    stop("Computation Method metadata is required.")

  cl <- c()
  if ("CODELISTS" %in% nms)
    cl <- get_code_lists(lst[["CODELISTS"]])
  else
    stop("Code List metadata is required.")

  whr <- c()
  if ("WHERE_CLAUSES" %in% nms)
    whr <- get_where(lst[["WHERE_CLAUSES"]])


  cmnts <- c()
  if ("COMMENTS" %in% nms)
    cmnts <- get_comments(lst[["COMMENTS"]])


  extl <- c()
  if ("EXTERNAL_LINKS" %in% nms)
    extl <- get_external_links(lst[["EXTERNAL_LINKS"]])

  ftr <- get_footer()


  ret <- c(hdr, val, grps, defs,  comp, cl, whr, cmnts, extl, ftr)



  return(ret)

}




# Subsections -------------------------------------------------------------

#' @import glue
#' @noRd
get_header <- function(dta) {

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
              desc = dta[["STUDYDESCRIPTION"]][1],
              protocol = dta[["PROTOCOLNAME"]][1],
              sn = dta[["STANDARD"]][1],
              sv = dta[["VERSION"]][1],
              stylesheet = dta[["STYLESHEET"]][1])


  return(ret)
}


#' @noRd
get_item_groups <- function(toc, vardt) {



}

#' @noRd
get_item_defs <- function(toc, vardt) {



}

#' @noRd
get_value_level <- function(dta) {


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
                                   domain =  sp[1, "DOMAIN"],
                                   variable = sp[1, "VARIABLE"])

      for (rw in seq_len(nrow(sp))) {


          whrc <- ""
          if (!is.na(sp[rw, "WHERECLAUSEOID"]))
            whrc <- glue(wcstr, wcoid = sp[rw, "WHERECLAUSEOID"])

          ret[length(ret) + 1] <- glue(str,
                                       domain =  sp[rw, "DOMAIN"],
                                       variable = sp[rw, "VARIABLE"],
                                       value =  sp[rw, "VALUENAME"],
                                       varnum =  sp[rw, "VARNUM"],
                                       mandatory =  sp[rw, "MANDATORY"],
                                       wc = whrc
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
                                 mthdOID = dta[rw, "COMPUTATIONMETHODOID"],
                                 label = dta[rw, "LABEL"],
                                 comp = dta[rw, "TYPE"],
                                 compMthd = dta[rw, "COMPUTATIONMETHOD"])
  }
  return(ret)

}

#' @noRd
get_code_lists <- function(dta) {
  # blk <- '<!-- ************************************************************ -->
  # <!-- Codelists are presented below                                -->
  # <!-- ************************************************************ -->'
  # listHead <- '<CodeList OID="CodeList.ACN"
  # Name="ACN"
  # DataType="text">'
  # endCL <- '</CodeList>'
  # item <- '<CodeListItem CodedValue="DOSE INCREASED" Rank="1">
  #       <Decode>
  #         <TranslatedText>DOSE INCREASED</TranslatedText>
  #       </Decode>
  #     </CodeListItem>'
  # dict <- '<ExternalCodeList Dictionary="MedDRA" Version="18.0"/>'
  #
  #
  # f <- list(as.factor(dta[["CODELISTNAME"]]))
  # splts <- split(dta, f)
  # print(splts)

}

#' @noRd
get_where <- function(dta) {

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


#' @import glue
#' @noRd
get_comments <- function(dta) {

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



#' @import common
#' @noRd
get_external_links <- function(dta) {
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

  # for(rw in seq_len(nrow(dta))) {
  #   # print(unclass(dta[rw, "AnnotatedCRF"])[1])
  #   # print(unclass(dta[rw, "SupplementalDoc"])[1])
  #   print(dta[rw, "AnnotatedCRF"][1] == "Y")
  #   print(dta[rw, "SupplementalDoc"][1] == "Y")
  #   if(dta[rw, "AnnotatedCRF"][1] == "Y") {print("balls")}
  #   # Fix the if block below it's a retarded statement if you think about it
  #
  #   if(is.na(dta[rw, "AnnotatedCRF"][1]) || is.null(dta[rw,"AnnotatedCRF"][1])) {
  #
  #   }
  #   else if(is.na(dta[rw, "SupplementalDoc"][1]) || is.null(dta[rw, "SupplementalDoc"][1])) {
  #
  #   }
  #
  #   if(is.na(dta[rw, "AnnotatedCRF"][1]) || is.null(dta[rw,"AnnotatedCRF"][1]) ||
  #      is.na(dta[rw, "SupplementalDoc"][1]) || is.null(dta[rw, "SupplementalDoc"][1])) {
  #   }
  #   else if(dta[rw, "AnnotatedCRF"][1] == "Y") {
  #     print("hi")
  #     ret[length(ret) + 1] <- glue(str1, leafid = dta[rw, "LeafID"])
  #   }
  #   else if(dta[rw, "SupplementalDoc"][1] == "Y") {
  #     ret[length(ret) + 1] <- glue(str2, leafid = dta[rw, "LeafID"])
  #   }
  # }




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

