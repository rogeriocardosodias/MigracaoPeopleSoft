&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define buffer cliente for ems5cad.cliente.

define variable i_cont as integer no-undo.

DEFINE VARIABLE i AS INTEGER     NO-UNDO.

define stream saida.

define temp-table tt_reg_000 no-undo
  field setid             as character format "X(5)"       initial "SHARE"
  field cust_id           as character format "X(15)"
  field cust_status       as character format "X(1)"       initial "A"
  field customer_type     as character format "X(1)"       initial "A"
  field since_dt          as date      format "99/99/9999" initial 01/01/1900
  field name1             as character format "X(40)"      
  field name2             as character format "X(40)" 
  field corporate_cust_id as character format "X(15)"
  field cur_rt_type       as character format "X(5)"       initial "CRRNT"
  field currency_cd       as character format "X(3)"       initial "BRL"
  field ship_to_flg       as character format "X(1)"       initial "N"
  field bill_to_flg       as character format "X(1)"       initial "N"
  field sold_to_flg       as character format "X(1)"       initial "N"
  field cust_field_c10_a  as character format "X(10)"     
  field cust_field_c30_a  as character format "X(30)"      
  field cust_field_c30_b  as character format "X(30)"     
  field protest_flag_brl  as character format "X(1)"       initial "N"
  field corporate_cust    as character format "X(1)"       initial "N"
  field num_id            as integer   
  index id is primary unique num_id.


define temp-table tt_reg_100 no-undo
  field support_team_cd   as character format "X(6)"       initial "ESPM01"
  field default_flag      as character format "X(1)"       initial "Y"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_200 no-undo
  field address_seq_num   as integer   format ">>>>9"      initial 1
  field descr             as character format "X(30)"      initial "Principal"
  field bill_to_addr      as character format "X(1)"       initial "Y"
  field ship_to_addr      as character format "X(1)"       initial "N"
  field sold_to_addr      as character format "X(1)"       initial "Y"
  field crspd_to_addr     as character format "X(1)"       initial "Y"
  field primary_bill_to   as character format "X(1)"       initial "Y"
  field primary_ship_to   as character format "X(1)"       initial "Y"
  field primary_sold_to   as character format "X(1)"       initial "Y"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_210 no-undo
  field effdt             as date      format "99/99/9999" initial "01/01/1901"
  field eff_status        as character format "X(1)"       initial "A"
  field alt_name1         as character format "X(40)"      initial ""
  field alt_name2         as character format "X(40)"      initial ""
  field language_cd       as character format "X(3)"       initial "POR"
  field country           as character format "X(3)"       initial "BRA"
  field address1          as character format "X(55)"       
  field address2          as character format "X(55)"       
  field address3          as character format "X(55)" 
  field address4          as character format "X(55)"
  field county            as character format "X(30)"       
  field state             as character format "X(6)"       
  field postal            as character format "X(12)"       
  field country_code      as character format "X(3)"       
  field phone             as character format "X(24)"       
  field extension         as character format "X(6)"       
  field fax               as character format "X(24)"       
  field estabid           as character format "X(12)"       
  field city_cd_bbl       as character format "X(4)"       
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_211 no-undo
  field std_id_num_qual_1  as character format "X(3)"       
  field std_id_num_1       as character format "X(35)"
  field std_id_num_qual_2  as character format "X(3)"       
  field std_id_num_2       as character format "X(35)"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_300 no-undo
  field effdt             as date      format "99/99/9999" initial "01/01/1901"
  field eff_status        as character format "X(1)"       initial "A"
  field pymnt_terms_cd    as character format "X(5)"       initial ""
  field partial_py_sw     as character format "X(1)"       initial "N"
  field collector         as character format "X(8)"       initial "ESPM01"
  field cr_analyst        as character format "X(8)"       initial "ESPM01"
  field payment_method    as character format "X(3)"       initial "BRL"
  field ar_specialist     as character format "X(8)"       initial "ESPM01"
  field bank_cd           as character format "X(5)"
  field bank_acct_key     as character format "X(4)"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_400 no-undo
  field effdt             as date      format "99/99/9999" initial "01/01/1901"
  field eff_status        as character format "X(1)"       initial "A"
  field ship_type_id      as character format "X(10)"      initial "RODOVIµRIO"
  field language_cd       as character format "X(3)"       initial "POR"
  field freight_terms     as character format "X(10)"      initial "CIF"
  field carrier_id        as character format "X(10)"      
  field single_ship_flag  as character format "X(1)"       initial "N"
  field nf_splt_mth_bbl   as character format "X(2)"       initial "SO"
  field lt_grp_id_bbl     as character format "X(10)"      initial "VENDAS"
  field tof_pbl           as character format "X(5)"       initial "ISENTO"
  field purch_prop_brl    as character format "X(3)"       initial "CON"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_500 no-undo
  field effdt             as date      format "99/99/9999" initial "01/01/1901"
  field eff_status        as character format "X(1)"       initial "A"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_600 no-undo
  field effdt             as date      format "99/99/9999" initial "01/01/1901"
  field eff_status        as character format "X(1)"       initial "A"
  field credit_check      as character format "X(1)"       initial "Y"
  field aging_id          as character format "X(5)"       initial "10-20"
  field aging_category    as character format "X(2)"       initial "2"
  field backlog_days      as integer   format "9999"       initial "0"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_800 no-undo
  field ship_to_cust_id as character format "X(15)" initial ""
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_b00 no-undo
  field effdt             as date      format "99/99/9999" initial "01/01/1901"
  field eff_status        as character format "X(1)"       initial "A"
  field crspd_cntct       as character format "X(1)"       initial "S"
  field language_cd       as character format "X(3)"       initial "POR"
  field num_id            as integer   
  index id is primary unique num_id.

define temp-table tt_reg_c00 no-undo
  field cust_grp_type     as character format "X(4)"       initial ""
  field customer_group    as character format "X(10)"      initial ""
  field num_id            as integer   
  index id is primary unique num_id.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

run pi_inicio.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi_gera_000) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_000 Procedure 
PROCEDURE pi_gera_000 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_000.
assign tt_reg_000.num_id = i_cont.

assign tt_reg_000.cust_id           = if avail pessoa_fisic then pessoa_fisic.cod_id_feder else pessoa_jurid.cod_id_feder
       tt_reg_000.cust_status       = "A"
       tt_reg_000.customer_type     = "A"
       tt_reg_000.since_dt          = cliente.dat_impl_clien
       tt_reg_000.name1             = cliente.nom_pessoa
       tt_reg_000.corporate_cust_id = if avail pessoa_fisic then pessoa_fisic.cod_id_feder else ""
       tt_reg_000.ship_to_flg       = if avail pessoa_fisic then "N" else "Y"
       tt_reg_000.bill_to_flg       = if avail pessoa_fisic then "N" else "Y"
       tt_reg_000.sold_to_flg       = if avail pessoa_fisic then "N" else "Y"
       tt_reg_000.protest_flag_brl  = "N"
       tt_reg_000.corporate_cust    = if avail pessoa_fisic then "N" else "Y".

end PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_100) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_100 Procedure 
PROCEDURE pi_gera_100 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_100.
assign tt_reg_100.num_id = i_cont.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_200) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_200 Procedure 
PROCEDURE pi_gera_200 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_200.
assign tt_reg_200.num_id = i_cont.

assign tt_reg_200.bill_to_addr    = "Y"
       tt_reg_200.ship_to_addr    = if avail pessoa_fisic then "N" else "Y"
       tt_reg_200.sold_to_addr    = "Y"
       tt_reg_200.crspd_to_addr   = "Y"
       tt_reg_200.primary_bill_to = "Y"
       tt_reg_200.primary_ship_to = if avail pessoa_fisic then "N" else "Y"
       tt_reg_200.primary_sold_to = "Y".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_210) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_210 Procedure 
PROCEDURE pi_gera_210 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_210.
assign tt_reg_210.num_id = i_cont.

assign tt_reg_210.address1       = (if avail pessoa_fisic then 
                                        (if num-entries(pessoa_fisic.nom_endereco," ") > 0 then 
                                          entry(1,pessoa_fisic.nom_endereco," ") 
                                         else "") 
                                     else (if num-entries(pessoa_jurid.nom_endereco," ") > 0 then 
                                          entry(1,pessoa_jurid.nom_endereco," ") 
                                         else "") )
       tt_reg_210.address2        = trim(replace((if avail pessoa_fisic then pessoa_fisic.nom_endereco else pessoa_jurid.nom_endereco),tt_reg_210.address1,""))
       tt_reg_210.address3        = ""
       tt_reg_210.address4        = (if avail pessoa_fisic then pessoa_fisic.nom_ender_compl  else pessoa_jurid.nom_ender_compl )
       tt_reg_210.county          = (if avail pessoa_fisic then pessoa_fisic.nom_bairro       else pessoa_jurid.nom_bairro      )
       tt_reg_210.state           = (if avail pessoa_fisic then pessoa_fisic.cod_unid_federac else pessoa_jurid.cod_unid_federac)
       tt_reg_210.postal          = (if avail pessoa_fisic then pessoa_fisic.cod_cep          else pessoa_jurid.cod_cep       )
       tt_reg_210.phone           = (if avail pessoa_fisic then replace(pessoa_fisic.cod_telefone,"-","") else replace(pessoa_jurid.cod_telefone,"-",""))
       tt_reg_210.extension       = (if avail pessoa_fisic then replace(pessoa_fisic.cod_ramal,"-","")    else replace(pessoa_jurid.cod_ramal_modem,"-",""))
       tt_reg_210.fax             = (if avail pessoa_fisic then replace(pessoa_fisic.cod_fax,"-","")      else replace(pessoa_jurid.cod_fax     ,"-",""))
       tt_reg_210.estabid         = (if avail pessoa_fisic then "NCONT" else (if pessoa_jurid.cod_id_estad_jurid = "ISENTO" or pessoa_jurid.cod_id_estad_jurid = "" then "NCONT" else "CONTR")).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_211) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_211 Procedure 
PROCEDURE pi_gera_211 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

          
create tt_reg_211.
assign tt_reg_211.num_id = i_cont.

assign tt_reg_211.std_id_num_qual_1 = (if avail pessoa_fisic then "CPF" else "CNJ")
       tt_reg_211.std_id_num_1      = (if avail pessoa_fisic then pessoa_fisic.cod_id_feder else pessoa_jurid.cod_id_feder)
       tt_reg_211.std_id_num_qual_2 = (if avail pessoa_fisic then "" else "IE")
       tt_reg_211.std_id_num_2      = (if avail pessoa_fisic then "" else (if pessoa_jurid.cod_id_estad = "" then "ISENTO" else pessoa_jurid.cod_id_estad)).     
                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_300) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_300 Procedure 
PROCEDURE pi_gera_300 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_300.
assign tt_reg_300.num_id = i_cont.

assign tt_reg_300.payment_method = (if avail pessoa_fisic then "CHK" else "BRL").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_400) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_400 Procedure 
PROCEDURE pi_gera_400 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_400.
assign tt_reg_400.num_id = i_cont.

assign tt_reg_400.tof_pbl = (if avail pessoa_fisic then "BI009" else (if pessoa_jurid.cod_id_estad_jurid = "ISENTO" or pessoa_jurid.cod_id_estad_jurid = "" then "BI009" else "BI001" )).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_500) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_500 Procedure 
PROCEDURE pi_gera_500 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_500.
assign tt_reg_500.num_id = i_cont.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_600) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_600 Procedure 
PROCEDURE pi_gera_600 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_600.
assign tt_reg_600.num_id = i_cont. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_800) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_800 Procedure 
PROCEDURE pi_gera_800 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_800.
assign tt_reg_800.num_id = i_cont.
             
assign tt_reg_800.ship_to_cust_id = (if avail pessoa_fisic then pessoa_fisic.cod_id_feder else "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_B00) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_B00 Procedure 
PROCEDURE pi_gera_B00 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_B00.
assign tt_reg_B00.num_id = i_cont.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_C00) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_C00 Procedure 
PROCEDURE pi_gera_C00 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

create tt_reg_C00.
assign tt_reg_C00.num_id = i_cont.

assign tt_reg_C00.cust_grp_type = (if avail pessoa_fisic then "PRC" else "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_imprime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_imprime Procedure 
PROCEDURE pi_imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each tt_reg_000 no-lock:

  put stream saida unformatted tt_reg_000.setid            ";"
                   tt_reg_000.cust_id          ";"
                   tt_reg_000.cust_status      ";"
                   tt_reg_000.customer_type    ";"
                   tt_reg_000.since_dt         ";"
                   tt_reg_000.name1            ";"
                   tt_reg_000.name2            ";"
                   tt_reg_000.corporate_cust_i ";"
                   tt_reg_000.cur_rt_type      ";"
                   tt_reg_000.currency_cd      ";"
                   tt_reg_000.ship_to_flg      ";"
                   tt_reg_000.bill_to_flg      ";"
                   tt_reg_000.sold_to_flg      ";"
                   tt_reg_000.cust_field_c10_a ";"
                   tt_reg_000.cust_field_c30_a ";"
                   tt_reg_000.cust_field_c30_b ";"
                   tt_reg_000.protest_flag_brl ";"
                   tt_reg_000.corporate_cust   ";"    .

  find first tt_reg_100 of tt_reg_000 no-lock no-error.
  if avail tt_reg_100 then
    put stream saida unformatted tt_reg_100.support_team_cd ";"
                     tt_reg_100.default_flag    ";"  .

  find first tt_reg_200 of tt_reg_000 no-lock no-error.
  if avail tt_reg_200 then
    put stream saida unformatted tt_reg_200.address_seq_num  ";"
                     tt_reg_200.descr            ";"
                     tt_reg_200.bill_to_addr     ";"
                     tt_reg_200.ship_to_addr     ";"
                     tt_reg_200.sold_to_addr     ";"
                     tt_reg_200.crspd_to_addr    ";"
                     tt_reg_200.primary_bill_to  ";"
                     tt_reg_200.primary_ship_to  ";"
                     tt_reg_200.primary_sold_to  ";"  .

  find first tt_reg_210 of tt_reg_000 no-lock no-error.
  if avail tt_reg_210 then
    put stream saida unformatted tt_reg_210.effdt        ";"   
                     tt_reg_210.eff_status   ";"   
                     tt_reg_210.alt_name1    ";"   
                     tt_reg_210.alt_name2    ";"   
                     tt_reg_210.language_cd  ";"   
                     tt_reg_210.country      ";"   
                     tt_reg_210.address1     ";"   
                     tt_reg_210.address2     ";"   
                     tt_reg_210.address3     ";"
                     tt_reg_210.address4     ";"
                     tt_reg_210.county       ";"   
                     tt_reg_210.state        ";"   
                     tt_reg_210.postal       ";"   
                     tt_reg_210.country_code ";"   
                     tt_reg_210.phone        ";"   
                     tt_reg_210.extension    ";"   
                     tt_reg_210.fax          ";"   
                     tt_reg_210.estabid      ";"   
                     tt_reg_210.city_cd_bbl  ";"  .

  find first tt_reg_211 of tt_reg_000 no-lock no-error.
  if avail tt_reg_211 then
    put stream saida unformatted tt_reg_211.std_id_num_qual_1 ";"
                     tt_reg_211.std_id_num_1      ";"
                     tt_reg_211.std_id_num_qual_2 ";"
                     tt_reg_211.std_id_num_2      ";"   .

  find first tt_reg_300 of tt_reg_000 no-lock no-error.
  if avail tt_reg_300 then
    put stream saida unformatted tt_reg_300.effdt          ";"
                     tt_reg_300.eff_status     ";"
                     tt_reg_300.pymnt_terms_cd ";"
                     tt_reg_300.partial_py_sw  ";"
                     tt_reg_300.collector      ";"
                     tt_reg_300.cr_analyst     ";"
                     tt_reg_300.payment_method ";"
                     tt_reg_300.ar_specialist  ";"
                     tt_reg_300.bank_cd        ";"
                     tt_reg_300.bank_acct_key  ";"   .

  find first tt_reg_400 of tt_reg_000 no-lock no-error.
  if avail tt_reg_400 then
    put stream saida unformatted tt_reg_400.effdt            ";"
                     tt_reg_400.eff_status       ";"
                     tt_reg_400.ship_type_id     ";"
                     tt_reg_400.language_cd      ";"
                     tt_reg_400.freight_terms    ";"
                     tt_reg_400.carrier_id       ";"
                     tt_reg_400.single_ship_flag ";"
                     tt_reg_400.nf_splt_mth_bbl  ";"
                     tt_reg_400.lt_grp_id_bbl    ";"
                     tt_reg_400.tof_pbl          ";"
                     tt_reg_400.purch_prop_brl   ";"   .

  find first tt_reg_500 of tt_reg_000 no-lock no-error.
  if avail tt_reg_500 then
    put stream saida unformatted tt_reg_500.effdt      ";"
                     tt_reg_500.eff_status ";" .

  find first tt_reg_600 of tt_reg_000 no-lock no-error.
  if avail tt_reg_600 then
    put stream saida unformatted tt_reg_600.effdt          ";"
                     tt_reg_600.eff_status     ";"
                     tt_reg_600.credit_check   ";"
                     tt_reg_600.aging_id       ";"
                     tt_reg_600.aging_category ";"
                     tt_reg_600.backlog_days   ";"  .

  find first tt_reg_800 of tt_reg_000 no-lock no-error.
  if avail tt_reg_800 then
    put stream saida unformatted tt_reg_800.ship_to_cust_id  ";".

  find first tt_reg_b00 of tt_reg_000 no-lock no-error.
  if avail tt_reg_b00 then
    put stream saida unformatted tt_reg_b00.effdt       ";"
                     tt_reg_b00.eff_status  ";"
                     tt_reg_b00.crspd_cntct ";"
                     tt_reg_b00.language_cd ";" .

  find first tt_reg_c00 of tt_reg_000 no-lock no-error.
  if avail tt_reg_c00 then
    put stream saida unformatted tt_reg_c00.cust_grp_type  ";"  
                     tt_reg_c00.customer_group ";" .

  put stream saida unformatted skip.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_inicio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_inicio Procedure 
PROCEDURE pi_inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each cliente where (can-find(first tit_acr where tit_acr.cdn_cliente = tit_acr.cdn_cliente
                                                 and tit_acr.dat_Transacao >= 01/01/2013 )) no-lock:

  find first pessoa_fisic where pessoa_fisic.num_pessoa_fisic = cliente.num_pessoa no-lock no-error.
  find first pessoa_jurid where pessoa_jurid.num_pessoa_jurid = cliente.num_pessoa no-lock no-error.
  
  if not avail pessoa_fisic and not avail pessoa_jurid then next.

  i = i + 1.
  disp i with frame a.
  pause 0.
  process events.
   
  assign i_cont = i_cont + 1.
  
  run pi_gera_000.
  run pi_gera_100.
  run pi_gera_200.
  run pi_gera_210.
  run pi_gera_211.
  run pi_gera_300.
  run pi_gera_400.
  run pi_gera_500.
  run pi_gera_600.
  run pi_gera_800.
  run pi_gera_B00.
  run pi_gera_C00.

end.
  
output stream saida to c:\temp\rel_clientes_txt no-convert.
run pi_imprime.
output stream saida close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

