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

define buffer fornecedor for emscad.fornecedor.

define variable i_cont as integer no-undo.
define variable i      as integer no-undo.

define stream saida.

def temp-table tt_reg_000 no-undo
    fields vendor_id           as character format "X(10)" 
    fields vndr_name_shrt_usr  as character format "X(10)"
    fields name1               as character format "X(40)"
    fields name2               as character format "X(40)"
    fields vendor_class        as character format "X(1)" 
    fields remit_vendor        as character format "X(10)"
    fields prim_addr_seq_num   as integer   format ">>>>9" initial 1
    fields corporate_vendor    as character format "X(10)"
    fields wthd_sw             as character format "X(1)"
    fields purch_prop_brl      as character format "X(3)" 
    fields estabid             as character format "X(12)"
    fields collect_cod_brl     as character format "X(4)"
    fields num_id              as integer
    index i1 is  primary num_id.

def temp-table tt_reg_100 no-undo
    fields cpf_brl                 as character format "X(15)" 
    fields cgc_brl                 as character format "X(18)"
    fields ie_brl                  as character format "X(20)" 
    fields im_brl                  as character format "X(20)" 
    fields ipi_contrib_brl         as character format "X(1)"
    fields icms_contrib_brl        as character format "X(1)"
    fields cbo_brl                 as character format "X(7)"
    fields pis_contrib_brl         as character format "X(1)"
    fields cofins_contrib_brl  as character format "X(1)"
    fields num_id               as integer
    index i1 is  primary num_id.


def temp-table tt_reg_110 no-undo
    fields emailid                 as character format "X(70)" 
    fields address1_1          as character     format "X(55)" 
    fields address2                as character format "X(55)" 
    fields address3                as character format "X(55)" 
    fields address4                as character format "X(55)" 
    fields city_1              as character     format "X(30)" 
    fields num1                as character     format "X(6)" 
    fields county_1                as character format "X(30)" 
    fields state_1                 as character format "X(6)" 
    fields postal_1                as character format "X(12)" 
    fields city_cd_bbl         as character     format "X(4)"
    field num_id               as integer
    index i1 is  primary num_id.

def temp-table tt_reg_111 no-undo
    fields phone_type          as character     format "X(4)"
    fields phone               as character     format "X(24)"
    fields extension           as character     format "X(6)"
    field num_id               as integer
    index i1 is  primary num_id.

def temp-table tt_reg_210 no-undo
    fields eff_status_1        as character     format "X(1)"
    fields contact_name        as character     format "X(50)" 
    fields contact_type        as character     format "X(1)"  
    fields url                 as character format "X(254)"
    fields descr               as character     format "X(30)" 
    fields emailid_1           as character     format "X(70)"
    field num_id               as integer
    index i1 is  primary num_id.

define temp-table tt_reg_211 no-undo
    fields phone_type_1        as character     format "X(4)"  
    fields phone_1             as character     format "X(24)" 
    fields extension_1         as character     format "X(6)"
    field num_id               as integer
    index i1 is  primary num_id.

define temp-table tt_reg_320 no-undo
    fields remit_vendor        as character format "X(10)"
    field num_id               as integer
    index i1 is  primary num_id.

define temp-table tt_reg_321 no-undo
    fields eff_status          as character format "X(1)" 
    fields currency_cd         as character format "X(3)" 
    fields cur_rt_type         as character format "X(5)" 
    fields pymnt_hold          as character format "X(1)" 
    fields pymnt_group_cd          as character format "X(2)"
    field num_id               as integer
    index i1 is  primary num_id.

define temp-table tt_reg_322 no-undo
    fields bank_acct_seq_nbr   as integer   format ">>9" 
    fields descr_5             as character     format "X(30)" 
    fields bnk_id_nbr          as character     format "X(20)" 
    fields branch_id           as character     format "X(10)" 
    fields bank_acct_type          as character format "X(2)"  
    fields bank_account_num        as character format "X(35)" 
    fields check_digit         as character     format "X(2)"  
    fields beneficiary_bank        as character format "X(30)" 
    fields benef_branch        as character     format "X(30)" 
    fields country_2           as character     format "X(3)"   
    fields branch_chk_dig_brl  as character     format "X(2)"
    field num_id               as integer
    index i1 is  primary num_id.

define temp-table tt_reg_327 no-undo
    fields wthd_entity         as character     format "x(5)" initial ""  
    fields wthd_type           as character     format "x(5)" initial ""  
    fields wthd_jur_cd         as character     format "x(5)" initial ""  
    fields wthd_ind                as character format "x(1)" initial ""  
    fields wthd_rule           as character     format "x(5)" initial ""  
    fields hold_pay_ind        as character     format "x(1)" initial ""  
    fields hold_pay_flg        as character     format "x(1)" initial ""
    fields wthd_flg                as character format "x(1)" initial ""
    fields default_jur_flg_0   as character     format "x(1)" initial ""
    fields default_class_1         as character format "x(5)" initial ""
    fields vendor_category_0   as character     format "x(5)" initial ""  
    fields wthd_condition          as character format "x(1)" initial ""  
    fields remit_wthd_to_flg   as character     format "x(1)" initial ""  
    fields depend_num_brl_0        as integer   format ">>>>9"
    field num_id               as integer
    index i1 is  primary num_id.

define temp-table tt_reg_328 no-undo
    fields wthd_entity_1           as character format "X(5)" initial ""   
    fields effdt               as date      format "99/99/99999" initial ?
    fields address_seq_num_2   as integer   format ">>>>9"
    field num_id               as integer
    index i1 is  primary num_id.

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

assign tt_reg_000.vendor_id           = string(fornecedor.cdn_fornecedor)
       tt_reg_000.vndr_name_shrt_usr  = fornecedor.nom_abrev
       tt_reg_000.name1               = fornecedor.nom_pessoa
       tt_reg_000.name2               = fornecedor.nom_pessoa
       tt_reg_000.vendor_class        = (if fornecedor.cod_grp_fornec = "40" then "e" else "r")
       tt_reg_000.remit_vendor        = tt_reg_000.vendor_id
       tt_reg_000.prim_addr_seq_num   = 1
       tt_reg_000.corporate_vendor    = tt_reg_000.vendor_id
       tt_reg_000.wthd_sw             = (if fornec_financ.log_retenc_impto then "Y" else "N")
       tt_reg_000.purch_prop_brl      = "" /* verificar regis */
       tt_reg_000.estabid             = "" /* verificar regis */
       tt_reg_000.collect_cod_brl     = "" /* verificar regis */  .

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

assign tt_reg_100i.cpf_brl             = if avail pessoa_fisic then pessoa_fisic.cod_id_feder else ""
       tt_reg_100i.cgc_brl             = if avail pessoa_jurid then pessoa_jurid.cod_id_feder else "" 
       tt_reg_100i.ie_brl              = if avail pessoa_fisic then "" else (if avail pessoa_jurid then (if pessoa_jurid.cod_id_estad_jurid <> "" then pessoa_jurid.cod_id_estad_jurid else "Isento") else "")
       tt_reg_100i.im_brl              = if avail pessoa_fisic then "" else (if avail pessoa_jurid then (if pessoa_jurid.cod_id_munic_jurid <> "" then pessoa_jurid.cod_id_munic_jurid else "Isento") else "")
       tt_reg_100i.ipi_contrib_brl     = 
       tt_reg_100i.icms_contrib_brl    = 
       tt_reg_100i.cbo_brl             = 
       tt_reg_100i.pis_contrib_brl     = 
       tt_reg_100i.cofins_contrib_brl  = .
                                                                                      



40	100	ipi_contrib_brl	contribuinte de ipi	character	1	y - caso seja contribuinte
n - caso n∆o seja contribuinte (ex: governo e estrangeiros)
41	100	icms_contrib_brl	contribuinte de icms	character	1	y - caso seja contribuinte
n - caso n∆o seja contribuinte (ex: governo e estrangeiros)

42	100	cbo_brl	classificaá∆o ocupaá‰es brasil	character	7	conforme setup
43	100	pis_contrib_brl	contribuinte de pis	character	1	y - caso seja contribuinte
n - caso n∆o seja contribuinte (ex: governo e estrangeiros)
44	100	cofins_contrib_brl	contribuinte do cofins	character	1	y - caso seja contribuinte
n - caso n∆o seja contribuinte (ex: governo e estrangeiros)



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_110) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_110 Procedure 
PROCEDURE pi_gera_110 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_111) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_111 Procedure 
PROCEDURE pi_gera_111 :
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

&IF DEFINED(EXCLUDE-pi_gera_320) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_320 Procedure 
PROCEDURE pi_gera_320 :
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

&IF DEFINED(EXCLUDE-pi_gera_321) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_321 Procedure 
PROCEDURE pi_gera_321 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_gera_322) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_322 Procedure 
PROCEDURE pi_gera_322 :
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

&IF DEFINED(EXCLUDE-pi_gera_327) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_327 Procedure 
PROCEDURE pi_gera_327 :
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

&IF DEFINED(EXCLUDE-pi_gera_328) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_328 Procedure 
PROCEDURE pi_gera_328 :
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

for each fornecedor where (can-find(first tit_ap where tit_ap.cdn_fornecedor = tit_ap.cdn_fornecedor
                                                   and tit_ap.dat_Transacao >= 01/01/2013 )) no-lock:

  find first pessoa_fisic where pessoa_fisic.num_pessoa_fisic = fornecedor.num_pessoa no-lock no-error.
  find first pessoa_jurid where pessoa_jurid.num_pessoa_jurid = fornecedor.num_pessoa no-lock no-error.
  
  if not avail pessoa_fisic and not avail pessoa_jurid then next.

  find first fornec_financ of fornecedor no-lock no-error.
  if not avail fornec_financ then next.

  i = i + 1.
  disp i with frame a.
  pause 0.
  process events.
   
  assign i_cont = i_cont + 1.
  
  run pi_gera_000.
  run pi_gera_100.
  run pi_gera_110.
  run pi_gera_111.
  run pi_gera_210.
  run pi_gera_211.
  run pi_gera_320.
  run pi_gera_321.
  run pi_gera_322.
  run pi_gera_327.
  run pi_gera_328.

end.
  
output stream saida to c:\temp\rel_fornec_txt no-convert.
run pi_imprime.
output stream saida close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

