*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CAD_CTPG.PRG
* Data        : 01/01/1999
* Objetivo    : Cadastro de Contas a Pagar
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CAD_CTPG()
SAVE SCREEN TO TELA04
PTIT( "CADASTRO DE CONTAS A PAGAR")
SELE 5
USE CLAS_PRO INDEX CLAS_PRO,DESC_PRO
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 3
USE STATUS
GOTO TOP
SELE 2
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 1
USE CPAGAR INDEX CPAGAR,DATA_CPG,DATA_CPV,NUM_CHEQ,DATA_NF
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   DO WHILE .T.
      PLQUA("11")
      eNUM_LANC   = 0
      eCLASSE_D   = 0
      eCLASSE_PRO = 0
      eCODIGO_FOR = 0   
      eDESCRICAO  = SPACE(50)   
      eDATA_PREV  = CTOD("")
      ePREVISTO   = 0
      eDATA_REAL  = CTOD("")   
      eREALIZADO  = 0
      eCOD_CONTA  = 0
      eNUM_CHEQ   = 0
      eNUM_DOCUME = SPACE(20)
      ePARCELAS   = SPACE(1)
      eNUM_PARC   = 1
      eDATA_NOTA  = CTOD("")
      eNOMINAL    = "S"
      eINTEGRA    = SPACE(10)
      eNUM_COMPRA = SPACE(10)
      SELE 3
      IF F_BLOQR()
         eNUM_LANC  = LANC_CPG
         eORDE_FORN = ORDE_FORN
         eORDE_CLAD = ORDE_CLAD
         eORDE_CLAS = ORDE_CLAS
         REPLACE LANC_CPG WITH eNUM_LANC + 1
         UNLOCK
      ENDIF
      IF eORDE_FORN
         SELE 2
         USE FORNECED INDEX TEMP_FOR,FORNECED
         GOTO TOP
      ENDIF
      IF eORDE_CLAD
         SELE 4
         USE CLASSE_D INDEX TEMP_CLD,CLASSE_D
         GOTO TOP
      ENDIF
      eNUM_ANTE = eNUM_LANC
      @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
      READ
      IF eNUM_LANC = 0
         SELE 3
         IF F_BLOQR()
            REPLACE LANC_CPG WITH eNUM_ANTE
            UNLOCK
         ENDIF
         CLOSE ALL
         RESTORE SCREEN FROM TELA04
         RETURN nil
      ENDIF
      SELE 1
      GOTO TOP
      SEEK eNUM_LANC
      IF FOUND()
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
         PMENSAG("Lanáamento J† Existe")
         PPARA()
         LOOP
      ENDIF
      SELE 4
      GOTO TOP
      IF .NOT. eORDE_CLAD
         eCLASSE_D = 0
         DO WHILE .T.
            @ 08,05 SAY "Cod. Despesa  : " GET eCLASSE_D PICT "@R 99999999"
            READ
            SET COLOR TO W/N,+W/R
            @ 08,00
            @ 08,05 SAY "Cod. Despesa  : "
            SAVE SCREEN TO PESQUISA
            SELE 4
            GOTO TOP
            IF eCLASSE_D <> 0
               SEEK eCLASSE_D
            ENDIF
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Descriá∆o da Despesa"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLASSE_D = CODIGO
            RESTORE SCREEN FROM PESQUISA
            @ 08,00
            @ 08,05 SAY "Cod. Despesa  : "
            @ 08,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ENDDO
      ELSE
         eCLASSE_DESC = SPACE(30)
         DO WHILE .T.
            @ 08,05 SAY "Cod. Despesa  : " GET eCLASSE_DESC PICT "@S30"
            READ
            eCLASSE_DESC = ALLTRIM(eCLASSE_DESC)
            SELE 4
            GOTO TOP
            IF eCLASSE_DESC <> SPACE(30)
               SEEK eCLASSE_DESC
            ENDIF
            SET COLOR TO W/N,+W/R
            @ 08,00
            @ 08,05 SAY "Cod. Despesa  : "
            SAVE SCREEN TO PESQUISA
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Descriá∆o da Despesa"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLASSE_D = CODIGO
            eCLASSE_DESC = DESCRICAO
            RESTORE SCREEN FROM PESQUISA
            @ 08,00
            @ 08,05 SAY "Cod. Despesa  : "
            @ 08,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ENDDO
      ENDIF
      SELE 5
      GOTO TOP
      IF .NOT. eORDE_CLAS
         eCLAS_PRO = 0
         eDESC_PRO = SPACE(30)
         DO WHILE .T.
            @ 09,05 SAY "Cod. Produto: " GET eCLAS_PRO PICT "@R 99999999"
            READ
            SET COLOR TO W/N,+W/R
            @ 09,00
            @ 09,05 SAY "Cod. Produto: "
            SAVE SCREEN TO PESQUISA
            SELE 5
            USE CLAS_PRO INDEX CLAS_PRO,DESC_PRO
            GOTO TOP
            IF eCLAS_PRO <> 0
               SEEK eCLAS_PRO
            ENDIF
            DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@R99999"
            PIC[3]="@S35"
            CAB[1]="C¢digo"
            CAB[2]="Tipo"
            CAB[3]="Produto"         && Era Empreendimento
            @ 8,19 TO 18,79 DOUBLE
            DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLAS_PRO = CODIGO
            eDESC_PRO = DESCRICAO
            RESTORE SCREEN FROM PESQUISA
            @ 09,00
            @ 09,05 SAY "Cod. Produto: "
            @ 09,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ENDDO
      ELSE
         eCLAS_PRO = 0
         eDESC_PRO = SPACE(30)
         DO WHILE .T.
            @ 09,05 SAY "Cod. Produto: " GET eDESC_PRO PICT "@S30"
            READ
            eDESC_PRO = ALLTRIM(eDESC_PRO)
            SELE 5
            USE CLAS_PRO INDEX DESC_PRO,CLAS_PRO
            GOTO TOP
            IF eDESC_PRO <> SPACE(30)
               SEEK eDESC_PRO
            ENDIF
            SET COLOR TO W/N,+W/R
            @ 09,00
            @ 09,05 SAY "Cod. Produto: "
            SAVE SCREEN TO PESQUISA
            DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@R99999"
            PIC[3]="@S35"
            CAB[1]="C¢digo"
            CAB[2]="Tipo"
            CAB[3]="Produto"
            @ 8,19 TO 18,79 DOUBLE
            DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLAS_PRO = CODIGO
            eDESC_PRO = DESCRICAO
            RESTORE SCREEN FROM PESQUISA
            @ 09,00
            @ 09,05 SAY "Cod. Produto: "
            @ 09,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ENDDO
      ENDIF
      eCODIGO_FOR  = 0
      eCODIGO_FORN = 0
      IF .NOT. eORDE_FORN
         DO WHILE .T.
            @ 10,05 SAY "Fornecedor    : " GET eCODIGO_FOR PICT "@R 9999999"
            READ
            SET COLOR TO W/N,+W/R
            @ 10,00
            @ 10,05 SAY "Fornecedor    : " 
            SAVE SCREEN TO PESQUISA
            SELE 2
            GOTO TOP
            IF eCODIGO_FOR <> 0
               SEEK eCODIGO_FOR
            ENDIF
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Fornecedor"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(08,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCODIGO_FORN = NOME
            eCODIGO_FOR  = CODIGO
            RESTORE SCREEN FROM PESQUISA
            @ 10,00
            @ 10,05 SAY "Fornecedor    : "
            @ 10,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(NOME)
            SELE 1
            EXIT
         ENDDO
      ELSE
         eNOME_FORN  = SPACE(30)
         eCODIGO_FOR = 0
         DO WHILE .T.
            @ 10,05 SAY "Fornecedor    : " GET eNOME_FORN
            READ
            SELE 2
            GOTO TOP
            eNOME_FORN = ALLTRIM(eNOME_FORN)
            IF eNOME_FORN <> SPACE(30)
               SEEK eNOME_FORN
            ENDIF
            SET COLOR TO W/N,+W/R
            @ 10,00
            @ 10,05 SAY "Fornecedor    : "
            SAVE SCREEN TO PESQUISA
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Fornecedor"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eNOME_FORN   = NOME
            eCODIGO_FOR  = CODIGO
            RESTORE SCREEN FROM PESQUISA
            @ 10,00
            @ 10,05 SAY "Fornecedor    : "
            @ 10,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(NOME)
            SELE 1
            EXIT
         ENDDO
      ENDIF
      eVEZES = 0
      DO WHILE .T.
         @ 11,05 SAY "N. de Parcelas: " GET eNUM_PARC PICT "@R 99"
         READ
         IF eNUM_PARC = 0
            LOOP
         ELSE
            IF eNUM_PARC = 1
               eVEZES = 1
               EXIT
            ELSE
               PLQUA("01")
               PPERG ( "Confirma a criacao de ("+ALLTRIM(STR(eNUM_PARC,5))+") parcelas (S/N) ? ")
               IF sisRESP = "N"
                  LOOP
               ELSE
                  eVEZES = eNUM_PARC
                  EXIT
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      eCONTADOR = 1
      DO WHILE eCONTADOR <= eVEZES
         @ 11,21 SAY ALLTRIM(STR(eCONTADOR,3))+"/"+ALLTRIM(STR(eVEZES,3))
         SELE 1
         DO WHILE .T.
            @ 12,05 SAY "Data Prevista : " GET eDATA_PREV
            READ
            IF eDATA_PREV = CTOD("")
               LOOP
            ELSE
               EXIT
            ENDIF
         ENDDO
         @ 13,05 SAY "Vlr. Previsto : " GET ePREVISTO  PICT "@R 999,999,999,999.99"
         @ 14,05 SAY "Num. Documento: " GET eNUM_DOCUME
         @ 14,45 SAY "Num. Compra   : " GET eNUM_COMPRA
         @ 15,05 SAY "Dt.Nota Fiscal: " GET eDATA_NOTA
         @ 16,05 SAY "Descriá∆o     : " GET eDESCRICAO
         @ 17,05 SAY "Data Pagamento: " GET eDATA_REAL
         @ 18,05 SAY "Vlr. Realizado: " GET eREALIZADO PICT "@R 999,999,999,999.99"
         @ 19,05 SAY "C¢digo Conta  : " GET eCOD_CONTA PICT "@R 999999999"
         @ 20,05 SAY "Num. Cheque   : " GET eNUM_CHEQ  PICT "@R 999999999"
         @ 21,05 SAY "Nominal (S/N) : " GET eNOMINAL   PICT "@! X" VALID eNOMINAL $("SN")
         @ 21,45 SAY "Int. Contabil : " GET eINTEGRA   
         READ
         eCONTADOR = eCONTADOR + 1
         SELE 1
         PLQUA("01")
         PPERG ( "Confirma (S/N) ? ")
         IF sisRESP = "S"
            PLQUA("01")
            PMENSAG("Gravando, Aguarde ...")
            SELE 1
            APPEND BLANK
            IF F_BLOQR()
               REPLACE LANCAMENTO WITH eNUM_LANC,;
                       CLASSE_D   WITH eCLASSE_D
               REPLACE EMPREENDIM WITH eCLAS_PRO,;
                       CODIGO_FOR WITH eCODIGO_FOR
               REPLACE DESCRICAO  WITH eDESCRICAO,;
                       DATA_PREV  WITH eDATA_PREV,;
                       PREVISTO   WITH ePREVISTO
               REPLACE DATA_REAL  WITH eDATA_REAL,;
                       REALIZADO  WITH eREALIZADO,;
                       COD_CONTA  WITH eCOD_CONTA
               REPLACE NUM_CHEQUE WITH eNUM_CHEQ,;
                       NUM_DOCUME WITH eNUM_DOCUME,;
                       CONTABIL   WITH .T.
               REPLACE DATA_NOTA  WITH eDATA_NOTA,;
                       NOMINAL    WITH eNOMINAL,;
                       INTEGRACAO WITH eINTEGRA
               REPLACE NUM_COMPRA WITH eNUM_COMPRA
               UNLOCK
            ENDIF
            IF eCONTADOR <= eVEZES
               SELE 3
               IF F_BLOQR()
                  eNUM_LANC  = LANC_CPG
                  eORDE_FORN = ORDE_FORN
                  eORDE_CLAD = ORDE_CLAD
                  REPLACE LANC_CPG WITH eNUM_LANC + 1
                  UNLOCK
                  @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
                  CLEAR GETS
               ENDIF
            ENDIF
            SELE 1
            F_LACESSO(sisUSUARIO,"CONTAS A PAGAR","INCLUSAO LANC. : "+ALLTRIM(STR(eNUM_LANC,5)))
         ENDIF
      ENDDO
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FCON_CTPG
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         RET = 0
   ENDCASE
ENDIF
RETURN RET
******************************************************************

