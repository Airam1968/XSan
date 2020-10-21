*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CAD_CTRE.PRG
* Data        : 01/01/1999
* Objetivo    : Cadastro de Contas a Receber
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CAD_CTRE()
SAVE SCREEN TO TELA04
PTIT( "CADASTRO DE CONTAS A RECEBER")
SELE 5
USE CLAS_PRO INDEX CLAS_PRO,DESC_PRO
GOTO TOP
SELE 4
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 3
USE STATUS
GOTO TOP
SELE 2
USE CLIENTES INDEX CLIENTES
GOTO TOP
SELE 1
USE CRECEBER INDEX CRECEBER,DATA_CRE,DATA_CRV,DATA_NF
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   DO WHILE .T.
      PLQUA("11")
      SELE 1
      eNUM_LANC   = 0
      eCLASSE_R   = 0
      eCLASSE_PRO = 0
      eCODIGO_CLI = 0   
      eDESCRICAO  = SPACE(50)   
      eDATA_PREV  = CTOD("")
      ePREVISTO   = 0
      eDATA_REAL  = CTOD("")   
      eREALIZADO  = 0
      eCOD_CONTA  = 0
      eNUM_DOCUME = SPACE(20)
      eDATA_NOTA  = CTOD("")
      eINTEGRA    = INTEGRACAO
      eNUM_PARC   = 1
      SELE 3
      IF F_BLOQR()
         eNUM_LANC  = LANC_CRE
         eORDE_CLIE = ORDE_CLIE
         eORDE_CLAR = ORDE_CLAR
         eORDE_CLAS = ORDE_CLAS
         REPLACE LANC_CRE WITH eNUM_LANC + 1
         UNLOCK
      ENDIF
      IF eORDE_CLIE
         SELE 2
         USE CLIENTES
         INDEX ON NOME TO TEMP_CLI
         GOTO TOP
      ENDIF
      IF eORDE_CLAR
         SELE 4
         USE CLASSE_R
         INDEX ON DESCRICAO TO TEMP_CLR
         GOTO TOP
      ENDIF
      eNUM_ANTE = eNUM_LANC
      @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
      READ
      IF eNUM_LANC = 0
         SELE 3
         IF F_BLOQR()
            REPLACE LANC_CRE WITH eNUM_ANTE
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
      IF .NOT. eORDE_CLAR
         eCLASSE_R = 0
         DO WHILE .T.
            @ 08,05 SAY "Cod. Receita  : " GET eCLASSE_R PICT "@R 99999999"
            READ
            SET COLOR TO W/N,+W/R
            @ 08,00
            @ 08,05 SAY "Cod. Receita  : "
            SAVE SCREEN TO PESQUISA
            SELE 4
            GOTO TOP
            IF eCLASSE_R <> 0
               SEEK eCLASSE_R
            ENDIF
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Descriá∆o da Receita"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLASSE_R = CODIGO
            RESTORE SCREEN FROM PESQUISA
            @ 08,00
            @ 08,05 SAY "Cod. Receita  : "
            @ 08,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ENDDO
      ELSE
         eCLASSE_R    = 0
         eCLASSE_RECE = SPACE(30)
         DO WHILE .T.
            @ 08,05 SAY "Cod. Receita  : " GET eCLASSE_RECE PICT "@S30"
            READ
            eCLASSE_RECE = ALLTRIM(eCLASSE_RECE)
            SELE 4
            GOTO TOP
            IF eCLASSE_RECE <> SPACE(30)
               SEEK eCLASSE_RECE
            ENDIF
            SET COLOR TO W/N,+W/R
            @ 08,00
            @ 08,05 SAY "Cod. Receita  : "
            SAVE SCREEN TO PESQUISA
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Descriá∆o da Receita"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLASSE_R    = CODIGO
            eCLASSE_RECE = DESCRICAO
            RESTORE SCREEN FROM PESQUISA
            @ 08,00
            @ 08,05 SAY "Cod. Receita  : "
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
            @ 09,05 SAY "Cod.  Produto : " GET eCLAS_PRO PICT "@R 99999999"
            READ
            SET COLOR TO W/N,+W/R
            @ 09,00
            @ 09,05 SAY "Cod.  Produto : "
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
            CAB[3]="   Produto    "
            @ 8,19 TO 18,79 DOUBLE
            DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLAS_PRO = CODIGO
            eDESC_PRO = DESCRICAO
            RESTORE SCREEN FROM PESQUISA
            @ 09,00
            @ 09,05 SAY "Cod.  Produto : "
            @ 09,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ENDDO
      ELSE
         eCLAS_PRO = 0
         eDESC_PRO = SPACE(30)
         DO WHILE .T.
            @ 09,05 SAY "Cod. Produto : " GET eDESC_PRO PICT "@S30"
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
            @ 09,05 SAY "Cod.  Produto : "
            SAVE SCREEN TO PESQUISA
            DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@R99999"
            PIC[3]="@S35"
            CAB[1]="C¢digo"
            CAB[2]="Tipo"
            CAB[3]="   Produto    "
            @ 8,19 TO 18,79 DOUBLE
            DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eCLAS_PRO = CODIGO
            eDESC_PRO = DESCRICAO
            RESTORE SCREEN FROM PESQUISA
            @ 09,00
            @ 09,05 SAY "Cod.  Produto : "
            @ 09,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
            SELE 1
            EXIT
         ENDDO
      ENDIF

      IF .NOT. eORDE_CLIE
         eNOME_CLIE  = SPACE(30)
         eCODIGO_CLI = 0
         DO WHILE .T.
            @ 10,05 SAY "Cliente       : " GET eCODIGO_CLI PICT "@R 9999999"
            READ
            SET COLOR TO W/N,+W/R
            @ 10,00
            @ 10,05 SAY "Cliente       : "
            SAVE SCREEN TO PESQUISA
            SELE 2
            GOTO TOP
            IF eCODIGO_CLI <> 0
               SEEK eCODIGO_CLI
            ENDIF
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Fornecedor"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(08,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eNOME_CLIE  = NOME
            eCODIGO_CLI = CODIGO
            RESTORE SCREEN FROM PESQUISA
            @ 10,00
            @ 10,05 SAY "Cliente       : "
            @ 10,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(NOME)
            SELE 1
            EXIT
         ENDDO
      ELSE
         eNOME_CLIE  = SPACE(30)
         eCODIGO_CLI = 0
         DO WHILE .T.
            @ 10,05 SAY "Cliente       : " GET eNOME_CLIE 
            READ
            SELE 2
            GOTO TOP
            eNOME_CLIE = ALLTRIM(eNOME_CLIE)
            IF eNOME_CLIE <> SPACE(30)
               SEEK eNOME_CLIE
            ENDIF
            SET COLOR TO W/N,+W/R
            @ 10,00
            @ 10,05 SAY "Cliente       : " 
            SAVE SCREEN TO PESQUISA
            DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
            AFIELDS(CAMPO,.T.,LENS)
            PIC[1]="@R99999"
            PIC[2]="@S40"
            CAB[1]="C¢digo"
            CAB[2]="Fornecedor"
            @ 7,19 TO 18,79 DOUBLE
            DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
            eNOME_CLIE   = NOME
            eCODIGO_CLI  = CODIGO
            RESTORE SCREEN FROM PESQUISA
            @ 10,00
            @ 10,05 SAY "Cliente       : " 
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
         @ 14,05 SAY "Num.Documento : " GET eNUM_DOCUME
         @ 15,05 SAY "Dt.Nota Fiscal: " GET eDATA_NOTA
         @ 16,05 SAY "Descriá∆o     : " GET eDESCRICAO
         @ 17,05 SAY "Data Pagamento: " GET eDATA_REAL
         @ 18,05 SAY "Vlr. Realizado: " GET eREALIZADO PICT "@R 999,999,999,999.99"
         @ 19,05 SAY "C¢digo Conta  : " GET eCOD_CONTA PICT "@R 999999999"
         @ 20,05 SAY "Int.Contabil  : " GET eINTEGRA  
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
                       CLASSE_R   WITH eCLASSE_R
               REPLACE EMPREENDIM WITH eCLAS_PRO,;
                       CODIGO_CLI WITH eCODIGO_CLI
               REPLACE DESCRICAO  WITH eDESCRICAO,;
                       DATA_PREV  WITH eDATA_PREV,;
                       PREVISTO   WITH ePREVISTO
               REPLACE DATA_REAL  WITH eDATA_REAL,;
                       REALIZADO  WITH eREALIZADO,;
                       COD_CONTA  WITH eCOD_CONTA
               REPLACE NUM_DOCUME WITH eNUM_DOCUME,;
                       CONTABIL   WITH .T.,;
                       DATA_NOTA  WITH eDATA_NOTA
               REPLACE INTEGRACAO WITH eINTEGRA
               UNLOCK
            ENDIF
            IF eCONTADOR <= eVEZES
               SELE 3
               IF F_BLOQR()
                  eNUM_LANC  = LANC_CRE
                  eORDE_CLIE = ORDE_CLIE
                  eORDE_CLAR = ORDE_CLAR
                  eORDE_CLAS = ORDE_CLAS
                  REPLACE LANC_CRE WITH eNUM_LANC + 1
                  UNLOCK
                  @ 06,05 SAY "Lanáamento    : " GET eNUM_LANC PICT "@R 9999999999"
                  CLEAR GETS
               ENDIF
            ENDIF
            SELE 1            
            F_LACESSO(sisUSUARIO,"CONTAS A RECEBER","INCLUSAO LANC. : "+ALLTRIM(STR(eNUM_LANC,5)))
         ENDIF
      ENDDO
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
