*********************************************************************
* Sistema     : Gest�o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CTP2.PRG
* Data        : 01/01/99
* Objetivo    : Consulta dos Lan�amentos do Contas a Pagar por Empreendimento
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CTP2()
SAVE SCREEN TO TELA04
PTIT( "CONSULTA CONTAS A PAGAR - POR PRODUTO")
SELE 3
USE STATUS
GOTO TOP
IF F_BLOQR()
   eORDE_FORN = ORDE_FORN
   eORDE_CLAD = ORDE_CLAD
   eORDE_CLAS = ORDE_CLAS
   UNLOCK
ENDIF
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ou (0) para Todas ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA = 0
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Aten��o !",3)
         PMENSAG("Conta N�o Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
PLQUA("01")
eOPCAO = SPACE(1)
@ 23,00 SAY "Deseja T�tulos em Aberto ou Liquidados (A/L) ? " GET eOPCAO PICT "@!X" VALID eOPCAO $("AL")
READ
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv�lidas !",3)
      LOOP
   ELSE
      IF eDATA_INI > eDATA_FIM
         PLQUA("01")
         PMSGR( "Data Inicio > Que Final !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
ENDDO
* Selecao de Empreendimento
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
IF .NOT. eORDE_CLAS
   eCLAS_PRO = 0
   eDESC_PRO = SPACE(30)
   DO WHILE .T.
      PLQUA("01")
      @ 23,00 SAY "   Produto     ou (0) para Escritorio: " GET eCLAS_PRO PICT "@R 9999999"
      READ
      SET COLOR TO W/N,+W/R
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      SAVE SCREEN TO PESQUISA
      SELE 2
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
      CAB[1]="C�digo"
      CAB[2]="Tipo"
      CAB[3]="   Produto    "
      @ 8,19 TO 18,79 DOUBLE
      DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eCLAS_PRO = CODIGO
      eDESC_PRO = DESCRICAO
      RESTORE SCREEN FROM PESQUISA
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
      SELE 1
      EXIT
   ENDDO
ELSE
   eCLAS_PRO = 0
   eDESC_PRO = SPACE(30)
   DO WHILE .T.
      PLQUA("01")
      @ 23,00 SAY "   Produto     ou (0) para Escritorio: " GET eCLAS_PRO PICT "@R 9999999"
      READ
      eDESC_PRO = ALLTRIM(eDESC_PRO)
      SELE 2
      USE CLAS_PRO INDEX DESC_PRO,CLAS_PRO
      GOTO TOP
      IF eDESC_PRO <> SPACE(30)
         SEEK eDESC_PRO
      ENDIF
      SET COLOR TO W/N,+W/R
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      SAVE SCREEN TO PESQUISA
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@S35"
      CAB[1]="C�digo"
      CAB[2]="Tipo"
      CAB[3]="   Produto    "
      @ 8,19 TO 18,79 DOUBLE
      DBEDIT(9,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eCLAS_PRO = CODIGO
      eDESC_PRO = DESCRICAO
      RESTORE SCREEN FROM PESQUISA
      PLQUA("01")
      @ 23,00 SAY "   Produto     : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(DESCRICAO)
      SELE 1
      EXIT
   ENDDO
ENDIF
SELE 2
USE

* Selecao do Fornecedor
IF eORDE_FORN
   SELE 4
   USE FORNECED
   INDEX ON NOME TO TEMP_FOR
   GOTO TOP
ELSE
   SELE 4
   USE FORNECED INDEX FORNECED
   GOTO TOP
ENDIF
eCODIGO_FOR  = 0
eCODIGO_FORN = 0
IF .NOT. eORDE_FORN
   DO WHILE .T.
      PLQUA("01")
      @ 23,05 SAY "Fornecedor    : " GET eCODIGO_FOR PICT "@R 9999999"
      READ
      IF eCODIGO_FOR = 0
         EXIT
      ENDIF
      SET COLOR TO W/N,+W/R
      @ 23,00
      @ 23,05 SAY "Fornecedor    : " 
      SAVE SCREEN TO PESQUISA
      SELE 4
      GOTO TOP
      IF eCODIGO_FOR <> 0
         SEEK eCODIGO_FOR
      ENDIF
      DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@S40"
      CAB[1]="C�digo"
      CAB[2]="Fornecedor"
      @ 7,19 TO 18,79 DOUBLE
      DBEDIT(08,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eCODIGO_FORN = NOME
      eCODIGO_FOR  = CODIGO
      RESTORE SCREEN FROM PESQUISA
      PLQUA("01")
      @ 23,00
      @ 23,05 SAY "Fornecedor    : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(NOME)
      EXIT
   ENDDO
ELSE
   eNOME_FORN  = SPACE(30)
   eCODIGO_FOR = 0
   DO WHILE .T.
      PLQUA("01")
      @ 23,05 SAY "Fornecedor    : " GET eNOME_FORN
      READ
      IF eNOME_FORN = SPACE(30)
         EXIT
      ENDIF
      SELE 4
      GOTO TOP
      eNOME_FORN = ALLTRIM(eNOME_FORN)
      IF eNOME_FORN <> SPACE(30)
         SEEK eNOME_FORN
      ENDIF
      SET COLOR TO W/N,+W/R
      @ 23,00
      @ 23,05 SAY "Fornecedor    : "
      SAVE SCREEN TO PESQUISA
      DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@S40"
      CAB[1]="C�digo"
      CAB[2]="Fornecedor"
      @ 7,19 TO 18,79 DOUBLE
      DBEDIT(8,20,18,78,CAMPO,"FCON_CTPG",PIC,CAB,CHR(196),CHR(179),CHR(205))
      eNOME_FORN   = NOME
      eCODIGO_FOR  = CODIGO
      RESTORE SCREEN FROM PESQUISA
      @ 23,00
      @ 23,05 SAY "Fornecedor    : "
      @ 23,21 SAY STR(CODIGO,5)+" - "+ALLTRIM(NOME)
      EXIT
   ENDDO
ENDIF
eFORNECED = CODIGO
SELE 4
USE
DO CASE
   CASE eOPCAO = "A"
      DO CASE
      CASE eCONTA = 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. REALIZADO = 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND.  CODIGO_FOR = eFORNECED .AND. REALIZADO = 0
            GOTO TOP
         ENDCASE
      CASE eCONTA <> 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. COD_CONTA = eCONTA .AND. REALIZADO = 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. CODIGO_FOR = eFORNECED .AND. COD_CONTA = eCONTA .AND. REALIZADO = 0
            GOTO TOP
         ENDCASE
      ENDCASE
   CASE eOPCAO = "L"
      DO CASE
      CASE eCONTA = 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. REALIZADO <> 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. CODIGO_FOR = eFORNECED .AND. REALIZADO <> 0
            GOTO TOP
         ENDCASE
      CASE eCONTA <> 0
         DO CASE
         CASE eFORNECED = 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. COD_CONTA = eCONTA .AND. REALIZADO <> 0
            GOTO TOP
         CASE eFORNECED <> 0
            SELE 1
            USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
            SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. EMPREENDIM = eCLAS_PRO .AND. CODIGO_FOR = eFORNECED .AND. COD_CONTA = eCONTA .AND. REALIZADO <> 0
            GOTO TOP
         ENDCASE
      ENDCASE
ENDCASE
SELE 1
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[17],PIC[17],CAB[17],LENS[17]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 99999999"
PIC[2] ="@R 99999999"
PIC[3] ="@R 99999999"
PIC[4] ="@R 99999999"
PIC[5] ="@S50"
PIC[6] ="@D"
PIC[7] ="@R 999,999,999,999.99"
PIC[8] ="@D"
PIC[9] ="@R 999,999,999,999.99"
PIC[10]="@R 999999"
PIC[11]="@R 9999999999"
PIC[12]="@D" 
PIC[13]="@S20"
PIC[14]="@D"
PIC[15]="@S3"
PIC[16]="@!X"
PIC[17]="9"
CAB[1] ="Lan�amento"
CAB[2] ="Classe Desp."
CAB[3] ="   Produto    "
CAB[4] ="Fornecedor"
CAB[5] ="Descri��o"
CAB[6] ="Data Prev."
CAB[7] ="Vlr. Previsto"
CAB[8] ="Data Pgto."
CAB[9] ="Vlr. Pago"
CAB[10]="Cod. Conta"
CAB[11]="Num. Cheque"
CAB[12]="Data Compe." 
CAB[13]="Num. Docum."
CAB[14]="Data NF"
CAB[15]=" "
CAB[16]="Nominal (S/N)"
CAB[17]="Grupo Cheque"
@ 6,00 TO 20,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ESC]"
@ 23,20 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,05 SAY "Para Alterar"
@ 23,26 SAY "Movimenta��o"
DBEDIT(7,01,20,78,CAMPO,"F_CONCTP",PIC,CAB,CHR(196),CHR(179),CHR(205))
eTOT_PREV = 0
eTOT_REAL = 0
eTOT_TITU = 0
GOTO TOP
DO WHILE .NOT. EOF()
   eTOT_PREV = eTOT_PREV + PREVISTO
   eTOT_REAL = eTOT_REAL + REALIZADO
   eTOT_TITU = eTOT_TITU + 1
   SKIP
ENDDO
PLIMPA()
@ 10,10 SAY "Total Previsto    : " GET eTOT_PREV PICT "@R 999,999,999.99"
@ 12,10 SAY "Total Realizado   : " GET eTOT_REAL PICT "@R 999,999,999.99"
@ 14,10 SAY "Total Titulos     : " GET eTOT_TITU PICT "@R 999,999,999.99"
PLQUA("01")
PMENSAG("Pressione Qualquer Tecla para Finalizar ...")
PPARA()
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
