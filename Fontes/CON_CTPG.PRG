*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CTPG.PRG
* Data        : 01/01/99
* Objetivo    : Consulta dos Lan‡amentos do Contas a Pagar 
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
function CON_CTPG()
SAVE SCREEN TO TELA04
PTIT( "CONSULTA CONTAS A PAGAR - GERAL")
SELE 3
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
*SELE 4
*USE FORNECED INDEX FORNECED
GOTO TOP
PLQUA("01")
eOPCAO = SPACE(1)
@ 23,00 SAY "Consulta pela Data (P)revista, (R)eal ou (N)ota Fiscal (P/R/N) ? " GET eOPCAO PICT "@!X" VALID eOPCAO $("PRN")
READ
PLQUA("01")
eCHEQUE = SPACE(1)
@ 23,00 SAY "Deseja Relat‡rio por ordem de (D)ata ou (C)heque ? " GET eCHEQUE PICT "@!X" VALID eCHEQUE $("DC")
READ
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
         PMSGR( "Aten‡Æo !",3)
         PMENSAG("Conta NÆo Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Periodo : " GET eDATA_INI
   @ 23,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv lidas !",3)
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
DO CASE
CASE eCHEQUE = "D"
   DO CASE
   CASE eOPCAO = "N"
      DO CASE
      CASE eCONTA = 0
         SELE 1
         USE CPAGAR INDEX DATA_NF,DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
         SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM
         GOTO TOP
      CASE eCONTA <> 0
         SELE 1
         USE CPAGAR INDEX DATA_NF,DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ
         SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. COD_CONTA = eCONTA
         GOTO TOP
      ENDCASE
   CASE eOPCAO = "P"
      DO CASE
      CASE eCONTA = 0
         SELE 1
         USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
         SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM
         GOTO TOP
      CASE eCONTA <> 0
         SELE 1
         USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG,NUM_CHEQ,DATA_NF
         SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. COD_CONTA = eCONTA
         GOTO TOP
      ENDCASE
   CASE eOPCAO = "R"
      DO CASE
      CASE eCONTA = 0
         SELE 1
         USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
         SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM
         GOTO TOP
      CASE eCONTA <> 0
         SELE 1
         USE CPAGAR INDEX DATA_CPG,CPAGAR,DATA_CPV,NUM_CHEQ,DATA_NF
         SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. COD_CONTA = eCONTA
         GOTO TOP
      ENDCASE
   ENDCASE
CASE eCHEQUE = "C"
   DO CASE
   CASE eOPCAO = "P"
      DO CASE
      CASE eCONTA = 0
         SELE 1
         USE CPAGAR INDEX NUM_CHEQ,DATA_CPV,CPAGAR,DATA_CPG,DATA_NF
         SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM
         GOTO TOP
      CASE eCONTA <> 0
         SELE 1
         USE CPAGAR INDEX NUM_CHEQ,DATA_CPV,CPAGAR,DATA_CPG,DATA_NF
         SET FILTER TO DATA_PREV >= eDATA_INI .AND. DATA_PREV <= eDATA_FIM .AND. COD_CONTA = eCONTA
         GOTO TOP
      ENDCASE
   CASE eOPCAO = "R"
      DO CASE
      CASE eCONTA = 0
         SELE 1
         USE CPAGAR INDEX NUM_CHEQ,DATA_CPG,CPAGAR,DATA_CPV,DATA_NF
         SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM
         GOTO TOP
      CASE eCONTA <> 0
         SELE 1
         USE CPAGAR INDEX NUM_CHEQ,DATA_CPG,CPAGAR,DATA_CPV,DATA_NF
         SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. COD_CONTA = eCONTA
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
CAB[1] ="Lan‡amento"
CAB[2] ="Classe Desp."
CAB[3] ="    Produto   "
CAB[4] ="Fornecedor"
CAB[5] ="Descri‡Æo"
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
@ 23,26 SAY "Movimenta‡Æo"
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
**************************** funcao *********************************
FUNCTION F_CONCTP
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN = 2
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCLASSE_D   = CLASSE_D  
               SELE 3
               USE CLASSE_D INDEX CLASSE_D
               GOTO TOP
               SEEK eCLASSE_D  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Classe de Despesa",12)
               @ 13,15 GET DESCRICAO
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF
         IF DB_COLUMN = 3
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCLASSE_PRO = EMPREENDIM
               SELE 9
               USE CLAS_PRO INDEX CLAS_PRO
               GOTO TOP
               SEEK eCLASSE_PRO
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "	Produto   ",12)
               @ 13,15 GET DESCRICAO
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF
         IF DB_COLUMN = 4
	    IF F_BLOQR()
               SAVE SCREEN TO T_PROP1
               eCOD_FORN   = CODIGO_FOR
               SELE 9
               USE FORNECED INDEX FORNECED
               GOTO TOP
               SEEK eCOD_FORN  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Fornecedor",12)
               @ 13,15 GET NOME
               CLEAR GETS
               SELE 9
               USE
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP1
	       UNLOCK
	    ENDIF
	 ENDIF
         IF DB_COLUMN >= 8
            SAVE SCREEN
            IF F_BLOQR()
               SET CURSOR ON
               FLD_NAME = CAMPO[DB_COLUMN]
               FLD_PIC  = PIC[DB_COLUMN]
               IF F_BLOQR()
                  @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
                  READ
                  UNLOCK
               ENDIF
               SET CURSOR OFF
            ENDIF
            RESTORE SCREEN
	 ENDIF
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
