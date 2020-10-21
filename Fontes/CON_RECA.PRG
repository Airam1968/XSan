*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_RECA.PRG
* Data        : 01/01/99
* Objetivo    : Consulta do Resumo de Contas         
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION CON_RECA()
SAVE SCREEN TO TELA04
PTIT( "CONSULTA DE RESUMO DE CONTAS")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA04
   RETURN nil
ENDIF
SELE 1
SET EXCLUSIVE ON
USE RESUMOCC
GOTO TOP
IF F_BLOQA()
   DELE ALL
   PACK
ENDIF
SET EXCLUSIVE OFF
GOTO TOP
SELE 7
USE BANCOS INDEX BANCOS
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 5
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Data Resumo : " GET eDATA_INI
   READ
   IF eDATA_INI = CTOD("") 
      PLQUA("01")
      PMSGR( "Data Invalida !",3)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
SELE 2
GOTO TOP
eSUB_SALD = 0
eSUB_APLD = 0
eSUB_APLB = 0
eSUB_TOTG = 0
eTOT_TOTG = 0
DO WHILE .NOT. EOF()
   SELE 2
   eCONT  = STR(CODIGO,5)
   eZERO = "00"
   eARQ  = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
   eIND1 = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT)
   SELE 6
   USE &eARQ INDEX &eIND1
   GOTO TOP
   SEEK eDATA_INI
   IF FOUND()
      DO WHILE DATA = eDATA_INI .OR. .NOT. EOF()
         SKIP
      ENDDO
      SKIP -1
      eSUB_TOTG = 0
      eSUB_TOTG = (SALDO_F + APLICACAOD + APLICACAOB)
      eSUB_SALD = eSUB_SALD + SALDO_F 
      eSUB_APLD = eSUB_APLD + APLICACAOD
      eSUB_APLB = eSUB_APLB + APLICACAOB
      eTOT_TOTG = eTOT_TOTG + eSUB_TOTG
      eDATA       = DATA
      eCONTA      = CONTA
      eAPLICD     = APLICACAOD
      eAPLICB     = APLICACAOB
      eSALDO_F    = SALDO_F
      SELE 1
      APPEND BLANK
      IF F_BLOQR()
         REPLACE DATA       WITH eDATA,;
                 CONTA      WITH eCONTA,;
                 SALDOF     WITH eSALDO_F
         REPLACE APLICACAOD WITH eAPLICD,;
                 APLICACAOB WITH eAPLICB,;
                 TOTALGERAL WITH eSUB_TOTG
         UNLOCK
      ENDIF
      SELE 6
   ELSE
      SKIP -1
      eSUB_TOTG = 0
      eSUB_TOTG = (SALDO_F + APLICACAOD + APLICACAOB)
      eSUB_SALD = eSUB_SALD + SALDO_F
      eSUB_APLD = eSUB_APLD + APLICACAOD
      eSUB_APLB = eSUB_APLB + APLICACAOB
      eTOT_TOTG = eTOT_TOTG + eSUB_TOTG
      eDATA       = DATA
      eCONTA      = CONTA
      eAPLICD     = APLICACAOD
      eAPLICB     = APLICACAOB
      eSALDO_F    = SALDO_F
      SELE 1
      APPEND BLANK
      IF F_BLOQR()
         REPLACE DATA       WITH eDATA,;
                 CONTA      WITH eCONTA,;
                 SALDOF     WITH eSALDO_F
         REPLACE APLICACAOD WITH eAPLICD,;
                 APLICACAOB WITH eAPLICB,;
                 TOTALGERAL WITH eSUB_TOTG
         UNLOCK
      ENDIF
      SELE 6
   ENDIF
   SELE 2
   SKIP
ENDDO
* VISUALIZAá∆o DAS CONTAS
SELE 1
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@D"
PIC[2]="@R 999"
PIC[3]="@R 999,999,999.99"
PIC[4]="@R 999,999,999.99"
PIC[5]="@R 999,999,999.99"
PIC[6]="@R 999,999,999.99"
CAB[1]="Data"
CAB[2]="Conta"
CAB[3]="Saldo Final"
CAB[4]="Aplic.Desbl."
CAB[5]="Aplic.Bloq."
CAB[6]="Total Geral"
@ 6,01 TO 19,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ESC]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,08 SAY "Para Sair"
DBEDIT(7,02,19,78,CAMPO,"F_CAIXA",PIC,CAB,CHR(196),CHR(179),CHR(205))
@ 21,20  SAY eSUB_SALD  PICT "@R 9,999,999.99"
@ 21,35  SAY eSUB_APLD  PICT "@R 9,999,999.99"
@ 21,50  SAY eSUB_APLB  PICT "@R 9,999,999.99"
@ 21,65  SAY eTOT_TOTG  PICT "@R 9,999,999.99"
PPARA()
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_CAIXA
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
