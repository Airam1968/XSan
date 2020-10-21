*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CHCO.PRG
* Data        : 01/01/1999
* Objetivo    : Consulta dos cheques Compensados
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_CHCO()
SAVE SCREEN TO TELA05
PTIT( "CONSULTA CHEQUES COMPENSADOS")
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA <> 0
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
         PMENSAG("Conta N∆o Existe !")
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
      PMSGR( "Datas Inv†lidas !",3)
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
SELE 1
USE CPAGAR INDEX DATA_CPV,CPAGAR,DATA_CPG
SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE <> CTOD("") .AND. NUM_CHEQUE <> 0
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[12],PIC[12],CAB[12],LENS[12]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 99999999"
PIC[2] ="@R 99999999"
PIC[3] ="@R 99999999"
PIC[4] ="@R 99999999"
PIC[5] ="@S50"
PIC[6] ="  /  /    "
PIC[7] ="@R 999,999,999,999.99"
PIC[8] ="  /  /    "
PIC[9] ="@R 999,999,999,999.99"
PIC[10]="@R 999999"
PIC[11]="@R 9999999999"
PIC[12]="  /  /    "
CAB[1] ="Lanáamento"
CAB[2] ="Classe Desp."
CAB[3] ="   Produto    "
CAB[4] ="Fornecedor"
CAB[5] ="Descriá∆o"
CAB[6] ="Data Prev."
CAB[7] ="Vlr. Previsto"
CAB[8] ="Data Pgto."
CAB[9] ="Vlr. Pago"
CAB[10]="Cod. Conta"
CAB[11]="Num. Cheque"
CAB[12]="Data Compensacao"
@ 6,00 TO 20,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ESC]"
@ 23,20 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,05 SAY "Para Alterar"
@ 23,26 SAY "Movimentaá∆o"
DBEDIT(7,01,20,78,CAMPO,"F_CONCHE2",PIC,CAB,CHR(196),CHR(179),CHR(205))
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
RESTORE SCREEN FROM TELA05
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_CONCHE2
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
               SELE 9
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
	       PCENTRA( "Empreendimento",12)
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
         IF DB_COLUMN = 12
            SAVE SCREEN
            IF F_BLOQR()
               eDATA_COMPE2 = CTOD("")               
               IF DATA_COMPE = CTOD("")
                  eDATA_COMPE2 = DATA_REAL
               ENDIF
               SET CURSOR ON
               PLQUA("01")             
               @ 23,01 SAY "Data Compensaá∆o: " GET eDATA_COMPE2
               READ
               REPLACE DATA_COMPE WITH eDATA_COMPE2
               COMMIT
               PLQUA("01")
               SET CURSOR OFF
               UNLOCK
            ENDIF
            RESTORE SCREEN
	 ENDIF
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
