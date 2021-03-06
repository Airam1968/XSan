*********************************************************************
* Sistema     : Gest�o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_DECO.PRG
* Data        : 01/01/99
* Objetivo    : Consulta dos Depositos Compensados
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_DECO()
SAVE SCREEN TO TELA05
PTIT( "CONSULTA DEPOSITOS COMPENSADOS")
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
         PMSGR( "Aten��o !",3)
         PMENSAG("Conta N�o Existe !")
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
SELE 1
USE CRECEBER INDEX DATA_CRE,CRECEBER
SET FILTER TO DATA_REAL >= eDATA_INI .AND. DATA_REAL <= eDATA_FIM .AND. REALIZADO <> 0 .AND. COD_CONTA = eCONTA .AND. DATA_COMPE <> CTOD("") 
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[11],PIC[11],CAB[11],LENS[11]
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
PIC[11]="  /  /    "
CAB[1] ="Lan�amento"
CAB[2] ="Classe Recei."
CAB[3] ="   Produto    "
CAB[4] ="Cliente"
CAB[5] ="Descri��o"
CAB[6] ="Data Prev."
CAB[7] ="Vlr. Previsto"
CAB[8] ="Data Pgto."
CAB[9] ="Vlr. Pago"
CAB[10]="Cod. Conta"
CAB[11]="Data Compensacao"
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
DBEDIT(7,01,20,78,CAMPO,"F_CONDEP",PIC,CAB,CHR(196),CHR(179),CHR(205))
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
FUNCTION F_CONDEP
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
               eCLASSE_R   = CLASSE_R  
               SELE 9
               USE CLASSE_R INDEX CLASSE_R
               GOTO TOP
               SEEK eCLASSE_R  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Classe de Receita",12)
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
			   PCENTRA( "   Produto	",12)
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
               eCOD_CLIE   = CODIGO_CLI
               SELE 9
               USE FORNECED INDEX FORNECED
               GOTO TOP
               SEEK eCOD_CLIE  
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Cliente",12)
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
         IF DB_COLUMN = 11
            SAVE SCREEN
            IF F_BLOQR()
               eDATA_COMPE2 = CTOD("")               
               IF DATA_COMPE = CTOD("")
                  eDATA_COMPE2 = DATA_REAL
               ENDIF
               SET CURSOR ON
               PLQUA("01")             
               @ 23,01 SAY "Data Compensa��o: " GET eDATA_COMPE2
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
