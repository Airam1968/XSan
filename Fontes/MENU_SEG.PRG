*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_SEG.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO LOG DE SEGURANCA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_SEG()
dbcloseall()
SAVE SCREEN TO TELA03
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU DO LOG DE SEGURANCA")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 15,69
   @ 11,50 TO 15,69
   @ 12,51 PROMPT " 1    Consulta    " MESSAGE "                                           "
   @ 13,51 PROMPT " 2    Relatorio   " MESSAGE "                                           "
   @ 14,51 PROMPT " Esc     Fim      " MESSAGE " Fim de Opera‡Æo do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_09")
	   SAVE SCREEN TO TELA03
		   PTIT( "CONSULTA FITA LOG DE SEGURAN€A")
	   PLQUA("01")
	   PPERG ( "Confirma (S/N) ? ")
	   IF sisRESP = "S"
              PLQUA("01")
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
              eUSUARIO = SPACE(10)
              PLQUA("01")
              PPERG ( "Deseja Log Por Usu rio (S/N) ? ")
              IF sisRESP = "S"
                 PLQUA("01")
                 @ 23,00 SAY "Usu rio : " GET eUSUARIO
                 READ
                 SELE 1
                 USE LOG_USU 
                 GOTO TOP
                 SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM .AND. USUARIO = eUSUARIO
                 GOTO TOP
              ELSE
                 SELE 1
                 USE LOG_USU
                 GOTO TOP
                 SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM 
                 GOTO TOP
              ENDIF
	      SELE 1
	      PLIMPA()
	      SET COLOR TO W/N,+W/R
	      DECLARE CAMPO[8],PIC[8],CAB[8],LENS[8]
	      AFIELDS(CAMPO,.T.,LENS)
	      PIC[1] ="  /  /  "
	      PIC[2] ="@S10"
	      PIC[3] ="@S20"
	      PIC[4] ="@S30"
	      PIC[5] ="@S50"
	      CAB[1] ="DATA"
	      CAB[2] ="HORA"
	      CAB[3] ="USUµRIO"
	      CAB[4] ="MODULO"
	      CAB[5] ="ACAO EXECUTADA"
	      @ 5,00 TO 21,79 DOUBLE
	      PLQUA("01")
	      SET COLOR TO
	      SET COLOR TO W/N+*
	      @ 23,00 SAY "[ENTER]"
	      @ 23,21 SAY "[ESC]"
	      @ 23,37 SAY "[ESPA‡O]"
	      @ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
	      SET COLOR TO
	      SET COLOR TO W/N,+W/R
	      @ 23,07 SAY "Para Alterar"
	      @ 23,26 SAY "Para Sair"
	      @ 23,45 SAY "Para Incluir"
	      @ 23,65 SAY "Movimenta‡Æo"
	      DBEDIT(06,01,21,78,CAMPO,"F_CAD_USU",PIC,CAB,CHR(196),CHR(179),CHR(205))
	   ENDIF
	   CLOSE DATABASES
	   RESTORE SCREEN FROM TELA03
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA LOG DE SEGURANCA","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_09")
	   SAVE SCREEN TO TELA03
		   PTIT( "RELATORIO FITA LOG DE SEGURAN‡A")
	   PLQUA("01")
	   PPERG ( "Confirma (S/N) ? ")
	   IF sisRESP = "S"
              PLQUA("01")
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
              eUSUARIO = SPACE(10)
              PLQUA("01")
              PPERG ( "Deseja Log Por Usu‡rio (S/N) ? ")
              IF sisRESP = "S"
                 PLQUA("01")
                 @ 23,00 SAY "Usu‡rio : " GET eUSUARIO
                 READ
                 SELE 1
                 USE LOG_USU 
                 GOTO TOP
                 SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM .AND. USUARIO = eUSUARIO
                 GOTO TOP
              ELSE
                 SELE 1
                 USE LOG_USU
                 GOTO TOP
                 SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM 
                 GOTO TOP
              ENDIF
	      SELE 1
              GOTO TOP
              PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
              PPARA()
              DO PIMPR
              SET DEVICE TO PRINT
              DO WHILE .NOT. EOF()
                 @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
                 @ 01,115 SAY "Data : "+dtoc(DATE())
                 @ 02,00  SAY "Sistema : "+sisTIT2
                 @ 02,115 SAY "Relat: SAN004"
                 @ 03,10  SAY sisLETRAE+"FITA LOG DE SEGURANCA DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
                 @ 04,00  SAY "Usuario : "+IIF(eUSUARIO =SPACE(10),"TODOS",eUSUARIO)
                 @ 05,00  SAY REPLICATE("=",135)
                 @ 06,02  SAY "DATA"
                 @ 06,14  SAY "HORA"
                 @ 06,26  SAY "USUARIO"
                 @ 06,50  SAY "MODULO"
                 @ 06,82  SAY "ACAO" 
                 @ 07,00 SAY REPLICATE("=",135)
                 LIN = 8
                 DO WHILE .NOT. EOF() .AND. LIN <= 54
                    @ LIN,02  SAY DATA
                    @ LIN,14  SAY HORA
                    @ LIN,26  SAY USUARIO
                    @ LIN,50  SAY MODULO
                    @ LIN,82  SAY ACAO
                    LIN = LIN + 1
                    SKIP
                    DO PTECLA
                    IF sisINTERR
                       SET DEVICE TO SCREEN
                       CLOSE ALL
                       RETURN nil
                    ENDIF
                 ENDDO
                 @ 56,00 SAY REPLICATE("=",135)
                 @ 57,95  SAY sisTIT3
                 @ 01,00 SAY " "+sisLETRAN
              ENDDO
	   ENDIF
           SET DEVICE TO SCREEN
	   CLOSE DATABASES
	   RESTORE SCREEN FROM TELA03
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO LOG DE SEGURANCA","ACESSO NEGADO")
	ENDIF
     CASE LASTKEY() = 27
        dbcloseall()     
	CLOSE DATABASES
        RESTORE SCREEN FROM TELA03
	RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
