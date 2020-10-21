*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CREC.PRG
* Data        : 01/01/99
* Objetivo    : IMPRESSAO DA RELATORIO DE CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CREC()
PLQUA("01")
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO DE CONTAS A RECEBER")
   SELE 3
   USE CLASSE INDEX CLASSE
   GOTO TOP
   SELE 2
   USE CLIENTES INDEX CLIENTES
   GOTO TOP
   SELE 1
   USE CRECEBER INDEX DATA_CRE,CRECEBER
   GOTO TOP
   eSITUACAO = SPACE(1)
   PLQUA("01")
   @ 23,00 SAY "Relat¢rio de Previsäes\Real\Ambos (P\R\A) ? " GET eSITUACAO PICT "X" VALID eSITUACAO $("PRApra")
   READ
   DO WHILE .T.
      SET COLOR TO W/N,+W/R
      eDATA_INI = CTOD("")
      eDATA_FIM = CTOD("")
      PLQUA("01")
      @ 23,00 SAY "Informe o Per¡odo de " GET eDATA_INI
      @ 23,31 SAY "At‚ "                  GET eDATA_FIM
      READ
      IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
         LOOP
      ELSE
         IF eDATA_INI > eDATA_FIM
            PLQUA("01")
            PMSGR( "Data Inicial Maior que a Final",3
            LOOP
         ENDIF
      ENDIF
      SELE 1
      IF eSITUACAO = "P" .OR. eSITUACAO = "R"
         SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM .AND. TIPO = eSITUACAO
      ELSE
         SET FILTER TO DATA >= eDATA_INI .AND. DATA <= eDATA_FIM
      ENDIF
      GOTO TOP
      IF .NOT. EOF()
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "NÆo H  Lan‡amentos Neste Per¡odo",3
         PPARA()
         LOOP
      ENDIF
   ENDDO
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
   ENDIF        
   PLQUA("01")
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   eTOTAL_P  = 0
   eTOTAL_R  = 0
   eSUBTOT_P = 0
   eSUBTOT_R = 0
   DO WHILE .NOT. EOF()
     @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
     @ 01,120 SAY "Data : "+dtoc(DATE())
     @ 02,00  SAY "Sistema : "+sisTIT2
     @ 02,121 SAY "Relat: SCP002"
     @ 03,40  SAY sisLETRAE+"RELAT‡RIO DE CONTAS A RECEBER"+sisLETRAC
     @ 04,00  SAY "Per¡odo : "+DTOC(eDATA_INI)+" At‡ "+DTOC(eDATA_FIM)
     @ 04,80  SAY "Situa‡Æo : "
     @ 04,92  SAY IIF(eSITUACAO = "P","Previs‡o",IIF(eSITUACAO = "R","Real","Previs‡o + Real"))
     @ 05,00  SAY REPLICATE("=",135)
     @ 06,00  SAY "|"
     @ 06,02  SAY "Num.Lan‡."
     @ 06,12  SAY "Data"
     @ 06,22  SAY "Fornecedor"
     @ 06,75  SAY "Valor"
     @ 06,95  SAY "Situa‡Æo"
     @ 06,105 SAY "Tipo"
     @ 06,134 SAY "|"
     @ 07,00  SAY "|"
     @ 07,22  SAY "Descri‡Æo"
     @ 07,134 SAY "|"
     @ 08,00 SAY REPLICATE("=",135)
     LIN = 9
     DO WHILE .NOT. EOF() .AND. LIN <= 56
        eDATA_AUX = DATA
        DO WHILE .NOT. EOF() .AND. LIN <= 56 .AND. eDATA_AUX = DATA
           @ LIN,00  SAY "|"
           @ LIN,02  SAY LANCAMENTO PICT "9999999"
           @ LIN,12  SAY DATA
           eNUM_CLIE = CODIGO_CLI
           SELE 2
           SEEK eNUM_CLIE
           @ LIN,22  SAY ALLTRIM(NOME)
           SELE 1
           @ LIN,75  SAY VALOR      PICT "@R 999,999,999,999.99"
           IF SITUACAO = "P"
              eSUBTOT_P = eSUBTOT_P + VALOR
           ELSE
              eSUBTOT_R = eSUBTOT_R + VALOR
           ENDIF
           eCLASSE = SITUACAO
           SELE 3
           SEEK eCLASSE
           @ LIN,95  SAY ALLTRIM(DESCRICAO)
           SELE 1
           @ LIN,105 SAY TIPO PICT "99"
           @ LIN,134  SAY "|"
           LIN = LIN + 1
           @ LIN,00  SAY "|"
           @ LIN,22  SAY ALLTRIM(DESCRICAO)
           @ LIN,134  SAY "|"
           LIN = LIN + 1
           SKIP
           DO PTECLA
           IF sisINTERR
              SET DEVICE TO SCREEN
              CLOSE ALL
              RETURN nil
           ENDIF
        ENDDO
        IF DATA <> eDATA_AUX
           @ LIN,00 SAY REPLICATE("-",135)
           LIN = LIN + 1
           @ LIN,50  SAY "Total Previs‡o : "
           @ LIN,75  SAY eSUBTOT_P   PICT "@R 999,999,999,999.99"
           LIN = LIN + 1
           @ LIN,50  SAY "Total Real     : "
           @ LIN,75  SAY eSUBTOT_R   PICT "@R 999,999,999,999.99"
           LIN = LIN + 1
           @ LIN,50  SAY "Total Geral    : "
           @ LIN,75  SAY eSUBTOT_R+eSUBTOT_P PICT "@R 999,999,999,999.99"
           LIN = LIN + 2
           eTOTAL_P  = eTOTAL_P + eSUBTOT_P
           eTOTAL_R  = eTOTAL_R + eSUBTOT_R
           eSUBTOT_P = 0
           eSUBTOT_R = 0
        ENDIF
     ENDDO
     IF EOF()
        @ LIN,00 SAY REPLICATE("-",135)
        LIN = LIN + 1
        @ LIN,50  SAY "Total Previs‡o : "
        @ LIN,75  SAY eTOTAL_P   PICT "@R 999,999,999,999.99"
        LIN = LIN + 1
        @ LIN,50  SAY "Total Real     : "
        @ LIN,75  SAY eTOTAL_R   PICT "@R 999,999,999,999.99"
        LIN = LIN + 2
        @ LIN,50  SAY "Total Geral    : "
        @ LIN,75  SAY eTOTAL_R+eTOTAL_P PICT "@R 999,999,999,999.99"
     ENDIF
     @ 58,00 SAY REPLICATE("=",135)
     @ 59,104 SAY sisTIT3
     @ 01,00 SAY " "+sisLETRAN
  ENDDO
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************
