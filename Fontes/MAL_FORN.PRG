*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa	: 
* Data        : 01/01/99
* Objetivo    : PROGRAMA DE MALA DIRETA CARTAS FORNECEDORES
* PRG"s       :
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MAL_FORN()
PLQUA("01")
PPERG ( "Confirma ? (S/N)")
IF sisRESP="N"
   RETURN nil
ENDIF
PLQUA("01")
DO WHILE .T.
   eCOPIA = 0
   @ 23,00 SAY "Informe o Numero de Copias por Fornecedor : " GET eCOPIA PICT "99"
   READ
   IF eCOPIA <> 0
      EXIT
   ENDIF
ENDDO
PLQUA("01")
PPERG ( "Deseja Emitir as Cartas Tendo como Remetente a Empresa (S/N) ? ")
IF sisRESP = "S"
   eREMETENTE = .T.
ELSE 
   eREMETENTE = .F.
   DO WHILE .T.
      eNOME_REME = SPACE(40)
      @ 23,00 SAY "Remetente : " GET eNOME_REME
      READ
      IF eNOME_REME <> SPACE(40)
         EXIT
      ENDIF
   ENDDO
ENDIF
PLQUA("01")
PMENSAG("Aguarde, Preparando Nomes Selecionados ...")
SELE 3
USE STATUS
GOTO TOP
eCID_EMPRESA = CIDADE
SELE 2
USE CAR_PADR INDEX CAR_PADR
SET FILTER TO SELECAO = "X"
GOTO TOP
SELE 1
USE FORNECED INDEX FORNECED
SET FILTER TO SELECAO = "X"
GOTO TOP
IF EOF()
   PLQUA("01")
   PMENSAG("N∆o H† Fornecedors selecionados, Tecle [ENTER] para finalizar")
   PPARA()
ENDIF
PLQUA("01")
PMENSAG("Posicione o Papel na impressora, Tecle [ENTER] para comecar")
PPARA()
DO PIMPR
PLQUA("01")
PMENSAG("Aguarde ...")
SET DEVICE TO PRINT
eAUX = 1
SELE 1
GOTO TOP
DO WHILE .NOT. EOF()
   SET DEVICE TO PRINT
   eCONTA = 1
   DO WHILE eCONTA <= eCOPIA
      * Inicio da Impressao
      @ 05,50 SAY ALLTRIM(eCID_EMPRESA)+"," 
      @ 06,50 SAY pEXT+"."
      eTIT = SPACE(20)
      SELE 1
      IF SEXO = "M"
         eTIT = "Ilmo Sr."
      ELSE
         IF SEXO = "F"
            eTIT = "Ilma Sra."
         ELSE
            eTIT = "A"
         ENDIF
      ENDIF
      @ 12,05 SAY eTIT
      @ 14,05 SAY NOME
      @ 15,05 SAY END
      @ 16,05 SAY ALLTRIM(BAIRRO)+" - "+ALLTRIM(CIDADE)+" - "+ALLTRIM(ESTADO)
      IF CEP <> SPACE(8)
         @ 17,05 SAY "CEP "+ALLTRIM(CEP) PICT "@R XXXXXXXXX-XXX"
      ENDIF
      SELE 2
      @ 21,05 SAY LINHA_1
      @ 22,05 SAY LINHA_2
      @ 23,05 SAY LINHA_3
      @ 24,05 SAY LINHA_4
      @ 25,05 SAY LINHA_5
      @ 26,05 SAY LINHA_6
      @ 27,05 SAY LINHA_7
      @ 28,05 SAY LINHA_8
      @ 29,05 SAY LINHA_9
      @ 30,05 SAY LINHA_10
      @ 31,05 SAY LINHA_11
      @ 32,05 SAY LINHA_12
      @ 33,05 SAY LINHA_13
      @ 34,05 SAY LINHA_14
      @ 35,05 SAY LINHA_15
      @ 36,05 SAY LINHA_16
      @ 37,05 SAY LINHA_17
      @ 38,05 SAY LINHA_18
      @ 39,05 SAY LINHA_19
      IF eREMETENTE
		 PCENTRA( "---------------------------------------",43)
		 PCENTRA( sisTIT1,44)
		 PCENTRA( sisTIT1,44)
      ELSE
		 PCENTRA( "---------------------------------------",43)
		 PCENTRA( eNOME_REME,44)
		 PCENTRA( eNOME_REME,44)
      ENDIF
      @ 00,00 SAY " "
      eCONTA = eCONTA + 1
      SELE 1
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   SELE 1
   SKIP
ENDDO
SET DEVICE TO SCREEN
CLOSE DATABASES
RETURN nil
***********************  fim ***********************************
